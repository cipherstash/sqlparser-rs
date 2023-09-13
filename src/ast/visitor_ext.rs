use std::ops::ControlFlow;

use std::{cell::RefCell, fmt::Debug, rc::Rc};

mod sealed {
    pub trait Sealed {}
}

use self::sealed::Sealed;

/// Marker type to indicate a type that is an immutable reference.
#[derive(Debug, Clone)]
pub struct ByRef;

/// Marker type to indicate a type that is a mutable reference.
#[derive(Debug, Clone)]
pub struct ByMutRef;

/// Trait so that Node, Field & Primitive can be generic over mutability.
///
/// This trait is *sealed*: the list of implementors below is total.
pub trait Mutability<'a>: Sealed
where
    Self: Debug,
{
    type Type<T: 'a + Debug>: Debug + 'a;
}

impl Sealed for ByRef {}
impl Sealed for ByMutRef {}

impl<'a> Mutability<'a> for ByRef {
    type Type<T: 'a + Debug> = &'a T;
}

impl<'a> Mutability<'a> for ByMutRef {
    type Type<T: 'a + Debug> = Rc<RefCell<&'a mut T>>;
}

/// A optionally mutable reference to an AST node that contains a value of a
/// primitive type.
#[derive(Debug, Clone)]
pub enum Primitive<'ast, M: Mutability<'ast>> {
    String(M::Type<String>),
    Char(M::Type<char>),
    Bool(M::Type<bool>),
    F32(M::Type<f32>),
    F64(M::Type<f64>),
    U8(M::Type<u8>),
    U16(M::Type<u16>),
    U32(M::Type<u32>),
    U64(M::Type<u64>),
    I8(M::Type<i8>),
    I16(M::Type<i16>),
    I32(M::Type<i32>),
    I64(M::Type<i64>),
    #[cfg(feature = "bigdecimal")]
    BigDecimal(M::Type<bigdecimal::BigDecimal>),
}

// Defines `pub enum Node<'ast> { .. }`  and `pub enum Field<'ast>{ .. }`
include!(concat!(env!("OUT_DIR"), "/ast/generated.rs"));

/// An optionally mutable reference to an element of a Vec node.
/// It also contains the index of the element.
#[derive(Debug, Clone)]
pub struct ListItem<'ast, M: Mutability<'ast>>(pub usize, pub Node<'ast, M>);

/// Used as a [`std::ops::ControlFlow`] value so that [`VisitorExt`] /
/// implementations can communicate navigation instructions to
/// a [`VisitExt`] impls.
pub enum VisitOption {
    /// Tells the visitor visit all fields
    AllFields,
    /// Tells the visitor to skip children of the current node
    SkipFields,
}

/// Trait for building Node instances from a concrete AST node.
///
/// Intended for use only inside VisitExt implementations.
///
/// This trait is derived when VisitExt is derived.
pub trait NodeBuilder<'ast> {
    fn wrap_node(&'ast self) -> Node<'ast, ByRef>;

    fn wrap_node_mut(&'ast mut self) -> Node<'ast, ByMutRef>;
}

/// Trait for building Field instances from fields of a concrete AST node.
///
/// Intended for use only inside VisitExt implementations.
pub trait FieldBuilder<'ast> {
    type NodeField<M: Mutability<'ast> + 'ast>;

    fn wrap_field<F>(field: F) -> Field<'ast, ByRef>
    where
        Self::NodeField<ByRef>: From<F>;

    fn wrap_field_mut<F>(field: F) -> Field<'ast, ByMutRef>
    where
        Self::NodeField<ByMutRef>: From<F>;
}

pub trait VisitExt {
    fn visit_ext<V: VisitorExt>(&self, visitor: &mut V) -> ControlFlow<(), VisitOption>;

    fn visit_ext_mut<V: VisitorExtMut>(&mut self, visitor: &mut V) -> ControlFlow<(), VisitOption>;
}

/// A generalised visitor trait.
///
/// Every node in sqlparser's grammar is visitable which makes it possible for
/// SQL analytical tools based on this trait to know unambiguously where a given
/// node resides the AST, which is not possible with the non-general
/// implementation - for example - the Expr type is used in many different
/// places in the AST - knowing whether an Expr node is within a projection or a
/// where clause is impossible with the old visitor trait.
///
/// Implementations can also exploit exhaustiveness checking (by matching against
/// the value provided in the callback) - which is of particular importance for
/// analytical tools.
///
/// NOTE: the sqlparser AST is frequently updated - if exhaustiveness checking
/// was mandatory then every change to the AST has the potential to break
/// downstream VisitorExt implementations.  Therefore
/// Node/Field/ListItem/Primitive are [`#[non_exhaustive]`] by default -
/// exhaustiveness can be enabled via a crate feature.
///
/// QUESTION: how should Nodes/ListItems/Fields be wholesale *replaced* or *deleted* ?
///
/// Deciding to replace a node may be dependent on visiting the children of the node?
///
/// Deletion would only be possible from within a Vec or Option node, right?
///
///  - option 1: indicate node replacement and deletion via the return value
///              (extend VisitOption) - the Visit implementation should handle it
///  - option 2: Child nodes can be deleted/replaced from the parent node (possible now)
///              Ergonomics could be awkward if decision to delete is based on children
///              of current node, however, nothing prevents spawning a new `visit_ext`
///              from inside a Visitor implementation to gather desired info before
///              deletion/replacement.
///              How would the root node be deleted/replaced? There would be no parent
///              to latch on to.
pub trait VisitorExt {
    /// Called before entering a node
    #[allow(unused_variables)]
    fn enter_node<'a>(&mut self, node: &'a Node<'_, ByRef>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a node
    #[allow(unused_variables)]
    fn leave_node<'a>(&mut self, node: &'a Node<'_, ByRef>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering a field of a node
    #[allow(unused_variables)]
    fn enter_field<'a>(&mut self, field: &'a Field<'_, ByRef>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a field of a node
    #[allow(unused_variables)]
    fn leave_field<'a>(&mut self, field: &'a Field<'_, ByRef>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn enter_list_item<'a>(
        &mut self,
        item: &'a ListItem<'_, ByRef>,
    ) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn leave_list_item<'a>(&mut self, item: &'a ListItem<'_, ByRef>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

pub trait VisitorExtMut {
    /// Called before entering a node
    #[allow(unused_variables)]
    fn enter_node<'a>(&mut self, node: &'a Node<'_, ByMutRef>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a node
    #[allow(unused_variables)]
    fn leave_node<'a>(&mut self, node: &'a Node<'_, ByMutRef>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering a field of a node
    #[allow(unused_variables)]
    fn enter_field<'a>(&mut self, field: &'a Field<'_, ByMutRef>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a field of a node
    #[allow(unused_variables)]
    fn leave_field<'a>(&mut self, field: &'a Field<'_, ByMutRef>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn enter_list_item<'a>(
        &mut self,
        item: &'a ListItem<'_, ByMutRef>,
    ) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn leave_list_item<'a>(&mut self, item: &'a ListItem<'_, ByMutRef>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

/// A visitor that performs no action in any trait methods.
///
/// All `enter_*` methods return [`ControlFlow::Continue(VisitOption::AllFields)`].
/// All `leave_*` methods return [`ControlFlow::Continue(())`].
pub struct NullVisitorExt;

impl VisitorExt for NullVisitorExt {}
impl VisitorExtMut for NullVisitorExt {}
