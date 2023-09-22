use std::ops::ControlFlow;

use std::fmt::Debug;

#[derive(Debug)]
pub enum Primitive<'ast> {
    String(&'ast String),
    Char(&'ast char),
    Bool(&'ast bool),
    F32(&'ast f32),
    F64(&'ast f64),
    U8(&'ast u8),
    U16(&'ast u16),
    U32(&'ast u32),
    U64(&'ast u64),
    I8(&'ast i8),
    I16(&'ast i16),
    I32(&'ast i32),
    I64(&'ast i64),
    #[cfg(feature = "bigdecimal")]
    BigDecimal(&'ast bigdecimal::BigDecimal),
}

#[derive(Debug)]
pub enum PrimitiveMut<'ast> {
    String(&'ast mut String),
    Char(&'ast mut char),
    Bool(&'ast mut bool),
    F32(&'ast mut f32),
    F64(&'ast mut f64),
    U8(&'ast mut u8),
    U16(&'ast mut u16),
    U32(&'ast mut u32),
    U64(&'ast mut u64),
    I8(&'ast mut i8),
    I16(&'ast mut i16),
    I32(&'ast mut i32),
    I64(&'ast mut i64),
    #[cfg(feature = "bigdecimal")]
    BigDecimal(&'ast mut bigdecimal::BigDecimal),
}

// Defines `pub enum Node<'ast> { .. }`  and `pub enum Field<'ast>{ .. }`
include!(concat!(env!("OUT_DIR"), "/ast/generated.rs"));

#[derive(Debug)]
pub struct ListItem(pub usize);

/// Used as a [`std::ops::ControlFlow`] value so that [`VisitorExt`] /
/// implementations can communicate navigation instructions to
/// a [`VisitExt`] impls.
pub enum VisitOption {
    /// Tells the visitor visit all fields
    AllFields,
    /// Tells the visitor to skip children of the current node
    SkipFields,
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
    fn enter_node<'a>(&mut self, node: &'a Node<'_>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a node
    #[allow(unused_variables)]
    fn leave_node<'a>(&mut self, node: &'a Node<'_>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering a field of a node
    #[allow(unused_variables)]
    fn enter_field<'a>(&mut self, field: &'a Field<'_>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a field of a node
    #[allow(unused_variables)]
    fn leave_field<'a>(&mut self, field: &'a Field<'_>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn enter_list_item<'a>(
        &mut self,
        item: &'a ListItem,
    ) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn leave_list_item<'a>(&mut self, item: &'a ListItem) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

pub trait VisitorExtMut {
    /// Called before entering a node
    #[allow(unused_variables)]
    fn enter_node<'a>(&mut self, node: &'a NodeMut<'_>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a node
    #[allow(unused_variables)]
    fn leave_node<'a>(&mut self, node: &'a NodeMut<'_>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering a field of a node
    #[allow(unused_variables)]
    fn enter_field<'a>(&mut self, field: &'a FieldMut<'_>) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving a field of a node
    #[allow(unused_variables)]
    fn leave_field<'a>(&mut self, field: &'a FieldMut<'_>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    /// Called before entering an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn enter_list_item<'a>(
        &mut self,
        item: &'a ListItem,
    ) -> ControlFlow<(), VisitOption> {
        ControlFlow::Continue(VisitOption::AllFields)
    }

    /// Called after leaving an element of a sequence (e.g. a Vec).
    ///
    /// The index of the element is provided along with the node at that index.
    #[allow(unused_variables)]
    fn leave_list_item<'a>(&mut self, item: &'a ListItem) -> ControlFlow<()> {
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

impl<T: VisitExt> VisitExt for Option<T> {
    fn visit_ext<V: VisitorExt>(&self, visitor: &mut V) -> ControlFlow<(), VisitOption> {
        match self {
            None => ControlFlow::Continue(VisitOption::AllFields),
            Some(t) => t.visit_ext(visitor),
        }
    }

    fn visit_ext_mut<V: VisitorExtMut>(
        &mut self,
        visitor: &mut V,
    ) -> ControlFlow<(), VisitOption> {
        match self {
            None => ControlFlow::Continue(VisitOption::AllFields),
            Some(t) => t.visit_ext_mut(visitor),
        }
    }
}

impl<'ast, T: VisitExt> VisitExt for Vec<T> {
    fn visit_ext<V: VisitorExt>(&self, visitor: &mut V) -> ControlFlow<(), VisitOption> {
        for (idx, item) in self.into_iter().enumerate() {
            visitor.enter_list_item(&ListItem(idx))?;
            item.visit_ext(visitor)?;
            visitor.leave_list_item(&ListItem(idx))?;
        }
        ControlFlow::Continue(VisitOption::AllFields)
    }

    fn visit_ext_mut<V: VisitorExtMut>(
        &mut self,
        visitor: &mut V,
    ) -> ControlFlow<(), VisitOption> {
        for (idx, item) in self.into_iter().enumerate() {
            visitor.enter_list_item(&ListItem(idx))?;
            item.visit_ext_mut(visitor)?;
            visitor.leave_list_item(&ListItem(idx))?;
        }
        ControlFlow::Continue(VisitOption::AllFields)
    }
}

impl<T: VisitExt> VisitExt for Box<T> {
    fn visit_ext<V: VisitorExt>(&self, visitor: &mut V) -> ControlFlow<(), VisitOption> {
        (**self).visit_ext(visitor)?;
        ControlFlow::Continue(VisitOption::AllFields)
    }

    fn visit_ext_mut<V: VisitorExtMut>(
        &mut self,
        visitor: &mut V,
    ) -> ControlFlow<(), VisitOption> {
        (**self).visit_ext_mut(visitor)?;
        ControlFlow::Continue(VisitOption::AllFields)
    }
}

macro_rules! primitive_nodes {
    ($(($t:ty, $id:ident)),+) => {
        $(
            #[automatically_derived]
            impl VisitExt for $t {
                fn visit_ext<V: VisitorExt>(&self, _: &mut V) -> ControlFlow<(), VisitOption> {
                    ControlFlow::Continue(VisitOption::AllFields)
                }

                fn visit_ext_mut<V: VisitorExtMut>(&mut self, _: &mut V) -> ControlFlow<(), VisitOption> {
                    ControlFlow::Continue(VisitOption::AllFields)
                }
            }

            #[automatically_derived]
            impl<'ast> From<&'ast $t> for Node<'ast> {
                fn from(value: &'ast $t) -> Self {
                    Node::Primitive(Primitive::$id(value))
                }
            }

            #[automatically_derived]
            impl<'ast> From<&'ast mut $t> for NodeMut<'ast> {
                fn from(value: &'ast mut $t) -> Self {
                    NodeMut::Primitive(PrimitiveMut::$id(value))
                }
            }
        )+
    };
}

primitive_nodes!(
    (u8, U8),
    (u16, U16),
    (u32, U32),
    (u64, U64),
    (i8, I8),
    (i16, I16),
    (i32, I32),
    (i64, I64),
    (char, Char),
    (bool, Bool),
    (String, String)
);

#[cfg(feature = "bigdecimal")]
primitive_nodes!((bigdecimal::BigDecimal, BigDecimal));