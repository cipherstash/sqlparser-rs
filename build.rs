#[cfg(feature = "visitor_ext")]
mod generator {
    use std::collections::HashMap;
    use std::env;
    use std::fs::{create_dir_all, File};
    use std::io::{prelude::*, Read};
    use std::path::{Path, PathBuf};

    use ident_case::RenameRule;
    use proc_macro2::{Span, TokenStream};
    use quote::{quote, TokenStreamExt};
    use syn::ext::IdentExt;
    use syn::Meta::{self, List};
    use syn::{
        punctuated::Punctuated, Attribute, Ident, Item, ItemEnum, ItemStruct, PathArguments,
        PathSegment,
    };
    use syn::{Fields, FieldsNamed, FieldsUnnamed, Token, Visibility};

    pub(crate) fn generate_node_and_field_meta() {
        let mut node_gen = NodeScanner::new();
        node_gen.process_pub_mod(&Ident::new("ast", Span::call_site()));
        node_gen.nodes.sort_by(|a, b| a.ident().cmp(&b.ident()));

        let node_variants =
            node_gen
                .nodes
                .clone()
                .into_iter()
                .fold(TokenStream::new(), |mut ts, node| {
                    let (path, ident) = node.fq_ident();
                    ts.append_all(quote! {
                        #ident ( M::Type<crate::#path::#ident>),
                    });
                    ts
                });

        let field_variants =
            node_gen
                .nodes
                .clone()
                .into_iter()
                .fold(TokenStream::new(), |mut ts, node| {
                    let (_, ident) = node.fq_ident();
                    let mod_ident = node.ident_to_module_name();
                    ts.append_all(quote! {
                        #ident ( self::meta::#mod_ident::Field<'ast, M> ),
                    });
                    ts
                });

        let meta: TokenStream = generate_node_meta(&node_gen.nodes);

        let generated: TokenStream = quote! {
            #[derive(Debug)]
            pub enum Node<'ast, M: crate::ast::visitor_ext::Mutability<'ast> + 'ast> {
                Primitive(Primitive<'ast, M>),
                #node_variants
            }

            #[derive(Debug)]
            pub enum Field<'ast, M: crate::ast::visitor_ext::Mutability<'ast> + 'ast> {
                #field_variants
            }

            #[allow(unused_imports)]
            pub mod meta {
                #meta
            }
        }
        .into();

        create_dir_all(&node_gen.dest_file.parent().unwrap()).expect(
            format!(
                "Could not create directory for {}",
                &node_gen.dest_file.display()
            )
            .as_str(),
        );

        let mut file = File::create(&node_gen.dest_file)
            .expect(format!("Could not open {}", &node_gen.dest_file.to_str().unwrap()).as_str());

        eprintln!("cargo:message={}", generated.to_owned());

        let formatted = prettyplease::unparse(&syn::parse_file(&generated.to_string()).unwrap());

        file.write_all(formatted.as_bytes())
            .expect(format!("Could not write to {}", &node_gen.dest_file.display()).as_str());

        println!("cargo:rerun-if-changed=build.rs");
        println!("cargo:rerun-if-changed=src/ast");
    }

    struct NodeScanner {
        base_dir: PathBuf,
        dest_file: PathBuf,
        mod_path: Vec<Ident>,
        nodes: Vec<NodeInfo>,
    }

    impl NodeScanner {
        fn new() -> Self {
            Self {
                base_dir: PathBuf::from(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("src"),
                dest_file: PathBuf::from(&env::var("OUT_DIR").unwrap()).join("ast/generated.rs"),
                mod_path: Vec::new(),
                nodes: Vec::new(),
            }
        }

        fn syn_path(&self) -> syn::Path {
            assert!(
                self.mod_path.len() != 0,
                "expected mod_path to have len > 0"
            );

            let syn_path = syn::Path {
                leading_colon: None,
                segments: Punctuated::new(),
            };

            self.mod_path
                .clone()
                .iter()
                .fold(syn_path, move |mut acc, ident| {
                    acc.segments.push(PathSegment {
                        ident: ident.clone(),
                        arguments: PathArguments::None,
                    });
                    acc
                })
        }

        // Processes a restricted module.
        // We assume that the types contained within are re-exported from the
        // current module, so we do not make target_mod part of the FQN of
        // identifiers of generated types.
        fn process_restricted_mod(&mut self, target_mod: &Ident) -> () {
            let syntax_tree = self.parse_mod(target_mod);
            for item in syntax_tree.items {
                self.process_item(&item);
            }
        }

        fn parse_mod(&self, target_mod: &Ident) -> syn::File {
            let mut mod_path = self.base_dir.clone();

            let mod_file_base: PathBuf = self
                .mod_path
                .iter()
                .map(|ident| ident.to_string())
                .collect::<PathBuf>()
                .into();

            mod_path.push(mod_file_base);
            mod_path.push(target_mod.to_string());

            let mut mod_file = if Path::is_dir(&mod_path) {
                mod_path.push(Path::new("mod.rs"));
                File::open(&mod_path)
            } else {
                mod_path.set_extension("rs");
                File::open(&mod_path)
            }
            .expect(&format!(
                "Could not open file for module: {}",
                mod_path.display()
            ));

            let mut file_content = String::new();

            mod_file.read_to_string(&mut file_content).expect(&format!(
                "Could read file for module: {}",
                mod_path.display()
            ));

            let syntax_tree = syn::parse_file(&file_content).expect(&format!(
                "Could not parse file for module: {}",
                mod_path.display()
            ));

            syntax_tree
        }

        // Extracts types from a publically exported mod - target_mod become part
        // of the FQN of generated types.
        fn process_pub_mod(&mut self, target_mod: &Ident) -> () {
            let syntax_tree = self.parse_mod(&target_mod);

            self.mod_path.push(target_mod.clone());

            for item in syntax_tree.items {
                self.process_item(&item);
            }

            self.mod_path.pop();
        }

        fn process_item(&mut self, item: &Item) -> () {
            match item {
                Item::Mod(mod_item) => {
                    match &mod_item.content {
                        // A inline module (with a body)
                        Some((_, items)) => {
                            self.mod_path.push(mod_item.ident.clone());

                            for item in items {
                                self.process_item(item);
                            }

                            self.mod_path.pop();
                        }
                        // An imported module.
                        None => {
                            if let Visibility::Public(_) = &mod_item.vis {
                                // If it's public, we track its path segment
                                // so we can correctly resolve types in it.
                                self.process_pub_mod(&mod_item.ident);
                            } else {
                                // If it's not public, we assume its types
                                // are re-exported as part of the module we are in.
                                self.process_restricted_mod(&mod_item.ident);
                            }
                        }
                    }
                }
                Item::Struct(ItemStruct {
                    ident,
                    attrs,
                    fields,
                    ..
                }) => {
                    if should_generate_node_meta(&attrs) {
                        self.nodes.push(NodeInfo::StructNode(StructInfo {
                            path: self.syn_path(),
                            ident: ident.clone(),
                            fields: fields.clone(),
                        }))
                    }
                }
                Item::Enum(ItemEnum {
                    ident,
                    attrs,
                    variants,
                    ..
                }) => {
                    if should_generate_node_meta(&attrs) {
                        self.nodes.push(NodeInfo::EnumNode(EnumInfo {
                            path: self.syn_path(),
                            ident: ident.clone(),
                            variants: variants
                                .into_iter()
                                .map(|v| (v.ident.clone(), v.fields.clone()))
                                .collect(),
                        }));
                    }
                }
                _ => {}
            }
        }
    }

    fn generate_node_meta(nodes: &Vec<NodeInfo>) -> TokenStream {
        let mut tokens = TokenStream::new();

        nodes.into_iter().fold(&mut tokens, |tokens, node| {
            let (node_path, node_ident) = node.fq_ident();
            let node_mod = node.ident_to_module_name();
            let fields_or_variants_meta: TokenStream = node.to_field_enum();

            let node_meta: TokenStream = quote! {
                #[allow(unused_imports)]
                pub mod #node_mod {
                    use crate::ast::*;
                    use crate::ast::helpers::stmt_create_table::*;
                    use crate::ast::helpers::stmt_data_loading::*;

                    use std::{cell::RefCell, rc::Rc};

                    #[automatically_derived]
                    impl<'ast> crate::ast::visitor_ext::FieldBuilder<'ast> for crate::#node_path::#node_ident {
                        type NodeField<M: crate::ast::visitor_ext::Mutability<'ast> + 'ast> = self::Field<'ast, M>;

                        fn wrap_field<F>(field: F) -> crate::ast::visitor_ext::Field<'ast, ByRef>
                        where
                            Self::NodeField<ByRef>: From<F>,
                        {
                            crate::ast::visitor_ext::Field::#node_ident(field.into())
                        }

                        fn wrap_field_mut<F>(field: F) -> crate::ast::visitor_ext::Field<'ast, ByMutRef>
                        where
                            Self::NodeField<ByMutRef>: From<F>,
                        {
                            crate::ast::visitor_ext::Field::#node_ident(field.into())
                        }
                    }

                    #[automatically_derived]
                    impl<'ast> crate::ast::visitor_ext::NodeBuilder<'ast> for crate::#node_path::#node_ident {
                        fn wrap_node(&'ast self) -> crate::ast::visitor_ext::Node<'ast, ByRef> {
                            crate::ast::visitor_ext::Node::#node_ident(self)
                        }

                        fn wrap_node_mut(&'ast mut self) -> crate::ast::visitor_ext::Node<'ast, ByMutRef> {
                            crate::ast::Node::#node_ident(Rc::new(RefCell::new(self)))
                        }
                    }

                    #[automatically_derived]
                    impl<'ast, M: crate::ast::visitor_ext::Mutability<'ast>> From<self::Field<'ast, M>> for crate::ast::visitor_ext::Field<'ast, M> {
                        fn from(field: Field<'ast, M>) -> Self {
                            crate::ast::visitor_ext::Field::#node_ident(field)
                        }
                    }

                    #[automatically_derived]
                    impl<'ast> From<&'ast crate::#node_path::#node_ident> for Rc<RefCell<&'ast crate::#node_path::#node_ident>> {
                        fn from(value: &'ast crate::#node_path::#node_ident) -> Self {
                            Rc::new(RefCell::new(value))
                        }
                    }

                    #[automatically_derived]
                    impl<'ast> From<&'ast mut crate::#node_path::#node_ident> for Rc<RefCell<&'ast mut crate::#node_path::#node_ident>> {
                        fn from(value: &'ast mut crate::#node_path::#node_ident) -> Self {
                            Rc::new(RefCell::new(value))
                        }
                    }

                    #fields_or_variants_meta
                }
            }
            .into();

            tokens.append_all(node_meta);
            tokens
        });

        tokens
    }

    fn should_generate_node_meta(attrs: &Vec<Attribute>) -> bool {
        attrs.iter().any(|attr| {
            if attr.path().is_ident("cfg_attr") {
                let nested = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated);
                match nested {
                    Ok(nested) => {
                        for meta in nested {
                            match meta {
                                List(list) => {
                                    let is_deriving_visitor_ext: bool = list
                                        .parse_args_with(
                                            Punctuated::<Ident, Token![,]>::parse_terminated,
                                        )
                                        .unwrap()
                                        .into_iter()
                                        .map(|ident| ident.to_string())
                                        .filter(|name| {
                                            name == "VisitorExt" || name == "VisitorExtMut"
                                        })
                                        .count()
                                        > 0;
                                    return is_deriving_visitor_ext;
                                }
                                _ => {}
                            }
                        }
                    }
                    Err(_) => {}
                }
            }
            false
        })
    }

    #[derive(Clone)]
    pub(crate) enum NodeInfo {
        StructNode(StructInfo),
        EnumNode(EnumInfo),
    }

    #[derive(Clone)]
    pub(crate) struct EnumInfo {
        pub(crate) path: syn::Path,
        pub(crate) ident: Ident,
        pub(crate) variants: HashMap<Ident, Fields>,
    }

    #[derive(Clone)]
    pub(crate) struct StructInfo {
        pub(crate) path: syn::Path,
        pub(crate) ident: Ident,
        pub(crate) fields: Fields,
    }

    impl NodeInfo {
        fn ident(&self) -> Ident {
            match self {
                NodeInfo::StructNode(StructInfo { ident, .. }) => ident.clone(),
                NodeInfo::EnumNode(EnumInfo { ident, .. }) => ident.clone(),
            }
        }

        fn fq_ident(&self) -> (syn::Path, Ident) {
            match self {
                NodeInfo::StructNode(StructInfo { path, ident, .. }) => {
                    (path.clone(), ident.clone())
                }
                NodeInfo::EnumNode(EnumInfo { path, ident, .. }) => (path.clone(), ident.clone()),
            }
        }

        fn ident_to_module_name(&self) -> Ident {
            let ident = match self {
                NodeInfo::StructNode(StructInfo { ref ident, .. }) => ident,
                NodeInfo::EnumNode(EnumInfo { ref ident, .. }) => ident,
            };

            Case::Snake.convert(ident)
        }

        fn to_field_enum(&self) -> TokenStream {
            match self {
                NodeInfo::StructNode(sn) => {
                    generate_enum_for_fields((sn.ident.clone(), sn.fields.clone()), false)
                }
                NodeInfo::EnumNode(en) => {
                    let children = en
                        .variants
                        .clone()
                        .into_iter()
                        .map(|variant| generate_enum_for_fields(variant, true))
                        .collect::<TokenStream>();

                    let parent_variants: TokenStream = en
                        .variants
                        .keys()
                        .into_iter()
                        .map(|ident| -> TokenStream {
                            let child_module: Ident = Case::Snake.convert(ident);

                            quote! {
                                #ident (M::Type<#child_module::Field<'ast, M>>),
                            }
                            .into()
                        })
                        .collect::<TokenStream>();

                    if en.variants.len() > 0 {
                        quote! {
                            #children

                            #[derive(Debug)]
                            pub enum Field<'ast, M: crate::ast::visitor_ext::Mutability<'ast>> {
                                #parent_variants
                            }
                        }
                    } else {
                        quote! {
                            #children

                            #[derive(Debug)]
                            pub struct Field<'ast, M: crate::ast::visitor_ext::Mutability<'ast> + 'ast>(&'ast std::marker::PhantomData<M>);
                        }
                    }
                }
            }
        }
    }

    fn generate_enum_for_fields(variant: (Ident, Fields), wrap_in_mod: bool) -> TokenStream {
        let field_count = match variant.1.clone() {
            Fields::Named(FieldsNamed { named, .. }) => named.len(),
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed.len(),
            Fields::Unit => 0,
        };

        let fields: TokenStream = match variant.1 {
            Fields::Named(FieldsNamed { named, .. }) => named
                .into_iter()
                .map(|f| {
                    let ident = f.ident.clone().unwrap();
                    let ident = Case::Pascal.convert(&ident);
                    let ty = f.ty.clone();
                    quote! {
                        #ident (M::Type<#ty>),
                    }
                })
                .collect::<TokenStream>(),
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed
                .into_iter()
                .enumerate()
                .map(|(idx, f)| {
                    let ident = format!("Field{}", idx);
                    let ident = Ident::new(&ident, Span::call_site());
                    let ty = f.ty.clone();
                    quote! {
                        #ident (M::Type<#ty>),
                    }
                })
                .collect::<TokenStream>(),
            Fields::Unit => quote! {}.into(),
        };

        let module_ident = Case::Snake.convert(&variant.0);

        let enum_def: TokenStream = if field_count > 0 {
            quote! {
                #[derive(Debug, Clone)]
                pub enum Field<'ast, M: crate::ast::visitor_ext::Mutability<'ast> + 'ast> {
                    #fields
                }
            }
            .into()
        } else {
            quote! {
                #[derive(Debug, Clone)]
                pub struct Field<'ast, M: crate::ast::visitor_ext::Mutability<'ast> + 'ast>(&'ast std::marker::PhantomData<M>);
            }.into()
        };

        if wrap_in_mod {
            quote! {
                #[allow(unused_imports)]
                pub mod #module_ident {
                    use ::sqlparser::tokenizer::Token;
                    use crate::ast::*;

                    #enum_def
                }
            }
            .into()
        } else {
            enum_def
        }
    }

    enum Case {
        Pascal,
        Snake,
    }

    impl Case {
        fn convert(&self, ident: &Ident) -> Ident {
            let ident = ident.unraw();
            let ident = ident.to_string();
            let ident = ident.trim_start_matches('_').trim_end_matches('_');

            let ident = match self {
                Case::Pascal => RenameRule::PascalCase.apply_to_field(&ident),
                Case::Snake => RenameRule::SnakeCase.apply_to_variant(&ident),
            };

            if Case::is_keyword(&ident) {
                Ident::new_raw(&format!("{}_", ident), Span::call_site())
            } else {
                Ident::new(&ident, Span::call_site())
            }
        }

        // Shamelessly lifted from `syn` (syn does not export it)
        fn is_keyword(ident: &str) -> bool {
            match ident {
                // Based on https://doc.rust-lang.org/1.65.0/reference/keywords.html
                "abstract" | "as" | "async" | "await" | "become" | "box" | "break" | "const"
                | "continue" | "crate" | "do" | "dyn" | "else" | "enum" | "extern" | "false"
                | "final" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "macro"
                | "match" | "mod" | "move" | "mut" | "override" | "priv" | "pub" | "ref"
                | "return" | "Self" | "self" | "static" | "struct" | "super" | "trait" | "true"
                | "try" | "type" | "typeof" | "unsafe" | "unsized" | "use" | "virtual"
                | "where" | "while" | "yield" => true,
                _ => false,
            }
        }
    }
}

#[cfg(feature = "visitor_ext")]
fn main() {
    generator::generate_node_and_field_meta()
}

#[cfg(not(feature = "visitor_ext"))]
fn main() {
    // no-op
}
