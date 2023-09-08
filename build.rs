#[cfg(feature = "visitor_ext")]
mod generate_ast_enums {
    use std::collections::HashMap;
    use std::env;
    use std::fs::{create_dir_all, File};
    use std::io::{prelude::*, Read};
    use std::path::Path;

    use ident_case::RenameRule;
    use proc_macro2::{Span, TokenStream};
    use quote::{quote, TokenStreamExt};
    use syn::Meta::{self, List};
    use syn::{
        punctuated::Punctuated, Attribute, Ident, Item, ItemEnum, ItemStruct, PathArguments,
        PathSegment,
    };
    use syn::{Fields, Token, FieldsNamed, FieldsUnnamed};
    use walkdir::WalkDir;

    pub(crate) fn do_it() {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let pkg_dir = Path::new(&manifest_dir);
        let ast_dir = Path::new(&pkg_dir).join("src/ast");
        let out_dir = env::var_os("OUT_DIR").unwrap();
        let dest_dir = Path::new(&out_dir).join(Path::new("ast/generated"));
        let dest_path = Path::new(&dest_dir).join(Path::new("node.rs"));

        let mut used = TokenStream::new();

        let mut nodes = Vec::<NodeInfo>::new();

        for entry in WalkDir::new(&ast_dir).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.is_file() && path.extension().map_or(false, |ext| ext == "rs") {
                let mut file = File::open(path)
                    .expect(format!("Could not open file: {}", path.to_str().unwrap()).as_str());

                let mut file_content = String::new();

                file.read_to_string(&mut file_content)
                    .expect(format!("Could not read file: {}", path.to_str().unwrap()).as_str());

                let syntax_tree = syn::parse_file(&file_content)
                    .expect(format!("Failed to parse file: {}", path.to_str().unwrap()).as_str());

                let mut root_path = syn::Path {
                    leading_colon: None,
                    segments: Punctuated::new(),
                };

                root_path.segments.push(PathSegment {
                    ident: Ident::new("ast", Span::mixed_site()),
                    arguments: PathArguments::None,
                });

                for item in syntax_tree.items {
                    inspect_item(&item, &root_path, &mut used, &mut nodes);
                }
            }
        }

        nodes.sort_by(|a,b| { a.ident().cmp(&b.ident()) });

        let node_variants = nodes
            .clone()
            .into_iter()
            .fold(TokenStream::new(), |mut ts, node| {
                let (path, ident) = node.full_qualified_ident();
                ts.append_all(quote! {
                    #ident ( M::Type<'ast, crate::#path::#ident> ),
                });
                ts
            });

        let field_variants = nodes
            .clone()
            .into_iter()
            .fold(TokenStream::new(), |mut ts, node| {
                let (path, ident) = node.full_qualified_ident();
                let mod_ident = node.ident_to_module_name();
                ts.append_all(quote! {
                    #ident ( crate::#path::#mod_ident::Field<'ast, M> ),
                });
                ts
            });

        let meta: TokenStream = generate_node_meta(&nodes);

        let generated: TokenStream = quote! {
            #[derive(Debug, Clone)]
            pub enum Node<'ast, M: Mutability> {
                Primitive(Primitive<'ast, M>),
                #node_variants
            }

            #[derive(Debug, Clone)]
            pub enum Field<'ast, M: Mutability> {
                #field_variants
            }

            pub mod meta {
                #meta
            }
        }
        .into();

        create_dir_all(&dest_dir)
            .expect(format!("Could not create directory {}", &dest_dir.to_str().unwrap()).as_str());

        let mut file = File::create(&dest_path)
            .expect(format!("Could not open {}", &dest_path.to_str().unwrap()).as_str());

        eprintln!("cargo:message={}", generated.to_owned());

        let formatted = prettyplease::unparse(&syn::parse_file(&generated.to_string()).unwrap());

        file.write_all(formatted.as_bytes())
            .expect(format!("Could not write to {}", &dest_path.to_str().unwrap()).as_str());

        println!("cargo:rerun-if-changed=build.rs");
        println!("cargo:rerun-if-changed=src/ast");
    }

    fn generate_node_meta(nodes: &Vec<NodeInfo>) -> TokenStream {
        let mut tokens = TokenStream::new();

        nodes.into_iter().fold(&mut tokens, |tokens, node| {
            let node_ident = node.ident();
            let node_mod = node.ident_to_module_name();
            let fields_or_variants_meta: TokenStream = generate_fields_or_variant_meta(&node);

            let node_meta: TokenStream = quote! {
                pub mod #node_mod {
                    use std::{cell::RefCell, rc::Rc};

                    use crate::{
                        mutability::{ByMutRef, ByRef, Mutability},
                        NodeBuilder, Parent, FieldBuilder,
                    };

                    #[automatically_derived]
                    impl FieldBuilder for #node_ident {
                        type NodeField<'ast, M: Mutability> = Field<'ast, M>;

                        fn wrap_field<'ast, F>(field: F) -> crate::Field<'ast, ByRef>
                        where
                            Self::NodeField<'ast, ByRef>: From<F>,
                        {
                            crate::Field::#node_ident(field.into())
                        }

                        fn wrap_field_mut<'ast, F>(field: F) -> crate::Field<'ast, ByMutRef>
                        where
                            Self::NodeField<'ast, ByMutRef>: From<F>,
                        {
                            crate::Field::#node_ident(field.into())
                        }
                    }

                    #[automatically_derived]
                    impl NodeBuilder for #node_ident {
                        fn wrap_node<'ast>(&'ast self) -> crate::Node<'ast, ByRef> {
                            crate::Node::#node_ident(self)
                        }

                        fn wrap_node_mut<'ast>(&'ast mut self) -> crate::Node<'ast, ByMutRef> {
                            crate::Node::#node_ident(Rc::new(RefCell::new(self)))
                        }
                    }

                    #[automatically_derived]
                    impl<'ast, M: Mutability> From<Field<'ast, M>> for crate::Field<'ast, M> {
                        fn from(field: Field<'ast, M>) -> Self {
                            crate::Field::#node_ident(field)
                        }
                    }

                    #[automatically_derived]
                    impl<'ast> From<&'ast #node_ident> for Rc<RefCell<&'ast #node_ident>> {
                        fn from(value: &'ast #node_ident) -> Self {
                            Rc::new(RefCell::new(value))
                        }
                    }

                    #[automatically_derived]
                    impl<'ast> From<&'ast mut #node_ident> for Rc<RefCell<&'ast mut #node_ident>> {
                        fn from(value: &'ast mut #node_ident) -> Self {
                            Rc::new(RefCell::new(value))
                        }
                    }

                    #fields_or_variants_meta
                }
            }.into();

            tokens.append_all(node_meta);
            tokens
        });

        tokens
    }

    fn generate_fields_or_variant_meta(node: &NodeInfo) -> TokenStream {
        match node {
            NodeInfo::StructNode(StructInfo { fields, .. }) => {
                fields.to_field_enum()
            },
            NodeInfo::EnumNode(EnumInfo { variants, .. }) => {
                variants.into_iter().map(|(ident, fields)| {
                    let variant_module_name = RenameRule::SnakeCase.apply_to_variant(ident.to_string());
                    let variant_module_name = Ident::new(&variant_module_name, Span::call_site());
                    let fields_enum: TokenStream = fields.to_field_enum();
                    quote! {
                        #[derive(Debug, Clone)]
                        pub mod #variant_module_name {
                            #fields_enum
                        }

                        #[automatically_derived]
                        impl<'ast, M: Mutability> From<Field<'ast, M>> for super::Field<'ast, M> {
                            fn from(value: Field<'ast, M>) -> Self {
                                super::Field::X(value)
                            }
                        }
                    }
                }).collect()
            },
        }.into()
    }

    fn inspect_item(
        item: &Item,
        current_path: &syn::Path,
        used: &mut TokenStream,
        nodes: &mut Vec<NodeInfo>,
    ) {
        match item {
            Item::Mod(mod_item) => {
                eprintln!(
                    "cargo:message=INSPECTING MODULE: {:?}",
                    mod_item.ident.to_string()
                );
                let mut mod_path = current_path.clone();
                mod_path.segments.push(PathSegment {
                    ident: mod_item.ident.clone(),
                    arguments: PathArguments::None,
                });
                if let Some((_, items)) = &mod_item.content {
                    for item in items {
                        inspect_item(item, &mod_path, used, nodes);
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
                    let type_path = current_path.clone();
                    nodes.push(NodeInfo::StructNode(StructInfo {
                        path: type_path,
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
                    let type_path = current_path.clone();

                    nodes.push(NodeInfo::EnumNode(EnumInfo {
                        path: type_path,
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

    impl NodeInfo {
        pub(crate) fn ident(&self) -> Ident {
            match self {
                NodeInfo::StructNode(StructInfo { ident, .. }) => ident.clone(),
                NodeInfo::EnumNode(EnumInfo { ident, .. }) => ident.clone(),
            }
        }

        pub(crate) fn full_qualified_ident(&self) -> (syn::Path, Ident) {
            match self {
                NodeInfo::StructNode(StructInfo { path, ident, .. }) => (path.clone(), ident.clone()),
                NodeInfo::EnumNode(EnumInfo { path, ident, .. }) => (path.clone(), ident.clone()),
            }
        }

        pub(crate) fn ident_to_module_name(&self) -> Ident {
            let ident = match self {
                NodeInfo::StructNode(StructInfo { ref ident, .. }) => ident,
                NodeInfo::EnumNode(EnumInfo { ref ident, .. }) => ident,
            };

            let renamed = RenameRule::SnakeCase.apply_to_variant(ident.to_string());
            let renamed = Ident::new(&renamed, ident.span());
            renamed
        }
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

    trait ToFieldsEnum {
        fn to_field_enum(&self) -> TokenStream;
    }

    impl ToFieldsEnum for Fields {
        fn to_field_enum(&self) -> TokenStream {
            let the_enum: TokenStream = match self {
                Fields::Named(FieldsNamed { named, .. }) => {
                    let fields: TokenStream = named.into_iter().map(|f| {
                        let ident = RenameRule::CamelCase.apply_to_field(f.ident.clone().unwrap().to_string());

                        // FIXME: detect when using r# escaped identifiers and handle them generally.
                        let ident = if &ident == "in" {
                            "r#in"
                        } else {
                            &ident
                        };

                        let ident = Ident::new(&ident, Span::call_site());
                        let ty = f.ty.clone();
                        quote! {
                            #ident(M::Type<'ast, #ty>),
                        }
                    }).collect();

                    quote! {
                        #[derive(Debug, Clone)]
                        pub enum Field<'ast, M: Mutability> {
                            #fields
                        }
                    }
                },
                Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                    let fields: TokenStream = unnamed.into_iter().enumerate().map(|(idx, f)| {
                        let ident = format!("Field_{}", idx);
                        let ident = Ident::new(&ident, Span::call_site());
                        let ty = f.ty.clone();
                        quote! {
                            #ident(M::Type<'ast, #ty>),
                        }
                    }).collect();

                    quote! {
                        #[derive(Debug, Clone)]
                        pub enum Field<'ast, M: Mutability> {
                            #fields
                        }
                    }
                },
                Fields::Unit => { quote! {}.into() },
            };

            quote! {
                #the_enum
            }
        }
    }
}

#[cfg(feature = "visitor_ext")]
fn main() {
    generate_ast_enums::do_it()
}

#[cfg(not(feature = "visitor_ext"))]
fn main() {
    // no-op
}
