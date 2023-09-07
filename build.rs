#[cfg(feature = "visitor_ext")]
mod generate_ast_enums {
    use std::cmp::Ordering;
    use std::env;
    use std::fs::{create_dir_all, File};
    use std::io::{prelude::*, Read};
    use std::path::Path;

    use ident_case::RenameRule;
    use proc_macro2::{Span, TokenStream};
    use quote::{quote, TokenStreamExt};
    use syn::Meta::{self, List};
    use syn::Token;
    use syn::{
        punctuated::Punctuated, Attribute, Ident, Item, ItemEnum, ItemStruct, PathArguments,
        PathSegment,
    };
    use walkdir::WalkDir;

    pub(crate) fn do_it() {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let pkg_dir = Path::new(&manifest_dir);
        let ast_dir = Path::new(&pkg_dir).join("src/ast");
        let out_dir = env::var_os("OUT_DIR").unwrap();
        let dest_dir = Path::new(&out_dir).join(Path::new("ast/generated"));
        let dest_path = Path::new(&dest_dir).join(Path::new("node.rs"));

        let mut used = TokenStream::new();

        let mut node_variants = Vec::<(String, TokenStream)>::new();
        let mut field_variants = Vec::<(String, TokenStream)>::new();

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
                    inspect_item(
                        &item,
                        &root_path,
                        &mut used,
                        &mut node_variants,
                        &mut field_variants,
                    );
                }
            }
        }

        // Sort by the identifier
        node_variants.sort_by(|a, b| a.0.cmp(&b.0));
        field_variants.sort_by(|a, b| a.0.cmp(&b.0));

        let node_variants = node_variants
            .into_iter()
            .fold(TokenStream::new(), |mut a, b| {
                a.append_all(b.1);
                a
            });

        let field_variants = field_variants
            .into_iter()
            .fold(TokenStream::new(), |mut a, b| {
                a.append_all(b.1);
                a
            });

        let generated: TokenStream = quote! {
            pub enum Node<'ast, M: Mutability> {
                #node_variants
            }

            pub enum Field<'ast, M: Mutability> {
                #field_variants
            }
        }
        .into();

        create_dir_all(&dest_dir)
            .expect(format!("Could not create directory {}", &dest_dir.to_str().unwrap()).as_str());

        let mut file = File::create(&dest_path)
            .expect(format!("Could not open {}", &dest_path.to_str().unwrap()).as_str());

        file.write_all(generated.to_string().as_bytes())
            .expect(format!("Could not write to {}", &dest_path.to_str().unwrap()).as_str());

        println!("cargo:rerun-if-changed=build.rs");
        println!("cargo:rerun-if-changed=src/ast");
    }

    fn inspect_item(
        item: &Item,
        current_path: &syn::Path,
        used: &mut TokenStream,
        node_variants: &mut Vec<(String, TokenStream)>,
        field_variants: &mut Vec<(String, TokenStream)>,
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
                        inspect_item(item, &mod_path, used, node_variants, field_variants);
                    }
                }
            }
            Item::Struct(ItemStruct { ident, attrs, .. }) => {
                if is_ast_node(&attrs) {
                    let type_path = current_path.clone();
                    let mod_ident = RenameRule::SnakeCase.apply_to_variant(ident.to_string());
                    let mod_ident = Ident::new(&mod_ident, ident.span());

                    node_variants.push((
                        ident.to_string(),
                        quote! {
                            #ident ( M::Type<'ast, crate::#type_path::#ident> ),
                        },
                    ));

                    field_variants.push((
                        ident.to_string(),
                        quote! {
                            #ident ( crate::#type_path::#mod_ident::Field<'ast, M> ),
                        },
                    ));
                }
            }
            Item::Enum(ItemEnum { ident, attrs, .. }) => {
                if is_ast_node(&attrs) {
                    let type_path = current_path.clone();
                    let mod_ident = RenameRule::SnakeCase.apply_to_variant(ident.to_string());
                    let mod_ident = Ident::new(&mod_ident, ident.span());

                    node_variants.push((
                        ident.to_string(),
                        quote! {
                            #ident ( M::Type<'ast, crate::#type_path::#ident> ),
                        },
                    ));

                    field_variants.push((
                        ident.to_string(),
                        quote! {
                            #ident ( crate::#type_path::#mod_ident::Field<'ast, M> ),
                        },
                    ));
                }
            }
            _ => {}
        }
    }

    fn is_ast_node(attrs: &Vec<Attribute>) -> bool {
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
}

#[cfg(feature = "visitor_ext")]
fn main() {
    generate_ast_enums::do_it()
}

#[cfg(not(feature = "visitor_ext"))]
fn main() {
    // no-op
}
