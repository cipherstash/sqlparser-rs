use ident_case::RenameRule;
use proc_macro2::{TokenStream, Span};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::ext::IdentExt;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, Attribute, Data, DeriveInput, Fields, GenericParam, Generics,
    Ident, Index, Lit, Meta, MetaNameValue, NestedMeta,
};


/// Implementation of `[#derive(Visit)]`
#[proc_macro_derive(VisitMut, attributes(visit))]
pub fn derive_visit_mut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_visit(input, &VisitType {
        visit_trait: quote!(VisitMut),
        visitor_trait: quote!(VisitorMut),
        modifier: Some(quote!(mut)),
    })
}

/// Implementation of `[#derive(Visit)]`
#[proc_macro_derive(Visit, attributes(visit))]
pub fn derive_visit_immutable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_visit(input, &VisitType {
        visit_trait: quote!(Visit),
        visitor_trait: quote!(Visitor),
        modifier: None,
    })
}

/// Implementation of `[#derive(VisitExt)]`
#[proc_macro_derive(VisitExt)]
pub fn derive_visit_ext(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = &parse_macro_input!(input as DeriveInput);
    let name = input.ident.clone();

    let generics = add_trait_bounds_for_visit_ext(input.clone().generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let children = visit_ext_children(&input.clone(), None);
    let children_mut = visit_ext_children(&input, Some(quote!(mut)));

    quote! {
        impl #impl_generics sqlparser::ast::VisitExt for #name #ty_generics #where_clause {
            fn visit_ext<V: sqlparser::ast::VisitorExt>(&self, visitor: &mut V) -> std::ops::ControlFlow<(), sqlparser::ast::VisitOption> {
                match visitor.enter_node(&sqlparser::ast::Node::#name(self)) {
                    std::ops::ControlFlow::Continue(sqlparser::ast::VisitOption::AllFields) => {
                        match self {
                            #children
                        }
                    },
                    std::ops::ControlFlow::Continue(sqlparser::ast::VisitOption::SkipFields) => {},
                    _ => return std::ops::ControlFlow::Break(()),
                }

                visitor.leave_node(&sqlparser::ast::Node::#name(self))?;
                std::ops::ControlFlow::Continue(sqlparser::ast::VisitOption::AllFields)
            }

            fn visit_ext_mut<V: sqlparser::ast::VisitorExtMut>(&mut self, visitor: &mut V) -> std::ops::ControlFlow<(), sqlparser::ast::VisitOption> {
                use std::ops::DerefMut;
                let rc_self = std::rc::Rc::new(std::cell::RefCell::new(self));
                match visitor.enter_node(&sqlparser::ast::NodeMut::#name(rc_self.clone())) {
                    std::ops::ControlFlow::Continue(sqlparser::ast::VisitOption::AllFields) => {
                        match rc_self.borrow_mut().deref_mut() {
                            #children_mut
                        }
                    },
                    std::ops::ControlFlow::Continue(sqlparser::ast::VisitOption::SkipFields) => {},
                    _ => return std::ops::ControlFlow::Break(()),
                }

                visitor.leave_node(&sqlparser::ast::NodeMut::#name(rc_self.clone()))?;
                std::ops::ControlFlow::Continue(sqlparser::ast::VisitOption::AllFields)
            }
        }
    }.into()
}

fn visit_ext_children(input: &DeriveInput, modifier: Option<TokenStream>) -> TokenStream {
    let name = input.ident.clone();
    let ty_mod_ident: Ident = Case::Snake.convert(&input.ident);

    let visit_fn: Ident = match modifier {
        Some(_) => Ident::new("visit_ext_mut", Span::call_site()),
        None => Ident::new("visit_ext", Span::call_site()),
    };

    let ref_kind: TokenStream = if modifier.is_some() {
        quote! { ref mut }
    } else {
        quote! { ref }
    };

    // let ref_kind: TokenStream = quote! { };

    match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let field_ident = f.ident.clone().unwrap();
                    let field_enum_variant = Case::Pascal.convert(&f.ident.clone().unwrap());

                    let wrap_field: TokenStream = match modifier {
                        Some(_) => quote! {
                            sqlparser::ast::FieldMut::#name(
                                sqlparser::ast::meta::#ty_mod_ident::FieldMut::#field_enum_variant(#field_ident)
                                // sqlparser::ast::meta::#ty_mod_ident::FieldMut::#field_enum_variant(std::rc::Rc::new(std::cell::RefCell::new(#field_ident)))
                            )
                        },
                        None => quote! {
                            sqlparser::ast::Field::#name(
                                sqlparser::ast::meta::#ty_mod_ident::Field::#field_enum_variant(#field_ident)
                            )
                        }
                    }.into();

                    quote_spanned!(f.span() =>
                        visitor.enter_field(&#wrap_field)?;
                        #field_ident.#visit_fn(visitor)?;
                        visitor.leave_field(&#wrap_field)?;
                    )
                });

                let field_names = fields.named.iter().map(|f| { let ident = f.ident.clone(); ident.unwrap()});
                quote! {
                    Self{#(#ref_kind #field_names),*} => {
                        #(#recurse)*
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let visit = fields.unnamed.iter().enumerate().map(|(i, f)| {
                    let field_enum_variant = Case::Pascal.convert(&Ident::new(&format!("Field{}", i), Span::call_site()));

                    let field = Ident::new(&format!("f{}", i), Span::call_site());

                    let wrap_field: TokenStream = match modifier {
                        Some(_) => quote! {
                            sqlparser::ast::FieldMut::#name(
                                // sqlparser::ast::meta::#ty_mod_ident::FieldMut::#field_enum_variant(std::rc::Rc::new(std::cell::RefCell::new(#field)))
                                sqlparser::ast::meta::#ty_mod_ident::FieldMut::#field_enum_variant(#field)
                            )
                        },
                        None => quote! {
                            sqlparser::ast::Field::#name(
                                sqlparser::ast::meta::#ty_mod_ident::Field::#field_enum_variant(#field)
                            )
                        }
                    }.into();


                    quote_spanned!(f.span() =>
                        visitor.enter_field(&#wrap_field)?;
                        #field.#visit_fn(visitor)?;
                        visitor.leave_field(&#wrap_field)?;
                    )
                });

                let field_names = fields.unnamed.iter().enumerate().map(|(idx,_)| { Ident::new(&format!("f{}", idx), Span::call_site()) });

                quote! {
                    Self(#(#ref_kind #field_names),*) => {
                        #(#visit)*
                    }
                }
            }
            Fields::Unit => { quote!() }
        },
        Data::Enum(data) => {
            let match_arms = data.variants.iter().map(|v| {
                let variant_name = &v.ident;
                let variant_mod = Case::Snake.convert(&v.ident);
                match &v.fields {
                    Fields::Named(fields) => {
                        let names = fields.named.iter().map(|f| { let ident = f.ident.clone(); ident.unwrap()});

                        let visit = fields.named.iter().map(|f| {
                            let field_ident = f.ident.clone().unwrap();
                            let field_enum_variant = Case::Pascal.convert(&field_ident);

                            let wrap_field: TokenStream = match modifier {
                                Some(_) => quote! {
                                    sqlparser::ast::FieldMut::#name(
                                        sqlparser::ast::meta::#ty_mod_ident::FieldMut::#variant_name(
                                            // sqlparser::ast::meta::#ty_mod_ident::#variant_mod::FieldMut::#field_enum_variant(std::rc::Rc::new(std::cell::RefCell::new(#field_ident)))
                                            sqlparser::ast::meta::#ty_mod_ident::#variant_mod::FieldMut::#field_enum_variant(#field_ident)
                                        )
                                    )
                                },
                                None => quote! {
                                    sqlparser::ast::Field::#name(
                                        sqlparser::ast::meta::#ty_mod_ident::Field::#variant_name(
                                            sqlparser::ast::meta::#ty_mod_ident::#variant_mod::Field::#field_enum_variant(#field_ident)
                                        )
                                    )
                                }
                            }.into();

                            quote! {
                                visitor.enter_field(&#wrap_field)?;
                                #field_ident.#visit_fn(visitor)?;
                                visitor.leave_field(&#wrap_field)?;
                            }
                        });

                        quote!(
                            Self::#variant_name { #(#ref_kind #names),* } => {
                                #(#visit)*
                            }
                        )
                    }
                    Fields::Unnamed(fields) => {
                        let visit = fields.unnamed.iter().enumerate().map(|(i, f)| {
                            let index = Index::from(i);
                            let field = format_ident!("f{}", index);
                            let field_enum_variant = Case::Pascal.convert(&Ident::new(&format!("Field{}", i), Span::call_site()));

                            let wrap_field: TokenStream = match modifier {
                                Some(_) => quote! {
                                    sqlparser::ast::FieldMut::#name(
                                        sqlparser::ast::meta::#ty_mod_ident::FieldMut::#variant_name(
                                            // sqlparser::ast::meta::#ty_mod_ident::#variant_mod::FieldMut::#field_enum_variant(std::rc::Rc::new(std::cell::RefCell::new(#field)))
                                            sqlparser::ast::meta::#ty_mod_ident::#variant_mod::FieldMut::#field_enum_variant(#field)
                                        )
                                    )
                                },
                                None => quote! {
                                    sqlparser::ast::Field::#name(
                                        sqlparser::ast::meta::#ty_mod_ident::Field::#variant_name (
                                            sqlparser::ast::meta::#ty_mod_ident::#variant_mod::Field::#field_enum_variant(#field)
                                        )
                                    )
                                }
                            }.into();

                            quote_spanned!(f.span() =>
                                visitor.enter_field(&#wrap_field)?;
                                #field.#visit_fn(visitor)?;
                                visitor.leave_field(&#wrap_field)?;
                            )
                        });
                        let field_names = fields.unnamed.iter().enumerate().map(|(i, _)| format_ident!("f{}", i));
                        quote! {
                            Self::#variant_name(#(#ref_kind #field_names),*)  => {
                                #(#visit)*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            Self::#variant_name => {}
                        }
                    }
                }
            });

            quote! {
                #(#match_arms),*
            }
        }
        Data::Union(_) => unimplemented!(),
    }
}


struct VisitType {
    visit_trait: TokenStream,
    visitor_trait: TokenStream,
    modifier: Option<TokenStream>,
}

fn derive_visit(
    input: proc_macro::TokenStream,
    visit_type: &VisitType,
) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let VisitType { visit_trait, visitor_trait, modifier } = visit_type;

    let attributes = Attributes::parse(&input.attrs);
    // Add a bound `T: Visit` to every type parameter T.
    let generics = add_trait_bounds(input.generics, visit_type);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let (pre_visit, post_visit) = attributes.visit(quote!(self));
    let children = visit_children(&input.data, visit_type);

    let expanded = quote! {
        // The generated impl.
        impl #impl_generics sqlparser::ast::#visit_trait for #name #ty_generics #where_clause {
            fn visit<V: sqlparser::ast::#visitor_trait>(
                &#modifier self,
                visitor: &mut V
            ) -> ::std::ops::ControlFlow<V::Break> {
                #pre_visit
                #children
                #post_visit
                ::std::ops::ControlFlow::Continue(())
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

/// Parses attributes that can be provided to this macro
///
/// `#[visit(leaf, with = "visit_expr")]`
#[derive(Default)]
struct Attributes {
    /// Content for the `with` attribute
    with: Option<Ident>,
}

impl Attributes {
    fn parse(attrs: &[Attribute]) -> Self {
        let mut out = Self::default();
        for attr in attrs.iter().filter(|a| a.path.is_ident("visit")) {
            let meta = attr.parse_meta().expect("visit attribute");
            match meta {
                Meta::List(l) => {
                    for nested in &l.nested {
                        match nested {
                            NestedMeta::Meta(Meta::NameValue(v)) => out.parse_name_value(v),
                            _ => panic!("Expected #[visit(key = \"value\")]"),
                        }
                    }
                }
                _ => panic!("Expected #[visit(...)]"),
            }
        }
        out
    }

    /// Updates self with a name value attribute
    fn parse_name_value(&mut self, v: &MetaNameValue) {
        if v.path.is_ident("with") {
            match &v.lit {
                Lit::Str(s) => self.with = Some(format_ident!("{}", s.value(), span = s.span())),
                _ => panic!("Expected a string value, got {}", v.lit.to_token_stream()),
            }
            return;
        }
        panic!("Unrecognised kv attribute {}", v.path.to_token_stream())
    }

    /// Returns the pre and post visit token streams
    fn visit(&self, s: TokenStream) -> (Option<TokenStream>, Option<TokenStream>) {
        let pre_visit = self.with.as_ref().map(|m| {
            let m = format_ident!("pre_{}", m);
            quote!(visitor.#m(#s)?;)
        });
        let post_visit = self.with.as_ref().map(|m| {
            let m = format_ident!("post_{}", m);
            quote!(visitor.#m(#s)?;)
        });
        (pre_visit, post_visit)
    }
}

// Add a bound `T: Visit` to every type parameter T.
fn add_trait_bounds(mut generics: Generics, VisitType{visit_trait, ..}: &VisitType) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(sqlparser::ast::#visit_trait));
        }
    }
    generics
}

fn add_trait_bounds_for_visit_ext(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(sqlparser::ast::visitor_ext::VisitExt));
        }
    }
    generics
}

// Generate the body of the visit implementation for the given type
fn visit_children(data: &Data, VisitType{visit_trait, modifier, ..}: &VisitType) -> TokenStream {
    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let attributes = Attributes::parse(&f.attrs);
                    let (pre_visit, post_visit) = attributes.visit(quote!(&#modifier self.#name));
                    quote_spanned!(f.span() => #pre_visit sqlparser::ast::#visit_trait::visit(&#modifier self.#name, visitor)?; #post_visit)
                });
                quote! {
                    #(#recurse)*
                }
            }
            Fields::Unnamed(fields) => {
                let recurse = fields.unnamed.iter().enumerate().map(|(i, f)| {
                    let index = Index::from(i);
                    let attributes = Attributes::parse(&f.attrs);
                    let (pre_visit, post_visit) = attributes.visit(quote!(&self.#index));
                    quote_spanned!(f.span() => #pre_visit sqlparser::ast::#visit_trait::visit(&#modifier self.#index, visitor)?; #post_visit)
                });
                quote! {
                    #(#recurse)*
                }
            }
            Fields::Unit => {
                quote!()
            }
        },
        Data::Enum(data) => {
            let statements = data.variants.iter().map(|v| {
                let name = &v.ident;
                match &v.fields {
                    Fields::Named(fields) => {
                        let names = fields.named.iter().map(|f| &f.ident);
                        let visit = fields.named.iter().map(|f| {
                            let name = &f.ident;
                            let attributes = Attributes::parse(&f.attrs);
                            let (pre_visit, post_visit) = attributes.visit(name.to_token_stream());
                            quote_spanned!(f.span() => #pre_visit sqlparser::ast::#visit_trait::visit(#name, visitor)?; #post_visit)
                        });

                        quote!(
                            Self::#name { #(#names),* } => {
                                #(#visit)*
                            }
                        )
                    }
                    Fields::Unnamed(fields) => {
                        let names = fields.unnamed.iter().enumerate().map(|(i, f)| format_ident!("_{}", i, span = f.span()));
                        let visit = fields.unnamed.iter().enumerate().map(|(i, f)| {
                            let name = format_ident!("_{}", i);
                            let attributes = Attributes::parse(&f.attrs);
                            let (pre_visit, post_visit) = attributes.visit(name.to_token_stream());
                            quote_spanned!(f.span() => #pre_visit sqlparser::ast::#visit_trait::visit(#name, visitor)?; #post_visit)
                        });

                        quote! {
                            Self::#name ( #(#names),*) => {
                                #(#visit)*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            Self::#name => {}
                        }
                    }
                }
            });

            quote! {
                match self {
                    #(#statements),*
                }
            }
        }
        Data::Union(_) => unimplemented!(),
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