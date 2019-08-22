#![recursion_limit = "128"]

extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

// Not 100% accurate, but it is to full-moon's codebase
fn snake_case(pascal_case: &str) -> String {
    let mut chars = pascal_case.chars();
    let mut result = chars.next().unwrap().to_ascii_lowercase().to_string();

    for character in chars {
        if character.is_ascii_uppercase() {
            result.push('_');
            result.push(character.to_ascii_lowercase());
        } else {
            result.push(character);
        }
    }

    result
}

enum VisitSelfHint {
    Skip,
    VisitAs(String),
}

fn visit_self_hint(attrs: &[syn::Attribute]) -> Option<VisitSelfHint> {
    for attr in attrs {
        let meta = match attr.parse_meta() {
            Ok(meta) => meta,
            Err(_) => continue,
        };

        if meta.name() != "visit" {
            continue;
        }

        if let syn::Meta::List(list) = meta {
            for nested in list.nested.iter() {
                match nested {
                    syn::NestedMeta::Meta(syn::Meta::Word(word)) => {
                        if word == "skip_visit_self" {
                            return Some(VisitSelfHint::Skip);
                        }
                    }

                    syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) => {
                        if name_value.ident == "visit_as" {
                            match &name_value.lit {
                                syn::Lit::Str(lit_str) => {
                                    return Some(VisitSelfHint::VisitAs(lit_str.value()))
                                }
                                _ => panic!("expected literal string for visit_as"),
                            }
                        }
                    }

                    _ => {}
                }
            }
        }
    }

    None
}

#[proc_macro_derive(Visit, attributes(visit))]
pub fn derive_visit(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input_ident = &input.ident;

    let expanded = match &input.data {
        syn::Data::Enum(enumm) => {
            let mut cases = Vec::with_capacity(enumm.variants.len());

            for variant in &enumm.variants {
                let variant_ident = &variant.ident;
                match &variant.fields {
                    syn::Fields::Named(named) => {
                        let fields: Vec<_> = named
                            .named
                            .iter()
                            .map(|field| field.ident.as_ref().unwrap())
                            .collect();
                        let fields = &fields;

                        cases.push(quote! {
                            #input_ident::#variant_ident {
                                #(#fields,)*
                            } => {
                                #(
                                    visit!(#fields, visitor);
                                )*
                            }
                        });
                    }

                    syn::Fields::Unnamed(fields) => {
                        let fields: Vec<_> = fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(index, _)| {
                                syn::Ident::new(&format!("__self_{}", index), variant_ident.span())
                            })
                            .collect();
                        let fields = &fields;

                        cases.push(quote! {
                            #input_ident::#variant_ident(
                                #(#fields,)*
                            ) => {
                                #(
                                    visit!(#fields, visitor);
                                )*
                            }
                        });
                    }

                    syn::Fields::Unit => {}
                }
            }

            quote! {
                match self {
                    #(#cases)*
                    _ => {}
                }
            }
        }

        syn::Data::Struct(strukt) => {
            let fields = strukt
                .fields
                .iter()
                .filter(|field| {
                    field
                        .attrs
                        .iter()
                        .filter_map(|attr| attr.parse_meta().ok())
                        .filter(|attr| attr.name() == "visit")
                        .filter_map(|variant| {
                            if let syn::Meta::List(list) = variant {
                                Some(list)
                            } else {
                                None
                            }
                        })
                        .all(|list| {
                            for nested in list.nested {
                                if let syn::NestedMeta::Meta(meta) = nested {
                                    if let syn::Meta::Word(ident) = meta {
                                        if ident == "skip" {
                                            return false;
                                        }
                                    }
                                }
                            }

                            true
                        })
                })
                .map(|field| field.ident.as_ref().unwrap());

            quote! {
                #(visit!(self.#fields, visitor);)*
            }
        }

        other => panic!("don't know how to derive Visit from {:?}", other),
    };

    let lifetime = input
        .generics
        .params
        .first()
        .and_then(|param| match param.value() {
            syn::GenericParam::Lifetime(lifetime) => Some(lifetime),
            _ => None,
        })
        .expect("must derive something with a lifetime");

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let visit_self = match visit_self_hint(&input.attrs) {
        Some(VisitSelfHint::Skip) => quote! {},
        Some(VisitSelfHint::VisitAs(visit_as)) => {
            let visit_as = syn::Ident::new(&format!("visit_{}", visit_as), input_ident.span());
            quote! {
                visitor.#visit_as(self);
            }
        }
        None => {
            // name of self in snake_case
            let ssself = syn::Ident::new(
                &format!("visit_{}", snake_case(&input_ident.to_string())),
                input_ident.span(),
            );
            quote! {
                visitor.#ssself(self);
            }
        }
    };

    let source = quote! {
        impl #impl_generics crate::visitors::Visit<#lifetime> for #input_ident #ty_generics #where_clause {
            fn visit<V: crate::visitors::Visitor<#lifetime>>(&self, visitor: &mut V) {
                macro_rules! visit {
                    ($visit_what: expr, $visitor: expr) => {
                        $visit_what.visit($visitor);
                    }
                }

                #visit_self
                #expanded
            }
        }

        impl #impl_generics crate::visitors::VisitMut<#lifetime> for #input_ident #ty_generics #where_clause {
            fn visit_mut<V: crate::visitors::VisitorMut<#lifetime>>(&mut self, visitor: &mut V) {
                macro_rules! visit {
                    ($visit_what: expr, $visitor: expr) => {
                        $visit_what.visit_mut($visitor);
                    }
                }

                #visit_self
                #expanded
            }
        }
    };

    TokenStream::from(source)
}

#[proc_macro_derive(Node)]
pub fn derive_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input_ident = &input.ident;

    let expanded = match &input.data {
        syn::Data::Enum(enumm) => {
            let mut cases = Vec::with_capacity(enumm.variants.len());

            for variant in &enumm.variants {
                let variant_ident = &variant.ident;

                match &variant.fields {
                    syn::Fields::Named(named) => {
                        let fields = named
                            .named
                            .iter()
                            .map(|field| field.ident.as_ref().unwrap())
                            .collect::<Vec<_>>();

                        let (mut start_position, mut end_position) = (
                            Vec::with_capacity(fields.len()),
                            Vec::with_capacity(fields.len()),
                        );

                        for field in &fields {
                            start_position.push(quote! {
                                .or_else(|| {
                                    #field.start_position()
                                })
                            });
                        }

                        for field in fields.iter().rev() {
                            end_position.push(quote! {
                                .or_else(|| {
                                    #field.end_position()
                                })
                            });
                        }

                        cases.push(quote! {
                            #input_ident::#variant_ident {
                                #(#fields,)*
                            } => {
                                Some((None#(#start_position)*?, None#(#end_position)*?))
                            }
                        });
                    }

                    syn::Fields::Unnamed(unnamed) => {
                        let fields = &unnamed.unnamed;
                        if fields.len() == 1 {
                            cases.push(quote! {
                                #input_ident::#variant_ident(inner) => {
                                    Some((inner.start_position()?, inner.end_position()?))
                                }
                            })
                        } else {
                            let fields_count = fields.len() - 1;
                            let field_match: Vec<_> = fields
                                .iter()
                                .enumerate()
                                .map(|(index, _)| {
                                    syn::Ident::new(
                                        match index {
                                            0 => "first",
                                            _x if _x == fields_count => "last",
                                            _ => "_",
                                        },
                                        variant_ident.span(),
                                    )
                                })
                                .collect();

                            cases.push(quote! {
                                #input_ident::#variant_ident(
                                    #(#field_match,)*
                                ) => {
                                    Some((first.start_position()?, last.end_position()?))
                                }
                            })
                        }
                    }

                    syn::Fields::Unit => {}
                }
            }

            quote! {
                #[allow(unused_variables)]
                match self {
                    #(#cases)*
                }
            }
        }

        syn::Data::Struct(strukt) => {
            let fields = strukt
                .fields
                .iter()
                .map(|field| field.ident.as_ref().unwrap())
                .collect::<Vec<_>>();

            let (mut start_position, mut end_position) = (
                Vec::with_capacity(fields.len()),
                Vec::with_capacity(fields.len()),
            );

            for field in &fields {
                start_position.push(quote! {
                    self.#field.start_position()
                });
            }

            for field in &fields {
                end_position.push(quote! {
                    self.#field.end_position()
                });
            }

            quote! {
                Some((
                    vec![#(#start_position,)*].into_iter().flatten().min()?,
                    vec![#(#end_position,)*].into_iter().flatten().max()?,
                ))
            }
        }

        other => panic!("don't know how to derive Node from {:?}", other),
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let source = quote! {
        impl #impl_generics crate::node::Node for #input_ident #ty_generics #where_clause {
            fn start_position(&self) -> Option<crate::tokenizer::Position> {
                Some(#expanded?.0)
            }

            fn end_position(&self) -> Option<crate::tokenizer::Position> {
                Some(#expanded?.1)
            }
        }

        impl #impl_generics crate::private::Sealed for #input_ident #ty_generics #where_clause {}
    };

    TokenStream::from(source)
}

#[proc_macro_derive(Owned)]
pub fn derive_owned(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input_ident = &input.ident;

    let expanded = match &input.data {
        syn::Data::Enum(enumm) => {
            let mut cases = Vec::with_capacity(enumm.variants.len());

            for variant in &enumm.variants {
                let variant_ident = &variant.ident;
                match &variant.fields {
                    syn::Fields::Named(named) => {
                        let fields: Vec<_> = named
                            .named
                            .iter()
                            .map(|field| field.ident.as_ref().unwrap())
                            .collect();
                        let fields = &fields;
                        let fields2 = fields.clone();

                        cases.push(quote! {
                            #input_ident::#variant_ident {
                                #(#fields,)*
                            } => {
                                #input_ident::#variant_ident {
                                    #(
                                        #fields: #fields2.owned(),
                                    )*
                                }
                            }
                        });
                    }

                    syn::Fields::Unnamed(fields) => {
                        let fields: Vec<_> = fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(index, _)| {
                                syn::Ident::new(&format!("__self_{}", index), variant_ident.span())
                            })
                            .collect();
                        let fields = &fields;

                        cases.push(quote! {
                            #input_ident::#variant_ident(
                                #(#fields,)*
                            ) => {
                                #input_ident::#variant_ident(
                                    #(
                                        #fields.owned(),
                                    )*
                                )
                            }
                        });
                    }

                    syn::Fields::Unit => {
                        cases.push(quote! {
                            #input_ident::#variant_ident => #input_ident::#variant_ident
                        })
                    }
                }
            }

            quote! {
                match self {
                    #(#cases)*
                }
            }
        }

        syn::Data::Struct(strukt) => {
            let fields = strukt
                .fields
                .iter()
                .map(|field| field.ident.as_ref().unwrap())
                .collect::<Vec<_>>();
            let fields2 = fields.clone();

            quote! {
                #input_ident {
                    #(
                        #fields: self.#fields2.owned(),
                    )*
                }
            }
        }

        other => panic!("don't know how to derive Visit from {:?}", other),
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let source = quote! {
        impl #impl_generics crate::ast::owned::Owned for #input_ident #ty_generics #where_clause {
            type Owned = #input_ident<'static>;

            fn owned(&self) -> Self::Owned {
                #expanded
            }
        }
    };

    TokenStream::from(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snake_case() {
        assert_eq!(snake_case("LocalAssignment"), "local_assignment");
    }
}
