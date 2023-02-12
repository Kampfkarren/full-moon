use crate::derive::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

fn token_getter(
    ty: &syn::Type,
    ident: &syn::Ident,
    mut prefix: Option<TokenStream>,
    deref: bool,
) -> TokenStream {
    if let syn::Type::Path(path) = ty {
        let token_ref = path.path.segments.first().expect("no first segment?");

        // Clippy suggests *cow.ident, which doesn't work
        #[allow(clippy::cmp_owned)]
        if token_ref.ident.to_string() == "TokenReference" {
            return quote! {
                crate::node::TokenItem::TokenReference(&#prefix #ident)
            };
        }
    }

    if deref {
        prefix = Some(quote! { * });
    }

    quote! {
        crate::node::TokenItem::MoreTokens(&#prefix #ident)
    }
}

#[derive(PartialEq)]
enum NodeHint {
    FullRange,
}

impl Hint for NodeHint {
    fn unit(name: String) -> Option<Self> {
        if name == "full_range" {
            Some(NodeHint::FullRange)
        } else {
            None
        }
    }
}

pub struct NodeGenerator;

impl DeriveGenerator for NodeGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream {
        let input_ident = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let macro_name = format_ident!("NodeGenerator{}", input_ident);

        let pattern = quote! {{
            range => $range:expr,
            similar => $similar:expr,
            tokens => $tokens:expr,
        }};

        quote! {
            macro_rules! #macro_name {
                ("range", #pattern) => {
                    $range
                };

                ("similar", #pattern) => {
                    $similar
                };

                ("tokens", #pattern) => {
                    $tokens
                }
            }

            impl #impl_generics crate::node::Node #impl_generics for #input_ident #ty_generics #where_clause {
                fn start_position(&self) -> Option<crate::tokenizer::Position> {
                    Some(#macro_name!("range", { #tokens })?.0)
                }

                fn end_position(&self) -> Option<crate::tokenizer::Position> {
                    Some(#macro_name!("range", { #tokens })?.1)
                }

                fn similar(&self, other: &Self) -> bool {
                    #macro_name!("similar", { #tokens })
                }

                fn tokens<'a>(&'a self) -> crate::node::Tokens<'a> {
                    #macro_name!("tokens", { #tokens })
                }
            }

            impl #impl_generics crate::private::Sealed for #input_ident #ty_generics #where_clause {}
        }
    }
}

impl StructGenerator for NodeGenerator {
    fn generate(ident: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let range = StructRangeGenerator::generate(ident, strukt);
        let similar = StructSimilarGenerator::generate(ident, strukt);
        let tokens = StructTokensGenerator::generate(ident, strukt);

        quote! {
            range => { #range },
            similar => { #similar },
            tokens => { #tokens },
        }
    }
}

pub struct StructRangeGenerator;

impl StructGenerator for StructRangeGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let full_range = strukt
            .fields
            .iter()
            .find(|field| search_hint("node", &field.attrs) == Some(NodeHint::FullRange));

        if let Some(full_range) = full_range {
            let ident = full_range.ident.as_ref().unwrap();
            quote! {
                self.#ident.range()
            }
        } else {
            let (mut start_position, mut end_position) = (
                Vec::with_capacity(fields.len()),
                Vec::with_capacity(fields.len()),
            );

            for field in &fields {
                start_position.push(quote! {
                    .or_else(|| {
                        self.#field.start_position()
                    })
                });
            }

            for field in fields.iter().rev() {
                end_position.push(quote! {
                    .or_else(|| {
                        self.#field.end_position()
                    })
                });
            }

            quote! {
                Some((None #(#start_position)*?, None #(#end_position)*?))
            }
        }
    }
}

pub struct StructSimilarGenerator;

impl StructGenerator for StructSimilarGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        quote! {
            #(
                self.#fields.similar(&other.#fields) &&
            )* true
        }
    }
}

pub struct StructTokensGenerator;

impl StructGenerator for StructTokensGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let mut getters = Vec::with_capacity(strukt.fields.len());

        for field in &strukt.fields {
            getters.push(token_getter(
                &field.ty,
                field.ident.as_ref().unwrap(),
                Some(quote! {
                    self.
                }),
                false,
            ));
        }

        quote! {
            crate::node::Tokens {
                items: vec![#(
                    #getters,
                )*],
            }
        }
    }
}

impl EnumGenerator for NodeGenerator {
    fn generate(ident: &syn::Ident, enumm: &syn::DataEnum) -> TokenStream {
        let range = EnumRangeGenerator::generate(ident, enumm);
        let similar = EnumSimilarGenerator::generate(ident, enumm);
        let tokens = EnumTokensGenerator::generate(ident, enumm);

        quote! {
            range => {
                #[allow(unused)]
                #range
            },

            similar => { #similar },
            tokens => { #tokens },
        }
    }
}

pub struct EnumRangeGenerator;

impl MatchEnumGenerator for EnumRangeGenerator {
    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        named: &syn::FieldsNamed,
    ) -> TokenStream {
        let fields = named
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let full_range = named
            .named
            .iter()
            .find(|field| search_hint("node", &field.attrs) == Some(NodeHint::FullRange));

        let body = if let Some(full_range) = full_range {
            let ident = full_range.ident.as_ref().unwrap();
            quote! {
                #ident.range()
            }
        } else {
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

            quote! {
                Some((None #(#start_position)*?, None #(#end_position)*?))
            }
        };

        quote! {
            #input::#variant {
                #(#fields,)*
            } => {
                #body
            }
        }
    }

    fn case_unnamed(
        input: &syn::Ident,
        variant: &syn::Ident,
        fields: &syn::FieldsUnnamed,
    ) -> TokenStream {
        let fields = &fields.unnamed;

        if fields.len() == 1 {
            quote! {
                #input::#variant(inner) => {
                    Some((inner.start_position()?, inner.end_position()?))
                }
            }
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
                        variant.span(),
                    )
                })
                .collect();

            quote! {
                #input::#variant(
                    #(#field_match,)*
                ) => {
                    Some((first.start_position()?, last.end_position()?))
                }
            }
        }
    }
}

pub struct EnumSimilarGenerator;

impl MatchEnumGenerator for EnumSimilarGenerator {
    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        named: &syn::FieldsNamed,
    ) -> TokenStream {
        let fields = named
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let other_fields: Vec<_> = fields
            .iter()
            .map(|ident| format_ident!("other_{}", ident))
            .collect();

        quote! {
            #input::#variant {
                #(#fields,)*
            } => {
                if let #input::#variant {
                    #(
                        #fields: #other_fields,
                    )*
                } = &other {
                    #(
                        #fields.similar(#other_fields) &&
                    )* true
                } else {
                    false
                }
            }
        }
    }

    fn case_unnamed(
        input: &syn::Ident,
        variant: &syn::Ident,
        fields: &syn::FieldsUnnamed,
    ) -> TokenStream {
        let fields: Vec<_> = fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(index, _)| format_ident!("__self_{}", index))
            .collect();

        let other_fields: Vec<_> = fields
            .iter()
            .map(|ident| format_ident!("other_{}", ident))
            .collect();

        quote! {
            #input::#variant(
                #(#fields,)*
            ) => {
                if let #input::#variant(#(#other_fields,)*) = &other {
                    #(
                        #fields.similar(#other_fields) &&
                    )* true
                } else {
                    false
                }
            }
        }
    }

    fn case_unit(input: &syn::Ident, variant: &syn::Ident) -> TokenStream {
        quote! {
            #input::#variant => other == #input::#variant
        }
    }
}

pub struct EnumTokensGenerator;

impl MatchEnumGenerator for EnumTokensGenerator {
    const DEREF: bool = true;

    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        named: &syn::FieldsNamed,
    ) -> TokenStream {
        let named = &named.named;

        let mut fields = Vec::with_capacity(named.len());
        let mut getters = Vec::with_capacity(named.len());

        for field in named {
            fields.push(field.ident.as_ref().unwrap());
            getters.push(token_getter(
                &field.ty,
                field.ident.as_ref().unwrap(),
                None,
                true,
            ));
        }

        quote! {
            #input::#variant {
                #(ref #fields,)*
            } => {
                crate::node::Tokens {
                    items: vec![#(
                        #getters,
                    )*],
                }
            }
        }
    }

    fn case_unnamed(
        input: &syn::Ident,
        variant: &syn::Ident,
        fields: &syn::FieldsUnnamed,
    ) -> TokenStream {
        let names: Vec<_> = fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(index, _)| format_ident!("__self_{}", index))
            .collect();

        let mut getters = Vec::with_capacity(fields.unnamed.len());

        for (field, name) in fields.unnamed.iter().zip(&names) {
            getters.push(token_getter(&field.ty, name, None, true));
        }

        quote! {
            #input::#variant(
                #(ref #names,)*
             ) => {
                crate::node::Tokens {
                    items: vec![#(
                        #getters,
                    )*],
                }
            }
        }
    }
}
