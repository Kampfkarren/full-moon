use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

pub trait DeriveGenerator: EnumGenerator + StructGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream;

    fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
        let input = parse_macro_input!(input as DeriveInput);

        let expanded = match &input.data {
            syn::Data::Enum(data) => <Self as EnumGenerator>::generate(&input.ident, data),
            syn::Data::Struct(data) => <Self as StructGenerator>::generate(&input.ident, data),
            _ => unimplemented!(),
        };

        Self::complete(&input, expanded).into()
    }
}

pub trait EnumGenerator {
    fn generate(input: &syn::Ident, enumm: &syn::DataEnum) -> TokenStream;
}

pub trait StructGenerator {
    fn generate(input: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream;
}

pub trait MatchEnumGenerator {
    const DEREF: bool = false;
    const SELF: &'static str = "self";

    fn complete(input: TokenStream) -> TokenStream {
        input
    }

    fn case_named(
        _input: &syn::Ident,
        _variant: &syn::Ident,
        _fields: &syn::FieldsNamed,
    ) -> TokenStream {
        quote! {}
    }

    fn case_unnamed(
        _input: &syn::Ident,
        _variant: &syn::Ident,
        _fields: &syn::FieldsUnnamed,
    ) -> TokenStream {
        quote! {}
    }

    fn case_unit(_input: &syn::Ident, _variant: &syn::Ident) -> TokenStream {
        quote! {}
    }
}

impl<T: MatchEnumGenerator> EnumGenerator for T {
    fn generate(input_ident: &syn::Ident, enumm: &syn::DataEnum) -> TokenStream {
        let mut cases = Vec::with_capacity(enumm.variants.len());

        for variant in &enumm.variants {
            let variant_ident = &variant.ident;

            match &variant.fields {
                syn::Fields::Named(fields) => {
                    cases.push(T::case_named(input_ident, variant_ident, fields));
                }

                syn::Fields::Unnamed(fields) => {
                    cases.push(T::case_unnamed(input_ident, variant_ident, fields));
                }

                syn::Fields::Unit => cases.push(T::case_unit(input_ident, variant_ident)),
            }
        }

        let self_ident = format_ident!("{}", T::SELF);
        let deref = if T::DEREF { Some(quote! { * }) } else { None };

        T::complete(quote! {
            match #deref#self_ident {
                #(#cases)*
            }
        })
    }
}

pub trait Hint
where
    Self: Sized,
{
    fn key_value(_key: String, _value: String) -> Option<Self> {
        None
    }

    fn unit(_name: String) -> Option<Self> {
        None
    }
}

pub fn search_hint<T: Hint>(name: &str, attrs: &[syn::Attribute]) -> Option<T> {
    macro_rules! path_ident {
        ($path:expr) => {
            match $path.get_ident() {
                Some(ident) => ident,
                None => continue,
            }
        };
    }

    for attr in attrs {
        let meta = match attr.parse_meta() {
            Ok(meta) => meta,
            Err(_) => continue,
        };

        if path_ident!(meta.path()) != name {
            continue;
        };

        if let syn::Meta::List(list) = meta {
            for nested in list.nested {
                match nested {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                        return T::unit(path_ident!(path).to_string());
                    }

                    syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) => {
                        return T::key_value(
                            path_ident!(name_value.path).to_string(),
                            match name_value.lit {
                                syn::Lit::Str(lit_str) => lit_str.value(),

                                other => unimplemented!("nested meta value: {:#?}", other),
                            },
                        );
                    }

                    other => unimplemented!("unknown attribute: {:#?}", other),
                }
            }
        }
    }

    None
}
