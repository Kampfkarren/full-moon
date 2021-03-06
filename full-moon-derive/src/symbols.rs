// Used to create the Symbol enum, as well as the combinator for it
// Needed because alt() only takes up to 21 elements
// The combinator provided will give up to 20 symbols, with the last being an alt() for the rest.

use indexmap::IndexMap;
use quote::quote;
use syn::{
    self,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitStr, Token,
};

const ALT_LIMIT: usize = 21;

#[derive(Debug)]
struct SymbolsInput {
    symbols: IndexMap<Ident, LitStr>,
}

#[derive(Debug)]
enum ParseState {
    Ident,
    Arrow,
    String,
    Comma,
}

impl Parse for SymbolsInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut looking_for = ParseState::Ident;
        let mut current_ident = None;
        let mut symbols = IndexMap::new();

        while !input.is_empty() {
            looking_for = match looking_for {
                ParseState::Ident => {
                    current_ident = Some(input.parse()?);
                    ParseState::Arrow
                }

                ParseState::Arrow => {
                    input.parse::<Token![=>]>()?;
                    ParseState::String
                }

                ParseState::String => {
                    symbols.insert(current_ident.take().unwrap(), input.parse::<LitStr>()?);
                    ParseState::Comma
                }

                ParseState::Comma => {
                    input.parse::<Token![,]>()?;
                    ParseState::Ident
                }
            };
        }

        Ok(Self { symbols })
    }
}

pub fn parse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let symbols = parse_macro_input!(input as SymbolsInput).symbols;

    let string: Vec<_> = symbols.values().collect();
    let ident: Vec<_> = symbols.keys().collect();
    let symbols: Vec<_> = symbols.iter().collect();
    let splits = symbols.chunks(ALT_LIMIT).map(|symbols| {
        let parsers = symbols.iter().map(|(symbol, string)| {
            // Note this doesn't handle the case of keywords with digits
            // which doesn't currently occur.
            let matcher = if string
                .value()
                .chars()
                .all(|char| char.is_ascii_alphabetic() || char == '_')
            {
                quote! {
                    terminated(
                        tag(#string),
                        not(alt((alphanumeric1, tag("_")))),
                    )
                }
            } else {
                quote! {tag(#string)}
            };
            quote! {value(Symbol::#symbol, #matcher)}
        });

        quote! {
            alt((#(
                #parsers,
            )*))
        }
    });

    let output = quote! {
        /// A literal symbol, used for both words important to syntax (like while) and operators (like +)
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        pub enum Symbol {
            #(
                #[cfg_attr(feature = "serde", serde(rename = #string))]
                #[allow(missing_docs)]
                #ident,
            )*
        }

        impl<'a> fmt::Display for Symbol {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    #(Symbol::#ident => #string,)*
                }
                .fmt(formatter)
            }
        }

        impl FromStr for Symbol {
            type Err = ();

            fn from_str(string: &str) -> Result<Self, Self::Err> {
                Ok(match string {
                    #(#string => Symbol::#ident,)*
                    _ => return Err(()),
                })
            }
        }

        fn parse_symbol(code: &str) -> IResult<&str, Symbol> {
            use ::nom::{
                branch::alt,
                bytes::complete::{tag},
                character::complete::{alphanumeric1},
                combinator::{not, value},
                sequence::terminated,
            };
            let mut combinator = alt((
                #(
                    #splits,
                )*
            ));
            combinator(code)
        }
    };

    output.into()
}
