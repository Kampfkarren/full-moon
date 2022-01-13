// Used to create the Symbol enum, as well as the combinator for it
// Needed because alt() only takes up to 21 elements
// The combinator provided will give up to 20 symbols, with the last being an alt() for the rest.

use indexmap::IndexMap;
use quote::quote;
use syn::{
    self,
    parse::{Parse, ParseStream},
    parse_macro_input, Attribute, Ident, LitStr, Token,
};

#[derive(Debug)]
struct SymbolsInput {
    symbols: IndexMap<Ident, (Option<Attribute>, LitStr)>,
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
        let mut attribute = None;
        let mut current_ident = None;
        let mut symbols = IndexMap::new();

        while !input.is_empty() {
            looking_for = match looking_for {
                ParseState::Ident => {
                    // Optionally look for an attribute first
                    attribute = input
                        .call(Attribute::parse_outer)
                        .ok()
                        .and_then(|vec| vec.into_iter().next());
                    current_ident = Some(input.parse()?);
                    ParseState::Arrow
                }

                ParseState::Arrow => {
                    input.parse::<Token![=>]>()?;
                    ParseState::String
                }

                ParseState::String => {
                    symbols.insert(
                        current_ident.take().unwrap(),
                        (attribute.take(), input.parse::<LitStr>()?),
                    );
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

    let (attribute, string): (Vec<_>, Vec<_>) = symbols.values().cloned().unzip();
    let ident: Vec<_> = symbols.keys().collect();
    let symbols: Vec<_> = symbols.iter().collect();
    let (keywords, operators): (Vec<_>, Vec<_>) = symbols.iter().partition(|(_, (_, string))| {
        // Note this doesn't handle the case of keywords with digits
        // which doesn't currently occur.
        string
            .value()
            .chars()
            .all(|char| char.is_ascii_alphabetic() || char == '_')
    });

    let identifier_match: Vec<_> = keywords
        .iter()
        .map(|(symbol, (attr, string))| quote!(#attr #string => Symbol::#symbol,))
        .collect();

    let operator_array: Vec<_> = operators
        .iter()
        .map(|(symbol, (attr, string))| quote!(#attr { (Symbol::#symbol, #string) }))
        .collect();

    let output = quote! {
        /// A literal symbol, used for both words important to syntax (like while) and operators (like +)
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        #[non_exhaustive]
        pub enum Symbol {
            #(
                #[cfg_attr(feature = "serde", serde(rename = #string))]
                #[allow(missing_docs)]
                #attribute
                #ident,
            )*
        }

        impl<'a> fmt::Display for Symbol {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    #(#attribute Symbol::#ident => #string,)*
                }
                .fmt(formatter)
            }
        }

        impl FromStr for Symbol {
            type Err = ();

            fn from_str(string: &str) -> Result<Self, Self::Err> {
                Ok(match string {
                    #(#attribute #string => Symbol::#ident,)*
                    _ => return Err(()),
                })
            }
        }

        fn parse_keyword(identifier: &str) -> Option<Symbol> {
            Some(match identifier {
                #(#identifier_match)*
                _ => return None,
            })
        }

        trait ParseSymbol<'input> {
            fn parse_symbol(self, pos: usize) -> peg::RuleResult<Symbol>;
        }

        impl<'input> ParseSymbol<'input> for &'input str {
            fn parse_symbol(self: Self, pos: usize) -> peg::RuleResult<Symbol> {
                for (symbol, string) in &[#(#operator_array,)*] {
                    if self[pos..].starts_with(string) {
                        return peg::RuleResult::Matched(pos + string.len(), *symbol);
                    }
                }

                peg::RuleResult::Failed
            }
        }
    };

    output.into()
}
