// Exported macros are documented since no amount of allow(missing_docs) silenced the lint

use super::punctuated::{Pair, Punctuated};
use crate::{
    node::Node,
    tokenizer::TokenReference,
    visitors::{Visit, VisitMut},
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, fmt};

// This is cloned everywhere, so make sure cloning is as inexpensive as possible
#[derive(Clone, Copy)]
pub struct ParserState<'a> {
    pub index: usize,
    pub len: usize,
    pub tokens: *const TokenReference<'a>,
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &[TokenReference<'a>]) -> ParserState<'a> {
        ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        }
    }

    pub fn advance(self) -> Option<ParserState<'a>> {
        if self.index + 1 == self.len {
            None
        } else {
            Some(ParserState {
                index: self.index + 1,
                ..self
            })
        }
    }

    pub fn peek(&self) -> Cow<'a, TokenReference<'a>> {
        if self.index >= self.len {
            panic!("peek failed, when there should always be an eof");
        }

        let result = unsafe {
            &*self
                .tokens
                .add(self.index)
                .as_ref()
                .expect("couldn't peek, no eof?")
        };

        Cow::Borrowed(result)
    }
}

impl<'a> fmt::Debug for ParserState<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "ParserState {{ index: {}, current: {:?} }}",
            self.index,
            self.peek()
        )
    }
}

pub(crate) trait Parser<'a>: Sized {
    type Item;

    fn parse(
        &self,
        state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Self::Item), InternalAstError<'a>>;
}

#[doc(hidden)]
#[macro_export]
macro_rules! define_parser {
    ($parser:ident, $node:ty, $body:expr) => {
        impl<'a> Parser<'a> for $parser {
            type Item = $node;

            fn parse(
                &self,
                state: ParserState<'a>,
            ) -> Result<(ParserState<'a>, $node), InternalAstError<'a>> {
                $body(self, state)
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! parse_first_of {
    ($state:ident, {$($(@#[$meta:meta])? $parser:expr => $constructor:expr,)+}) => ({
        $(
            $(#[$meta])?
            match $parser.parse($state) {
                Ok((state, node)) => return Ok((state, $constructor(node.into()))),
                Err(InternalAstError::NoMatch) => {},
                Err(other) => return Err(other),
            };
        )+

        Err(InternalAstError::NoMatch)
    });
}

#[doc(hidden)]
#[macro_export]
macro_rules! expect {
    ($state:ident, $parsed:expr) => {
        match $parsed {
            Ok((state, node)) => (state, node),
            Err(InternalAstError::NoMatch) => {
                return Err(InternalAstError::UnexpectedToken {
                    token: $state.peek().into_owned(),
                    additional: None,
                });
            }
            Err(other) => return Err(other),
        };
    };

    ($state:ident, $parsed:expr, $error:tt) => {
        match $parsed {
            Ok((state, node)) => (state, node),
            Err(InternalAstError::NoMatch) => {
                return Err(InternalAstError::UnexpectedToken {
                    token: $state.peek().into_owned(),
                    additional: Some($error),
                });
            }
            Err(other) => return Err(other),
        };
    };
}

// This name is bad
#[doc(hidden)]
#[macro_export]
macro_rules! keep_going {
    ($parsed:expr) => {
        match $parsed {
            Ok((state, node)) => Ok((state, node)),
            Err(InternalAstError::NoMatch) => Err(InternalAstError::NoMatch),
            Err(other) => return Err(other),
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! define_roblox_parser {
    ($parser:ident, $node:ty, $mock_ty:ty, $body:expr) => {
        cfg_if::cfg_if! {
            if #[cfg(feature = "roblox")] {
                define_parser!($parser, $node, $body);
            } else {
                define_parser!($parser, $mock_ty, |_, _| {
                    Err(InternalAstError::NoMatch)
                });
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum InternalAstError<'a> {
    NoMatch,
    UnexpectedToken {
        #[cfg_attr(feature = "serde", serde(borrow))]
        token: TokenReference<'a>,
        additional: Option<&'a str>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct ZeroOrMore<P>(pub P);

impl<'a, P, T> Parser<'a> for ZeroOrMore<P>
where
    P: Parser<'a, Item = T>,
{
    type Item = Vec<T>;

    fn parse(
        &self,
        mut state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Vec<T>), InternalAstError<'a>> {
        let mut nodes = Vec::new();
        loop {
            match self.0.parse(state) {
                Ok((new_state, node)) => {
                    state = new_state;
                    nodes.push(node);
                }
                Err(InternalAstError::NoMatch) => break,
                Err(other) => return Err(other),
            };
        }
        Ok((state, nodes))
    }
}

macro_rules! test_pairs_logic {
    ($nodes:expr, $cause:expr) => {
        if cfg!(debug_assertions) {
            let len = $nodes.len();
            for (index, node) in $nodes.pairs().enumerate() {
                if index + 1 == len && node.punctuation().is_some() {
                    panic!(
                        "{} pairs illogical: last node has punctuation: {:?}",
                        $cause,
                        node.punctuation().unwrap()
                    );
                } else if index + 1 != len && node.punctuation().is_none() {
                    panic!(
                        "{} pairs illogical: non-last node ({}) has no punctuation",
                        $cause, index
                    );
                }
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct ZeroOrMoreDelimited<ItemParser, Delimiter>(
    pub ItemParser, // What items to parse, what is actually returned in a vec
    pub Delimiter,  // Delimiter parser between one item and another
    pub bool,       // Allow trailing delimiter?
);

// False positive clippy lints
#[allow(clippy::block_in_if_condition_stmt)]
#[allow(clippy::nonminimal_bool)]
impl<'a, ItemParser, Delimiter, T> Parser<'a> for ZeroOrMoreDelimited<ItemParser, Delimiter>
where
    ItemParser: Parser<'a, Item = T>,
    Delimiter: Parser<'a, Item = Cow<'a, TokenReference<'a>>>,
    T: Node + Visit<'a> + VisitMut<'a>,
{
    type Item = Punctuated<'a, T>;

    fn parse(
        &self,
        mut state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Punctuated<'a, T>), InternalAstError<'a>> {
        let mut nodes = Punctuated::new();

        if let Ok((new_state, node)) = keep_going!(self.0.parse(state)) {
            state = new_state;
            nodes.push(Pair::End(node));
        } else {
            return Ok((state, Punctuated::new()));
        }

        while let Ok((new_state, delimiter)) = keep_going!(self.1.parse(state)) {
            let last_value = nodes.pop().unwrap().into_value();
            nodes.push(Pair::Punctuated(last_value, delimiter));

            state = new_state;

            match self.0.parse(state) {
                Ok((new_state, node)) => {
                    state = new_state;
                    nodes.push(Pair::End(node));
                }

                Err(InternalAstError::NoMatch) => {
                    if self.2 {
                        break;
                    } else {
                        return Err(InternalAstError::UnexpectedToken {
                            token: state.peek().into_owned(),
                            additional: Some("trailing character"),
                        });
                    }
                }

                Err(other) => {
                    return Err(other);
                }
            }
        }

        if !self.2 {
            test_pairs_logic!(nodes, "ZeroOrMoreDelimited");
        }

        Ok((state, nodes))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct OneOrMore<ItemParser, Delimiter>(
    pub ItemParser, // What items to parse, what is actually returned in a vec
    pub Delimiter,  // Delimiter parser between one item and another
    pub bool,       // Allow trailing delimiter?
);

// False positive clippy lints
#[allow(clippy::block_in_if_condition_stmt)]
#[allow(clippy::nonminimal_bool)]
impl<'a, ItemParser, Delimiter: Parser<'a>, T> Parser<'a> for OneOrMore<ItemParser, Delimiter>
where
    ItemParser: Parser<'a, Item = T>,
    Delimiter: Parser<'a, Item = Cow<'a, TokenReference<'a>>>,
    T: Node + Visit<'a> + VisitMut<'a>,
{
    type Item = Punctuated<'a, ItemParser::Item>;

    fn parse(
        &self,
        state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Punctuated<'a, ItemParser::Item>), InternalAstError<'a>> {
        let mut nodes = Punctuated::new();
        let (mut state, node) = self.0.parse(state)?;
        nodes.push(Pair::End(node));

        while let Ok((new_state, delimiter)) = self.1.parse(state) {
            let last_value = nodes.pop().unwrap().into_value();
            nodes.push(Pair::Punctuated(last_value, delimiter));

            match self.0.parse(new_state) {
                Ok((new_state, node)) => {
                    state = new_state;
                    nodes.push(Pair::End(node));
                }

                Err(InternalAstError::NoMatch) => {
                    if self.2 {
                        state = new_state;
                    }

                    break;
                }

                Err(other) => {
                    return Err(other);
                }
            }
        }

        if !self.2 {
            let last_value = nodes.pop().unwrap().into_value();
            nodes.push(Pair::End(last_value));

            test_pairs_logic!(nodes, "OneOrMore");
        }

        Ok((state, nodes))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NoDelimiter;

define_parser!(NoDelimiter, (), |_, state: ParserState<'a>| Ok((state, ())));
