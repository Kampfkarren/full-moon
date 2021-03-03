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
#[derive(Clone, Copy, PartialEq)]
pub struct ParserState<'a, 'b> {
    pub index: usize,
    pub len: usize,
    pub tokens: &'b [TokenReference<'a>],
}

impl<'a, 'b> ParserState<'a, 'b> {
    pub fn new(tokens: &'b [TokenReference<'a>]) -> ParserState<'a, 'b> {
        ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens,
        }
    }

    pub fn advance(self) -> Option<ParserState<'a, 'b>> {
        if self.index + 1 == self.len {
            None
        } else {
            Some(ParserState {
                index: self.index + 1,
                ..self
            })
        }
    }

    // TODO: This is bad, containing both a mandatory clone on every call so that everything is
    // backwards compatible, since it SHOULD just borrow. It is only like this because of a failure
    // to tackle lifetimes.
    pub fn peek(&self) -> Cow<'a, TokenReference<'a>> {
        if self.index >= self.len {
            panic!("peek failed, when there should always be an eof");
        }

        let result = { self.tokens.get(self.index).expect("couldn't peek, no eof?") };

        Cow::Owned(result.to_owned())
    }

    pub(crate) fn take_if(
        self,
        pred: impl FnOnce(&TokenReference<'a>) -> bool,
    ) -> Result<(Self, TokenReference<'a>), InternalAstError<'a>> {
        if self.index + 1 == self.len {
            Err(InternalAstError::NoMatch)
        } else {
            let token = self.tokens.get(self.index).expect("inconsistent length");
            if pred(token) {
                Ok((
                    ParserState {
                        index: self.index + 1,
                        ..self
                    },
                    token.to_owned(),
                ))
            } else {
                Err(InternalAstError::NoMatch)
            }
        }
    }
}

impl<'a, 'b> fmt::Debug for ParserState<'a, 'b> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "ParserState {{ index: {}, current: {:?} }}",
            self.index,
            self.peek()
        )
    }
}

pub(crate) trait Parser<'a, 'b>{
    type Item;

    fn parse(
        &self,
        state: ParserState<'a, 'b>,
    ) -> Result<(ParserState<'a, 'b>, Self::Item), InternalAstError<'a>>;
}

#[doc(hidden)]
#[macro_export]
macro_rules! make_op {
    ($enum:ident, $(#[$outer:meta])* { $($operator:ident,)+ }) => {
        #[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        $(#[$outer])*
        #[display(fmt = "{}")]
        pub enum $enum<'a> {
            #[cfg_attr(feature = "serde", serde(borrow))]
            $(
                #[allow(missing_docs)]
                $operator(Cow<'a, TokenReference<'a>>),
            )+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! define_parser {
    ($parser:ident, $node:ty, $body:expr) => {
        impl<'a, 'b> Parser<'a, 'b> for $parser {
            type Item = $node;

            fn parse(
                &self,
                state: ParserState<'a, 'b>,
            ) -> Result<(ParserState<'a, 'b>, $node), InternalAstError<'a>> {
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

impl<'a, 'b, P, T> Parser<'a, 'b> for ZeroOrMore<P>
where
    P: Parser<'a, 'b, Item = T>,
{
    type Item = Vec<T>;

    fn parse(
        &self,
        mut state: ParserState<'a, 'b>,
    ) -> Result<(ParserState<'a, 'b>, Vec<T>), InternalAstError<'a>> {
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
#[allow(clippy::blocks_in_if_conditions)]
#[allow(clippy::nonminimal_bool)]
impl<'a, 'b, ItemParser, Delimiter, T> Parser<'a, 'b> for ZeroOrMoreDelimited<ItemParser, Delimiter>
where
    ItemParser: Parser<'a, 'b, Item = T>,
    Delimiter: Parser<'a, 'b, Item = Cow<'a, TokenReference<'a>>>,
    T: Node<'a> + Visit<'a> + VisitMut<'a>,
{
    type Item = Punctuated<'a, T>;

    fn parse(
        &self,
        mut state: ParserState<'a, 'b>,
    ) -> Result<(ParserState<'a, 'b>, Punctuated<'a, T>), InternalAstError<'a>> {
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
#[allow(clippy::blocks_in_if_conditions)]
#[allow(clippy::nonminimal_bool)]
impl<'a, 'b, ItemParser, Delimiter: Parser<'a, 'b>, T> Parser<'a, 'b> for OneOrMore<ItemParser, Delimiter>
where
    ItemParser: Parser<'a, 'b, Item = T>,
    Delimiter: Parser<'a, 'b, Item = Cow<'a, TokenReference<'a>>>,
    T: Node<'a> + Visit<'a> + VisitMut<'a>,
{
    type Item = Punctuated<'a, ItemParser::Item>;

    fn parse(
        &self,
        state: ParserState<'a, 'b>,
    ) -> Result<(ParserState<'a, 'b>, Punctuated<'a, ItemParser::Item>), InternalAstError<'a>> {
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

define_parser!(NoDelimiter, (), |_, state: ParserState<'a, 'b>| Ok((
    state,
    ()
)));
