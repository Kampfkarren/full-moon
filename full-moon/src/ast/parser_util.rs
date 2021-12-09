// Exported macros are documented since no amount of allow(missing_docs) silenced the lint

use super::punctuated::{Pair, Punctuated};
use crate::{
    node::Node,
    plugins::Plugin,
    tokenizer::TokenReference,
    visitors::{Visit, VisitMut},
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, fmt};

// This is cloned everywhere, so make sure cloning is as inexpensive as possible
#[derive(PartialEq)]
pub struct ParserState<'a, P: Plugin> {
    pub index: usize,
    pub len: usize,
    pub tokens: &'a [TokenReference],

    // This exists for the purpose of allowing the type system to easily infer P in parsers.
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=5ad9bac15ed2bcef716dfc0953c54f8f
    marker: std::marker::PhantomData<P>,
}

impl<'a, P: Plugin> ParserState<'a, P> {
    pub fn new(tokens: &'a [TokenReference]) -> Self {
        ParserState {
            index: 0,
            len: tokens.len(),
            tokens,

            marker: std::marker::PhantomData,
        }
    }

    pub fn advance(self) -> Option<Self> {
        if self.index + 1 == self.len {
            None
        } else {
            Some(ParserState {
                index: self.index + 1,
                ..self
            })
        }
    }

    pub fn peek(&self) -> &TokenReference {
        if self.index >= self.len {
            panic!("peek failed, when there should always be an eof");
        }

        self.tokens.get(self.index).expect("couldn't peek, no eof?")
    }
}

impl<'a, P: Plugin> fmt::Debug for ParserState<'a, P> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "ParserState {{ index: {}, current: {:?} }}",
            self.index,
            self.peek()
        )
    }
}

impl<'a, P: Plugin> Clone for ParserState<'a, P> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            len: self.len,
            tokens: self.tokens,
            marker: self.marker,
        }
    }
}

impl<'a, P: Plugin> Copy for ParserState<'a, P> {}

pub(crate) trait Parser<P: Plugin>: Sized {
    type Item;

    fn parse<'a>(
        &self,
        state: ParserState<'a, P>,
    ) -> Result<(ParserState<'a, P>, Self::Item), InternalAstError>;
}

#[doc(hidden)]
#[macro_export]
macro_rules! make_op {
    ($enum:ident, $(#[$outer:meta])* { $($operator:ident,)+ }) => {
        #[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        #[non_exhaustive]
        $(#[$outer])*
        #[display(fmt = "{}")]
        pub enum $enum {
            $(
                #[allow(missing_docs)]
                $operator(TokenReference),
            )+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! define_parser {
    ($parser:ty, $node:ty, |_, $state:ident| $body:expr) => {
        define_parser! {$parser, $node, |_, mut $state: ParserState<'a, P>| $body}
    };
    ($parser:ty, $node:ty, |$self:ident, $state:ident| $body:expr) => {
        define_parser! {$parser, $node, |$self:&$parser, mut $state: ParserState<'a, P>| $body}
    };
    ($parser:ty, $node:ty, $body:expr) => {
        impl<P: Plugin> Parser<P> for $parser {
            type Item = $node;

            #[allow(unused_mut)]
            fn parse<'a>(
                &self,
                state: ParserState<'a, P>,
            ) -> Result<(ParserState<'a, P>, $node), InternalAstError> {
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
            {
                let parser_result = $parser.parse($state).map(|(state, node)| (state, $constructor(node)));
                if parser_result != Err(InternalAstError::NoMatch) {
                    return parser_result;
                }
            }
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
                    token: $state.peek().clone(),
                    additional: None,
                });
            }
            Err(other) => return Err(other),
        }
    };

    ($state:ident, $parsed:expr, $error:tt) => {
        match $parsed {
            Ok((state, node)) => (state, node),
            Err(InternalAstError::NoMatch) => {
                return Err(InternalAstError::UnexpectedToken {
                    token: $state.peek().clone(),
                    additional: Some(Cow::from($error)),
                });
            }
            Err(other) => return Err(other),
        }
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
#[rustfmt::skip]
macro_rules! define_roblox_parser {
    ($parser:ident, $node:ty, $mock_ty:ty, |$self:ident, $state:ident| $body:expr) => {
        define_roblox_parser! ($parser, $node, $mock_ty, |$self:&$parser, mut $state: ParserState<'a, _>| $body);
    };
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

#[doc(hidden)]
#[macro_export]
macro_rules! define_lua52_parser {
    ($parser:ident, $node:ty, $mock_ty:ty, $body:expr) => {
        cfg_if::cfg_if! {
            if #[cfg(feature = "lua52")] {
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
pub enum InternalAstError {
    NoMatch,
    UnexpectedToken {
        token: TokenReference,
        additional: Option<Cow<'static, str>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct ZeroOrMore<ItemParser>(pub ItemParser);

impl<P, ItemParser, T> Parser<P> for ZeroOrMore<ItemParser>
where
    P: Plugin,
    ItemParser: Parser<P, Item = T>,
{
    type Item = Vec<T>;

    fn parse<'a>(
        &self,
        mut state: ParserState<'a, P>,
    ) -> Result<(ParserState<'a, P>, Vec<T>), InternalAstError> {
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
impl<P, ItemParser, Delimiter, T> Parser<P> for ZeroOrMoreDelimited<ItemParser, Delimiter>
where
    ItemParser: Parser<P, Item = T>,
    Delimiter: Parser<P, Item = TokenReference>,
    P: Plugin,
    T: Node + Visit + VisitMut,
{
    type Item = Punctuated<T>;

    fn parse<'a>(
        &self,
        mut state: ParserState<'a, P>,
    ) -> Result<(ParserState<'a, P>, Punctuated<T>), InternalAstError> {
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
                            token: state.peek().clone(),
                            additional: Some(Cow::from("trailing character")),
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
impl<P, ItemParser, Delimiter, T> Parser<P> for OneOrMore<ItemParser, Delimiter>
where
    ItemParser: Parser<P, Item = T>,
    Delimiter: Parser<P, Item = TokenReference>,
    P: Plugin,
    T: Node + Visit + VisitMut,
{
    type Item = Punctuated<ItemParser::Item>;

    fn parse<'a>(
        &self,
        state: ParserState<'a, P>,
    ) -> Result<(ParserState<'a, P>, Punctuated<ItemParser::Item>), InternalAstError> {
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

define_parser!(NoDelimiter, (), |_, state| Ok((state, ())));
