use std::{fmt, str::FromStr};

use serde::{Deserialize, Serialize};

macro_rules! symbols {
    ({
		$(
			$(#[$attribute:meta])?
			$name:ident => $string:expr,
		)+
    }, {
        $(
            $(#[$operator_attribute:meta])?
			$operator_name:ident => $operator_string:expr,
        )+
	}) => {
		/// A literal symbol, used for both words important to syntax (like while) and operators (like +)
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        #[non_exhaustive]
        pub enum Symbol {
            $(
                #[cfg_attr(feature = "serde", serde(rename = $string))]
                #[allow(missing_docs)]
                $(#[$attribute])?
                $name,
            )+

            $(
                #[cfg_attr(feature = "serde", serde(rename = $operator_string))]
                #[allow(missing_docs)]
                $(#[$operator_attribute])?
                $operator_name,
            )+
        }

		impl<'a> fmt::Display for Symbol {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    $($(#[$attribute])? Symbol::$name => $string,)+
                    $($(#[$operator_attribute])? Symbol::$operator_name => $operator_string,)+
                }
                .fmt(formatter)
            }
        }

        impl FromStr for Symbol {
            type Err = ();

            fn from_str(string: &str) -> Result<Self, Self::Err> {
                Ok(match string {
                    $($(#[$attribute])? $string => Symbol::$name,)*
                    $($(#[$operator_attribute])? $operator_string => Symbol::$operator_name,)*
                    _ => return Err(()),
                })
            }
        }

        pub fn parse_keyword(identifier: &str) -> Option<Symbol> {
            Symbol::from_str(identifier).ok()
        }

        pub trait ParseSymbol<'input> {
            fn parse_symbol(self, pos: usize) -> peg::RuleResult<Symbol>;
        }

        impl<'input> ParseSymbol<'input> for &'input str {
            fn parse_symbol(self: Self, pos: usize) -> peg::RuleResult<Symbol> {
				$(
					$(#[$operator_attribute])?
					if self[pos..].starts_with($operator_string) {
						return peg::RuleResult::Matched(pos + $operator_string.len(), Symbol::$operator_name);
					}
				)+

                peg::RuleResult::Failed
            }
        }
	};
}

symbols!({
    And => "and",
    Break => "break",
    Do => "do",
    ElseIf => "elseif",
    Else => "else",
    End => "end",
    False => "false",
    For => "for",
    Function => "function",
    If => "if",
    In => "in",
    Local => "local",
    Nil => "nil",
    Not => "not",
    Or => "or",
    Repeat => "repeat",
    Return => "return",
    Then => "then",
    True => "true",
    Until => "until",
    While => "while",
    #[cfg(feature = "lua52")]
    Goto => "goto",
}, {
    #[cfg(feature = "roblox")]
    PlusEqual => "+=",
    MinusEqual => "-=",
    StarEqual => "*=",
    SlashEqual => "/=",
    PercentEqual => "%=",
    CaretEqual => "^=",
    TwoDotsEqual => "..=",
    #[cfg(feature = "roblox")]
    Ampersand => "&",
    #[cfg(feature = "roblox")]
    ThinArrow => "->",
    #[cfg(any(feature = "roblox", feature = "lua52"))]
    TwoColons => "::",
    Caret => "^",
    Colon => ":",
    Comma => ",",
    Ellipse => "...",
    TwoDots => "..",
    Dot => ".",
    TwoEqual => "==",
    Equal => "=",
    GreaterThanEqual => ">=",
    GreaterThan => ">",
    Hash => "#",
    LeftBrace => "{",
    LeftBracket => "[",
    LeftParen => "(",
    LessThanEqual => "<=",
    LessThan => "<",
    Minus => "-",
    Percent => "%",
    #[cfg(feature = "roblox")]
    Pipe => "|",
    Plus => "+",
    #[cfg(feature = "roblox")]
    QuestionMark => "?",
    RightBrace => "}",
    RightBracket => "]",
    RightParen => ")",
    Semicolon => ";",
    Slash => "/",
    Star => "*",
    TildeEqual => "~=",
});
