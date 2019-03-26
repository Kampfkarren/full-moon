use lazy_static::lazy_static;
use regex::{self, Regex};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

macro_rules! symbols {
    ($($ident:ident => $string:tt,)+) => {
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        pub enum Symbol {
            $(
                #[cfg_attr(feature = "serde", serde(rename = $string))]
                $ident,
            )+
        }

        impl<'a> fmt::Display for Symbol {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    $(Symbol::$ident => $string,)+
                }
                .fmt(formatter)
            }
        }

        impl FromStr for Symbol {
            type Err = ();

            fn from_str(string: &str) -> Result<Self, Self::Err> {
                Ok(match string {
                    $($string => Symbol::$ident,)+
                    _ => Err(())?,
                })
            }
        }

        lazy_static! {
            static ref PATTERN_SYMBOL: Regex = Regex::new(
                &vec![$($string,)+]
                    .iter()
                    .map(|x| {
                        regex::escape(&x.to_string())
                    })
                    .collect::<Vec<_>>()
                    .join("|")
            ).unwrap();
        }
    };
}

symbols!(
    And => "and",
    Break => "break",
    Do => "do",
    Else => "else",
    ElseIf => "elseif",
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

    Caret => "^",
    Colon => ":",
    Comma => ",",
    Dot => ".",
    Ellipse => "...",
    Equal => "=",
    TwoEqual => "==",
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
    Plus => "+",
    RightBrace => "}",
    RightBracket => "]",
    RightParen => ")",
    Semicolon => ";",
    Slash => "/",
    Star => "*",
    TildeEqual => "~=",
    TwoDots => "..",
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TokenizerError {
    UnclosedComment,
    UnclosedString,
    UnexpectedToken(char),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum TokenType<'a> {
    Eof,

    Identifier {
        #[cfg_attr(feature = "serde", serde(borrow))]
        identifier: Cow<'a, str>,
    },

    MultiLineComment {
        blocks: usize,
        #[cfg_attr(feature = "serde", serde(borrow))]
        comment: Cow<'a, str>,
    },

    Number {
        #[cfg_attr(feature = "serde", serde(borrow))]
        text: Cow<'a, str>,
    },

    SingleLineComment {
        #[cfg_attr(feature = "serde", serde(borrow))]
        comment: Cow<'a, str>,
    },

    StringLiteral {
        #[cfg_attr(feature = "serde", serde(borrow))]
        literal: Cow<'a, str>,
        quote_type: StringLiteralQuoteType,
    },

    Symbol {
        symbol: Symbol,
    },

    Whitespace {
        #[cfg_attr(feature = "serde", serde(borrow))]
        characters: Cow<'a, str>,
    },
}

impl<'a> TokenType<'a> {
    pub fn ignore(&self) -> bool {
        match self {
            TokenType::SingleLineComment { .. }
            | TokenType::MultiLineComment { .. }
            | TokenType::Whitespace { .. } => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Token<'a> {
    pub start_position: Position,
    pub end_position: Position,
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub token_type: TokenType<'a>,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenType::*;

        match &self.token_type {
            Eof => "".to_string(),
            Number { text } => text.to_string(),
            Identifier { identifier } => identifier.to_string(),
            MultiLineComment { blocks, comment } => format!("--[{0}[{1}]{0}]", "=".repeat(*blocks), comment),
            SingleLineComment { comment } => format!("--{}", comment),
            StringLiteral {
                literal,
                quote_type,
            } => format!("{0}{1}{0}", quote_type.to_string(), literal.to_string()),
            Symbol { symbol } => symbol.to_string(),
            Whitespace { characters } => characters.to_string(),
        }
        .fmt(formatter)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Position {
    pub bytes: usize,
    pub character: usize,
    pub line: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenAdvancement<'a> {
    pub advance: usize,
    pub token_type: TokenType<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum StringLiteralQuoteType {
    Double,
    Single,
}

impl<'a> fmt::Display for StringLiteralQuoteType {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StringLiteralQuoteType::Double => "\"",
            StringLiteralQuoteType::Single => "'",
        }
        .fmt(formatter)
    }
}

lazy_static! {
    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"[^\W\d]+\w*").unwrap();
    static ref PATTERN_NUMBER: Regex =
        Regex::new(r"^((-?0x[A-Fa-f\d]+)|(-?((\d*\.\d+)|(\d+))([eE]-?\d+)?))").unwrap();
    static ref PATTERN_COMMENT_MULTI_LINE_BEGIN: Regex = Regex::new(r"--\[(=*)\[").unwrap();
    static ref PATTERN_COMMENT_SINGLE_LINE: Regex = Regex::new(r"--(.+)").unwrap();
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"(^[^\S\n]+\n?|\n)").unwrap();
}

type Advancement<'a> = Result<Option<TokenAdvancement<'a>>, TokenizerError>;

macro_rules! advance_regex {
    ($code:expr, $regex:ident, $token_type:ident($find:ident) $block:tt) => {
        if let Some($find) = $regex.find($code) {
            if $find.start() != 0 {
                Ok(None)
            } else {
                Ok(Some(TokenAdvancement {
                    advance: $find.end() - $find.start(),
                    token_type: TokenType::$token_type $block,
                }))
            }
        } else {
            Ok(None)
        }
    };
}

fn advance_comment(code: &str) -> Advancement {
    if let Some(captures) = PATTERN_COMMENT_MULTI_LINE_BEGIN.captures(code) {
        let whole_beginning = captures.get(0).unwrap();
        if whole_beginning.start() == 0 {
            let block_count = match captures.get(1) {
                Some(block_count) => block_count.end() - block_count.start(),
                None => 0,
            };

            let end_regex = Regex::new(&format!(r"\]={{{}}}", block_count)).unwrap();

            let end_find = match end_regex.find(code) {
                Some(find) => find,
                None => return Err(TokenizerError::UnclosedComment),
            };

            return Ok(Some(TokenAdvancement {
                advance: end_find.end() + 1,
                token_type: TokenType::MultiLineComment {
                    blocks: block_count,
                    comment: Cow::from(&code[whole_beginning.end()..end_find.start()]),
                },
            }));
        }
    } else if let Some(find) = PATTERN_COMMENT_SINGLE_LINE.find(code) {
        if find.start() == 0 {
            return Ok(Some(TokenAdvancement {
                advance: find.end(),
                token_type: TokenType::SingleLineComment {
                    comment: Cow::from(&find.as_str()[2..]),
                },
            }));
        }
    }

    Ok(None)
}

fn advance_number(code: &str) -> Advancement {
    advance_regex!(code, PATTERN_NUMBER, Number(find) {
        text: Cow::from(find.as_str()),
    })
}

fn advance_identifier(code: &str) -> Advancement {
    advance_regex!(code, PATTERN_IDENTIFIER, Identifier(find) {
        identifier: Cow::from(find.as_str()),
    })
}

fn advance_quote(code: &str) -> Advancement {
    let quote = if code.starts_with("\"") {
        '"'
    } else if code.starts_with("'") {
        '\''
    } else {
        return Ok(None);
    };

    let mut end = None;
    let mut escape = false;

    for (index, character) in code.char_indices().skip(1) {
        if character == '\\' {
            escape = !escape;
        } else if character == quote {
            if escape {
                escape = false;
            } else {
                end = Some(index);
                break;
            }
        } else if character == '\r' || character == '\n' {
            return Err(TokenizerError::UnclosedString);
        } else {
            escape = false;
        }
    }

    if let Some(end) = end {
        Ok(Some(TokenAdvancement {
            advance: end + 1,
            token_type: TokenType::StringLiteral {
                literal: Cow::from(&code[1..end]),
                quote_type: match quote {
                    '"' => StringLiteralQuoteType::Double,
                    '\'' => StringLiteralQuoteType::Single,
                    _ => unreachable!(),
                },
            },
        }))
    } else {
        return Err(TokenizerError::UnclosedString);
    }
}

fn advance_symbol(code: &str) -> Advancement {
    if code.chars().next().unwrap().is_alphanumeric() {
        let identifier = PATTERN_IDENTIFIER.find(code).unwrap();
        let expected_len = identifier.end() - identifier.start();

        advance_regex!(&code[0..expected_len], PATTERN_SYMBOL, Symbol(find) {
            symbol: {
                if find.end() - find.start() == expected_len {
                    Symbol::from_str(find.as_str()).unwrap()
                } else {
                    return Ok(None)
                }
            }
        })
    } else {
        advance_regex!(code, PATTERN_SYMBOL, Symbol(find) {
            symbol: Symbol::from_str(find.as_str()).unwrap(),
        })
    }
}

// Keep finding whitespace until the line ends
fn advance_whitespace(code: &str) -> Advancement {
    advance_regex!(code, PATTERN_WHITESPACE, Whitespace(find) {
        characters: Cow::from(find.as_str()),
    })
}

pub fn tokens<'a>(code: &'a str) -> Result<Vec<Token<'a>>, TokenizerError> {
    let mut tokens = Vec::new();
    let mut position = Position {
        bytes: 0,
        character: 1,
        line: 1,
    };

    macro_rules! advance {
        ($function:ident) => {
            if let Some(advancement) = $function(&code[position.bytes..])? {
                let start_position = position;

                for _ in 0..advancement.advance {
                    if code.chars().nth(position.bytes).expect("text overflow") == '\n' {
                        position.line += 1;
                        position.character = 1;
                    } else {
                        position.character += 1;
                    }

                    position.bytes += 1;
                }

                tokens.push(Token {
                    start_position,
                    end_position: position,
                    token_type: advancement.token_type,
                });

                continue;
            }
        };
    }

    while code.len() > position.bytes {
        advance!(advance_whitespace);
        advance!(advance_comment);
        advance!(advance_number);
        advance!(advance_symbol);
        advance!(advance_identifier);
        advance!(advance_quote);

        return Err(TokenizerError::UnexpectedToken(
            code.chars()
                .nth(position.bytes)
                .expect("text overflow while giving unexpected token error"),
        ));
    }

    tokens.push(Token {
        start_position: position,
        end_position: position,
        token_type: TokenType::Eof,
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::*;
    use pretty_assertions::assert_eq;

    macro_rules! test_advancer {
        ($advancer:ident($code:tt), $result:expr) => {
            assert_eq!($advancer($code), $result);

            let result: Advancement = $result;
            match result {
                Ok(Some(token)) => {
                    let tokens = tokens($code).expect("couldn't tokenize");
                    let first_token = &tokens.get(0).expect("tokenized response is empty");
                    assert_eq!(first_token.token_type, token.token_type);
                }

                Err(err) => {
                    assert_eq!(tokens($code), Err(err));
                }

                _ => {}
            };
        };
    }

    #[test]
    fn test_advance_comment() {
        test_advancer!(
            advance_comment("-- hello world"),
            Ok(Some(TokenAdvancement {
                advance: 14,
                token_type: TokenType::SingleLineComment {
                    comment: Cow::from(" hello world"),
                },
            }))
        );
    }

    #[test]
    fn test_advance_numbers() {
        test_advancer!(
            advance_number("213"),
            Ok(Some(TokenAdvancement {
                advance: 3,
                token_type: TokenType::Number {
                    text: Cow::from("213"),
                },
            }))
        );

        test_advancer!(
            advance_number("123.45"),
            Ok(Some(TokenAdvancement {
                advance: 6,
                token_type: TokenType::Number {
                    text: Cow::from("123.45"),
                },
            }))
        );
    }

    #[test]
    fn test_advance_identifier() {
        test_advancer!(
            advance_identifier("hello"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Identifier {
                    identifier: Cow::from("hello"),
                },
            }))
        );

        test_advancer!(
            advance_identifier("hello world"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Identifier {
                    identifier: Cow::from("hello"),
                },
            }))
        );

        test_advancer!(
            advance_identifier("hello___"),
            Ok(Some(TokenAdvancement {
                advance: 8,
                token_type: TokenType::Identifier {
                    identifier: Cow::from("hello___"),
                },
            }))
        );

        test_advancer!(advance_identifier("123"), Ok(None));
    }

    #[test]
    fn test_advance_symbols() {
        test_advancer!(
            advance_symbol("local"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Symbol {
                    symbol: Symbol::Local
                },
            }))
        );
    }

    #[test]
    fn test_advance_whitespace() {
        test_advancer!(
            advance_whitespace("\t  \n"),
            Ok(Some(TokenAdvancement {
                advance: 4,
                token_type: TokenType::Whitespace {
                    characters: Cow::from("\t  \n"),
                },
            }))
        );

        test_advancer!(
            advance_whitespace("\thello"),
            Ok(Some(TokenAdvancement {
                advance: 1,
                token_type: TokenType::Whitespace {
                    characters: Cow::from("\t"),
                },
            }))
        );

        test_advancer!(
            advance_whitespace("\t\t\nhello"),
            Ok(Some(TokenAdvancement {
                advance: 3,
                token_type: TokenType::Whitespace {
                    characters: Cow::from("\t\t\n"),
                },
            }))
        );
    }

    #[test]
    fn test_advance_quote() {
        test_advancer!(
            advance_quote("\"hello\""),
            Ok(Some(TokenAdvancement {
                advance: 7,
                token_type: TokenType::StringLiteral {
                    literal: Cow::from("hello"),
                    quote_type: StringLiteralQuoteType::Double,
                },
            }))
        );

        test_advancer!(
            advance_quote("\"hello"),
            Err(TokenizerError::UnclosedString)
        );
    }

    #[test]
    fn test_symbols_within_symbols() {
        // "index" should not return "in"
        test_advancer!(advance_symbol("index"), Ok(None));

        // "<=" should not return "<"
        test_advancer!(
            advance_symbol("<="),
            Ok(Some(TokenAdvancement {
                advance: 2,
                token_type: TokenType::Symbol {
                    symbol: Symbol::LessThanEqual,
                },
            }))
        );
    }
}
