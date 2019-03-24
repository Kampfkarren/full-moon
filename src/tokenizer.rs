use lazy_static::lazy_static;
use regex::Regex;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TokenizerError {
    UnexpectedToken(char),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum TokenType<'a> {
    Identifier {
        #[cfg_attr(feature = "serde", serde(borrow))]
        identifier: Cow<'a, str>,
    },

    SingleLineComment {
        #[cfg_attr(feature = "serde", serde(borrow))]
        comment: Cow<'a, str>,
    },

    Whitespace {
        #[cfg_attr(feature = "serde", serde(borrow))]
        characters: Cow<'a, str>,
    },
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
            Identifier { identifier } => identifier.to_string(),
            SingleLineComment { comment } => format!("--{}", comment),
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

lazy_static! {
    static ref PATTERN_SINGLE_LINE_COMMENT: Regex = Regex::new(r"--(.+)").unwrap();
    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"[^\W\d]+\w*").unwrap();
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"(^[^\S\n]+\n?|\n)").unwrap();
}

type Advancement<'a> = Result<Option<TokenAdvancement<'a>>, TokenizerError>;

macro_rules! advance_regex {
    ($code:ident, $regex:ident, $token_type:ident.$field:ident) => {
        if let Some(find) = $regex.find($code) {
            if find.start() != 0 {
                Ok(None)
            } else {
                Ok(Some(TokenAdvancement {
                    advance: find.end() - find.start(),
                    token_type: TokenType::$token_type {
                        $field: Cow::from(find.as_str()),
                    },
                }))
            }
        } else {
            Ok(None)
        }
    };
}

fn advance_comment(code: &str) -> Advancement {
    if let Some(find) = PATTERN_SINGLE_LINE_COMMENT.find(code) {
        if find.start() == 0 {
            return Ok(Some(TokenAdvancement {
                advance: find.end() - find.start(),
                token_type: TokenType::SingleLineComment {
                    comment: Cow::from(&find.as_str()[2..]),
                },
            }));
        }
    }

    // TODO: Multi-line comment

    Ok(None)
}

fn advance_identifier(code: &str) -> Advancement {
    advance_regex!(code, PATTERN_IDENTIFIER, Identifier.identifier)
}

// Keep finding whitespace until the line ends
fn advance_whitespace(code: &str) -> Advancement {
    advance_regex!(code, PATTERN_WHITESPACE, Whitespace.characters)
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
        advance!(advance_identifier);

        return Err(TokenizerError::UnexpectedToken(
            code.chars()
                .nth(position.bytes)
                .expect("text overflow while giving unexpected token error"),
        ));
    }

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
}
