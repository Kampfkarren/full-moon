use crate::{
    tokenizer::{TokenType, TokenizerError, TokenizerErrorType},
    ShortString,
};

use super::{InterpolatedStringKind, Lexer, LexerResult, Position, Token};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BraceType {
    InterpolatedString,
    Normal,
}

pub(crate) fn read_interpolated_string_section(
    lexer: &mut Lexer,
    start_position: Position,
    format_type: InterpolatedStringKind,
    end_type: InterpolatedStringKind,
) -> LexerResult<Token> {
    let mut characters = Vec::new();
    let mut escape = false;
    let mut z_escaped = false;

    while let Some(character) = lexer.source.current() {
        if !escape && !z_escaped && matches!(character, '\n' | '\r') {
            break;
        }

        assert_eq!(character, lexer.source.next().unwrap());

        match (escape, character) {
            (true, 'z') => {
                escape = true;
                z_escaped = true;
                characters.push('z');
            }

            (true, ..) => {
                characters.push(character);
                z_escaped = true; // support for '\' followed by a new line
                escape = false;
            }

            // Make sure \u{1234} doesn't get treated as \u, followed by {1234}
            (false, '\\') if lexer.source.consume('u') => {
                characters.push('\\');
                characters.push('u');

                if lexer.source.consume('{') {
                    characters.push('{');

                    while lexer.source.peek().is_some() && lexer.source.peek() != Some('}') {
                        characters.push(lexer.source.next().unwrap());
                    }
                }
            }

            (false, '\\') => {
                characters.push(character);
                escape = true;
            }

            (false, '\n' | '\r') if z_escaped => {
                z_escaped = false;
                characters.push(character);
            }

            (false, ..) if character == '{' => {
                lexer.brace_stack.push(BraceType::InterpolatedString);
                return LexerResult::Ok(Token {
                    token_type: TokenType::InterpolatedString {
                        literal: ShortString::from_iter(characters),
                        kind: format_type,
                    },

                    start_position,
                    end_position: lexer.source.position(),
                });
            }

            (false, ..) if character == '`' => {
                return LexerResult::Ok(Token {
                    token_type: TokenType::InterpolatedString {
                        literal: ShortString::from_iter(characters),
                        kind: end_type,
                    },

                    start_position,
                    end_position: lexer.source.position(),
                });
            }

            _ => {
                characters.push(character);
            }
        }
    }

    LexerResult::Recovered(
        Token {
            token_type: TokenType::InterpolatedString {
                literal: ShortString::from_iter(characters),
                kind: end_type,
            },

            start_position,
            end_position: lexer.source.position(),
        },
        vec![TokenizerError {
            error: TokenizerErrorType::UnclosedString,
            range: (start_position, lexer.source.position()),
        }],
    )
}
