use crate::{
    ast::Ast,
    tokenizer::{Position, Token, TokenKind, TokenReference},
    visitors::VisitorMut,
};

#[derive(Default)]
struct UpdatePositionsRewriter {
    start_position: Position,
    next_is_new_line: bool,
}

impl UpdatePositionsRewriter {
    fn update_token<'ast>(&mut self, token: &Token<'ast>) -> Token<'ast> {
        let display = token.to_string();

        let mut lines = bytecount::count(&display.as_bytes(), b'\n');
        if token.token_kind() == TokenKind::Whitespace {
            lines = lines.saturating_sub(1);
        }

        let end_position = if token.token_kind() == TokenKind::Eof {
            self.start_position
        } else {
            let mut end_position = Position {
                bytes: self.start_position.bytes() + display.len(),
                line: self.start_position.line() + lines,
                character: {
                    let offset = display.lines().last().unwrap_or("").chars().count();
                    if lines > 0 || self.next_is_new_line {
                        offset + 1
                    } else {
                        self.start_position.character() + offset
                    }
                },
            };

            if self.next_is_new_line {
                end_position.line += 1;
                self.next_is_new_line = false;
            }

            end_position
        };

        if display.ends_with('\n') {
            self.next_is_new_line = true;
        }

        self.start_position = end_position;
        Token {
            start_position: self.start_position,
            end_position,
            token_type: token.token_type.to_owned(),
        }
    }
}

impl<'ast> VisitorMut<'ast> for UpdatePositionsRewriter {
    fn visit_token(&mut self, token: TokenReference<'ast>) -> TokenReference<'ast> {
        TokenReference::new(
            token
                .leading_trivia()
                .map(|token| self.update_token(token))
                .collect(),
            self.update_token(token.token()),
            token
                .trailing_trivia()
                .map(|token| self.update_token(token))
                .collect(),
        )
    }
}

impl Ast<'_> {
    /// Will update the positions of all the tokens in the tree
    /// Necessary if you are both mutating the tree and need the positions of the tokens
    pub fn update_positions(self) -> Self {
        let mut rewriter = UpdatePositionsRewriter {
            start_position: Position {
                bytes: 0,
                character: 1,
                line: 1,
            },

            ..Default::default()
        };

        rewriter.visit_ast(self)
    }
}
