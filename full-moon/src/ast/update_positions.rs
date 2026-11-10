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
    fn update_token(&mut self, token: &Token) -> Token {
        let display = token.to_string();

        let mut end_position = self.start_position;

        if token.token_kind() != TokenKind::Eof {
            for character in display.chars() {
                if self.next_is_new_line {
                    self.next_is_new_line = false;
                    end_position.line += 1;
                    end_position.character = 1;
                }

                if character == '\n' {
                    self.next_is_new_line = true;
                } else {
                    end_position.character += 1;
                }

                end_position.bytes += character.len_utf8();
            }
        }

        let result = Token {
            start_position: self.start_position,
            end_position,
            token_type: token.token_type.to_owned(),
        };

        if self.next_is_new_line {
            self.next_is_new_line = false;
            end_position.line += 1;
            end_position.character = 1;
        }

        self.start_position = end_position;

        result
    }
}

impl VisitorMut for UpdatePositionsRewriter {
    fn visit_token_reference(&mut self, token: TokenReference) -> TokenReference {
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

    /// `Ast::tokens()` ends with the EOF token, so it needs a position like any other.
    /// Left alone, it keeps the stale one it was parsed at.
    fn visit_eof(&mut self, eof: TokenReference) -> TokenReference {
        self.visit_token_reference(eof)
    }
}

impl Ast {
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

#[cfg(test)]
mod tests {
    use crate::{
        node::Node,
        parse,
        tokenizer::{Token, TokenReference, TokenType},
        visitors::VisitorMut,
    };
    use pretty_assertions::assert_eq;

    /// Lengthens every identifier, so positions from the parsed text no longer describe it.
    struct Lengthen;

    impl VisitorMut for Lengthen {
        fn visit_token_reference(&mut self, token: TokenReference) -> TokenReference {
            if token.token_kind() != crate::tokenizer::TokenKind::Identifier {
                return token;
            }

            let longer = format!("{}_longer", token.token());

            TokenReference::new(
                token.leading_trivia().cloned().collect(),
                Token::new(TokenType::Identifier {
                    identifier: longer.as_str().into(),
                }),
                token.trailing_trivia().cloned().collect(),
            )
        }
    }

    #[test]
    fn test_update_positions_validity() {
        let ast = parse("local foo = 1\nlocal bar = 2").unwrap();
        let old_positions: Vec<_> = ast
            .tokens()
            .map(|token| (token.start_position(), token.end_position()))
            .collect();
        let ast = ast.update_positions();
        assert_eq!(
            old_positions,
            ast.tokens()
                .map(|token| (token.start_position(), token.end_position()))
                .collect::<Vec<_>>(),
        );
    }

    /// The EOF position is rebuilt too: left as parsed it pointed into the old text.
    #[test]
    fn test_update_positions_moves_eof() {
        let ast = Lengthen.visit_ast(parse("local x = 1 -- trailing\n").unwrap());
        let text = ast.to_string();
        let ast = ast.update_positions();

        let eof = ast
            .eof()
            .start_position()
            .expect("eof has a position")
            .bytes();
        assert_eq!(eof, text.len());

        let last = ast.tokens().next_back().expect("tokens() is never empty");
        assert_eq!(
            last.start_position().expect("token has a position").bytes(),
            text.len()
        );
    }
}
