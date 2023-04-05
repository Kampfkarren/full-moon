// rewrite todo: do a lot of tests for reasonable cases of recoverability (like local x = \n local y = 1)
// rewrite todo: test function() true end, i think it's not going to highlight "true" and i want it to.
// actually, it might but only because the last token isn't an eof. parser is going to have to throw out those tokens
// and then read another block and merge them or something? wow that sounds awful because it won't work inside if's. good luck
// maybe keep parsing until there's an `end`???? but then global blocks...aaa

use super::{
    parser_structs::{ParserResult, ParserState},
    punctuated::{Pair, Punctuated},
    span::ContainedSpan,
    Expression, FunctionBody, Parameter,
};
use crate::{
    ast, // rewrite todo: make everything use this
    node::Node,
    tokenizer::{Symbol, Token, TokenKind, TokenReference, TokenType},
};

pub fn parse_block(state: &mut ParserState) -> ParserResult<ast::Block> {
    let mut stmts = Vec::new();

    loop {
        match parse_stmt(state) {
            ParserResult::Value(stmt) => {
                // todo: parse semicolons
                stmts.push((stmt, None));
            }
            ParserResult::NotFound => break,
            ParserResult::LexerMoved => {
                if stmts.is_empty() {
                    return ParserResult::LexerMoved;
                } else {
                    break;
                }
            }
        }
    }

    // todo: parse last stmts
    ParserResult::Value(ast::Block {
        stmts,
        last_stmt: None,
    })
}

// Blocks in general are not very fallible. This means, for instance, not finishing `function()`
// will result in a completely ignored function body.
// This is an opinionated choice because I believe selene is going to produce terrible outputs if we don't.
// rewrite todo: I don't love that this only accepts one token. it should be range(s?)
fn parse_block_with_end(
    state: &mut ParserState,
    start_for_errors: &TokenReference,
) -> Result<(ast::Block, TokenReference), ()> {
    let block = match parse_block(state) {
        ParserResult::Value(block) => block,
        ParserResult::NotFound => unreachable!("parse_block should always return a value"),
        ParserResult::LexerMoved => return Err(()),
    };

    let end_token = match state.current() {
        ParserResult::Value(token) if token.is_symbol(Symbol::End) => state.consume().unwrap(),

        ParserResult::Value(_) | ParserResult::NotFound => {
            state.token_error(start_for_errors.clone(), "expected `end` to close block");
            return Err(());
        }

        ParserResult::LexerMoved => return Err(()),
    };

    Ok((block, end_token))
}

fn parse_stmt(state: &mut ParserState) -> ParserResult<ast::Stmt> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Local,
        } => {
            const BAD_TOKEN_ERROR: &str = "expected either a variable name or `function`";

            let local_token = state.consume().unwrap();
            let next_token = match state.current() {
                ParserResult::Value(token) => token,
                ParserResult::NotFound => {
                    state.token_error(local_token, BAD_TOKEN_ERROR);
                    return ParserResult::LexerMoved;
                }
                ParserResult::LexerMoved => return ParserResult::LexerMoved,
            };

            match next_token.token_type() {
                TokenType::Identifier { identifier } => {
                    ParserResult::Value(ast::Stmt::LocalAssignment(
                        try_parser!(expect_local_assignment(state, local_token)).unwrap(),
                    ))
                }

                _ => {
                    state.token_error(next_token.clone(), BAD_TOKEN_ERROR);

                    ParserResult::LexerMoved
                }
            }
        }

        TokenType::Symbol { symbol: Symbol::Do } => {
            let do_token = state.consume().unwrap();
            let (block, end_token) = match parse_block_with_end(state, &do_token) {
                Ok(block) => block,
                Err(()) => return ParserResult::LexerMoved,
            };

            ParserResult::Value(ast::Stmt::Do(ast::Do {
                do_token,
                block,
                end_token,
            }))
        }

        TokenType::Symbol {
            symbol: Symbol::Function,
        } => {
            let function_token = state.consume().unwrap();

            let function_declaration = match expect_function_declaration(state, function_token) {
                Ok(function_declaration) => function_declaration,
                Err(()) => return ParserResult::LexerMoved,
            };

            ParserResult::Value(ast::Stmt::FunctionDeclaration(function_declaration))
        }

        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        }
        | TokenType::Identifier { .. } => {
            // unwrap() because we're always starting on the right path
            // rewrite todo: i'm very skeptical of the fallibility of this
            let (prefix, suffixes) = try_parser!(parse_prefix_and_suffixes(state)).unwrap();

            let var = match suffixes.last() {
                Some(ast::Suffix::Call(call)) => {
                    return ParserResult::Value(ast::Stmt::FunctionCall(ast::FunctionCall {
                        prefix,
                        suffixes,
                    }));
                }

                Some(ast::Suffix::Index(_)) => todo!(),

                None => match prefix {
                    ast::Prefix::Name(name) => ast::Var::Name(name),
                    ast::Prefix::Expression(expr) => todo!("VarExpression?"),
                },
            };

            const ASSIGNMENT_NEVER_CAME_ERROR: &str =
                "unexpected expression when looking for a statement";

            let current = match state.current() {
                ParserResult::Value(token) => token,

                // rewrite todo: this isn't even really possible to hit because of eof. none of these are.
                // maybe it shouldn't be possible for current() to return this?
                ParserResult::NotFound => {
                    // rewrite todo: ranges for prefix
                    state.token_error(
                        var.tokens().last().unwrap().clone(),
                        ASSIGNMENT_NEVER_CAME_ERROR,
                    );

                    // rewrite todo: recoverability?
                    return ParserResult::LexerMoved;
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,
            };

            if !current.is_symbol(Symbol::Comma) && !current.is_symbol(Symbol::Equal) {
                state.token_error(current.clone(), ASSIGNMENT_NEVER_CAME_ERROR);
                return ParserResult::LexerMoved;
            }

            let mut var_list = Punctuated::new();
            var_list.push(Pair::End(var));

            // rewrite todo: unwrap() here because of eof, like referenced in the last comment
            while state.current().unwrap().is_symbol(Symbol::Comma) {
                let next_comma = state.consume().unwrap();

                let (next_prefix, next_suffixes) = match parse_prefix_and_suffixes(state) {
                    ParserResult::Value((prefix, suffixes)) => (prefix, suffixes),

                    ParserResult::LexerMoved => {
                        break;
                    }

                    ParserResult::NotFound => {
                        state.token_error(next_comma, "expected another variable");
                        break;
                    }
                };

                match next_suffixes.last() {
                    Some(ast::Suffix::Call(call)) => {
                        state.token_error(
                            call.tokens().last().unwrap().clone(),
                            "can't assign to the result of a call",
                        );
                        break;
                    }

                    Some(ast::Suffix::Index(_)) => {
                        let last_var = var_list.pop().unwrap().into_value();
                        var_list.push(Pair::Punctuated(last_var, next_comma));

                        var_list.push(Pair::End(ast::Var::Expression(Box::new(
                            ast::VarExpression {
                                prefix: next_prefix,
                                suffixes: next_suffixes,
                            },
                        ))))
                    }

                    None => match next_prefix {
                        ast::Prefix::Name(name) => {
                            let last_var = var_list.pop().unwrap().into_value();
                            var_list.push(Pair::Punctuated(last_var, next_comma));

                            var_list.push(Pair::End(ast::Var::Name(name)));
                        }

                        ast::Prefix::Expression(expr) => todo!("VarExpression?"),
                    },
                }
            }

            let equal_token = match state.current() {
                ParserResult::Value(token) if token.is_symbol(Symbol::Equal) => {
                    state.consume().unwrap()
                }

                ParserResult::Value(token) => {
                    state.token_error(token.clone(), "expected `=`");
                    return ParserResult::LexerMoved;
                }

                ParserResult::NotFound => {
                    unreachable!("rewrite todo: i'm now refusing to write these");
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,
            };

            let expr_list = match parse_expression_list(state) {
                ParserResult::Value(expr_list) => expr_list,

                ParserResult::NotFound => {
                    state.token_error(equal_token.clone(), "expected values to set to");
                    Punctuated::new()
                }

                ParserResult::LexerMoved => Punctuated::new(),
            };

            ParserResult::Value(ast::Stmt::Assignment(ast::Assignment {
                var_list,
                equal_token,
                expr_list,
            }))
        }

        _ => ParserResult::NotFound,
    }
}

fn expect_function_name(state: &mut ParserState) -> ParserResult<ast::FunctionName> {
    let mut names = Punctuated::new();

    let name = match state.current() {
        ParserResult::Value(token)
            if matches!(token.token_type(), TokenType::Identifier { .. }) =>
        {
            state.consume().unwrap()
        }

        ParserResult::Value(token) => {
            state.token_error(token.clone(), "expected function name");
            return ParserResult::NotFound;
        }

        ParserResult::NotFound => {
            unreachable!("rewrite todo: this can't be possible because of eof")
        }

        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    names.push(Pair::End(name));

    loop {
        let current = match state.current() {
            ParserResult::Value(token) => token,

            ParserResult::NotFound => {
                unreachable!("rewrite todo: this can't be possible because of eof")
            }

            ParserResult::LexerMoved => return ParserResult::LexerMoved,
        };

        let middle_token = if current.is_symbol(Symbol::Colon) || current.is_symbol(Symbol::Dot) {
            state.consume().unwrap()
        } else {
            break;
        };

        let name = match state.current() {
            ParserResult::Value(token)
                if matches!(token.token_type(), TokenType::Identifier { .. }) =>
            {
                state.consume().unwrap()
            }

            ParserResult::Value(token) => {
                state.token_error(
                    token.clone(),
                    format!("expected name after `{}`", middle_token.token()),
                );
                return ParserResult::NotFound;
            }

            ParserResult::NotFound => {
                unreachable!("rewrite todo: this can't be possible because of eof")
            }

            ParserResult::LexerMoved => {
                break;
            }
        };

        if middle_token.is_symbol(Symbol::Dot) {
            names.push(Pair::Punctuated(name, middle_token));
        } else if middle_token.is_symbol(Symbol::Colon) {
            return ParserResult::Value(ast::FunctionName {
                names,
                colon_name: Some((middle_token, name)),
            });
        } else {
            unreachable!();
        }
    }

    ParserResult::Value(ast::FunctionName {
        names,
        colon_name: None,
    })
}

fn expect_function_declaration(
    state: &mut ParserState,
    function_token: TokenReference,
) -> Result<ast::FunctionDeclaration, ()> {
    let function_name = match expect_function_name(state) {
        ParserResult::Value(name) => name,
        ParserResult::NotFound | ParserResult::LexerMoved => return Err(()),
    };

    let function_body = match parse_function_body(state) {
        ParserResult::Value(body) => body,

        ParserResult::LexerMoved => ast::FunctionBody::new(),

        ParserResult::NotFound => {
            state.token_error(function_token.clone(), "expected a function body");
            ast::FunctionBody::new()
        }
    };

    Ok(ast::FunctionDeclaration {
        function_token,
        name: function_name,
        body: function_body,
    })
}

fn expect_local_assignment(
    state: &mut ParserState,
    local_token: TokenReference,
) -> ParserResult<ast::LocalAssignment> {
    let names = match parse_name_list(state) {
        ParserResult::Value(names) => names,
        ParserResult::NotFound => {
            unreachable!("expect_local_assignment called without upcoming identifier");
        }
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    let mut local_assignment = ast::LocalAssignment {
        local_token,
        name_list: names_to_tokens(names),
        equal_token: None,
        expr_list: Punctuated::new(),
    };

    if matches!(state.current(), ParserResult::Value(token) if token.is_symbol(Symbol::Equal)) {
        local_assignment.equal_token = Some(state.consume().unwrap());
    } else {
        return ParserResult::Value(local_assignment);
    }

    match parse_expression_list(state) {
        ParserResult::Value(expr_list) => local_assignment.expr_list = expr_list,

        ParserResult::NotFound => {
            state.token_error(
                // rewrite todo: This is a great reason why we should be using ranges and not Position
                local_assignment.equal_token.clone().unwrap(),
                "expected an expression",
            );
        }

        ParserResult::LexerMoved => {}
    };

    ParserResult::Value(local_assignment)
}

fn parse_prefix(state: &mut ParserState) -> ParserResult<ast::Prefix> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            todo!()
        }

        TokenType::Identifier { identifier } => {
            ParserResult::Value(ast::Prefix::Name(state.consume().unwrap()))
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_arguments(state: &mut ParserState) -> ParserResult<ast::FunctionArgs> {
    let current = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current.token_type() {
        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            let left_parenthesis = state.consume().unwrap();
            let arguments = try_parser!(parse_expression_list(state)).unwrap_or_default();
            let right_parenthesis = match state.current() {
                ParserResult::Value(token) if token.is_symbol(Symbol::RightParen) => {
                    state.consume().unwrap()
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,

                _ => {
                    state.token_error(
                        left_parenthesis.clone(),
                        "expected `)` to close function call",
                    );

                    TokenReference::symbol(")").unwrap()
                }
            };

            ParserResult::Value(ast::FunctionArgs::Parentheses {
                parentheses: ContainedSpan::new(left_parenthesis, right_parenthesis),
                arguments,
            })
        }

        TokenType::Symbol {
            symbol: Symbol::LeftBrace,
        } => {
            todo!("table call")
        }

        TokenType::StringLiteral { .. } => {
            todo!("string call")
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_suffix(state: &mut ParserState) -> ParserResult<ast::Suffix> {
    let current = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Dot,
        } => {
            let dot = state.consume().unwrap();
            let name = match state.current() {
                ParserResult::Value(token) if token.token_kind() == TokenKind::Identifier => {
                    state.consume().unwrap()
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,

                ParserResult::Value(_) | ParserResult::NotFound => {
                    state.token_error(dot, "expected identifier after `.`");
                    return ParserResult::LexerMoved;
                }
            };

            ParserResult::Value(ast::Suffix::Index(ast::Index::Dot { dot, name }))
        }

        TokenType::Symbol {
            symbol: Symbol::LeftBracket,
        } => {
            let left_bracket = state.consume().unwrap();

            let expression = match parse_expression(state) {
                ParserResult::Value(expression) => expression,
                ParserResult::LexerMoved => return ParserResult::LexerMoved,
                ParserResult::NotFound => {
                    state.token_error(left_bracket, "expected expression after `[`");
                    return ParserResult::LexerMoved;
                }
            };

            let right_bracket = match state.current() {
                ParserResult::Value(token) if token.is_symbol(Symbol::RightBracket) => {
                    state.consume().unwrap()
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,

                ParserResult::Value(_) | ParserResult::NotFound => {
                    state.token_error(
                        left_bracket.clone(),
                        "expected `]` to close index expression",
                    );

                    // rewrite todo: this feels weird to recover from
                    TokenReference::symbol("]").unwrap()
                }
            };

            ParserResult::Value(ast::Suffix::Index(ast::Index::Brackets {
                brackets: ContainedSpan::new(left_bracket, right_bracket),
                expression,
            }))
        }

        TokenType::Symbol {
            symbol: Symbol::LeftParen | Symbol::LeftBrace,
        }
        | TokenType::StringLiteral { .. } => {
            let arguments = try_parser!(parse_arguments(state)).unwrap();
            ParserResult::Value(ast::Suffix::Call(ast::Call::AnonymousCall(arguments)))
        }

        TokenType::Symbol {
            symbol: Symbol::Colon,
        } => {
            let colon_token = state.consume().unwrap();

            let name = match state.current() {
                ParserResult::Value(token) if token.token_kind() == TokenKind::Identifier => {
                    state.consume().unwrap()
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,

                ParserResult::Value(_) | ParserResult::NotFound => {
                    state.token_error(colon_token, "expected identifier after `:`");
                    return ParserResult::LexerMoved;
                }
            };

            let args = match parse_arguments(state) {
                ParserResult::Value(args) => args,
                ParserResult::LexerMoved => ast::FunctionArgs::empty(),
                ParserResult::NotFound => {
                    state.token_error(colon_token.clone(), "expected arguments after `:`");
                    ast::FunctionArgs::empty()
                }
            };

            ParserResult::Value(ast::Suffix::Call(ast::Call::MethodCall(ast::MethodCall {
                colon_token,
                name,
                args,
            })))
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_prefix_and_suffixes(
    state: &mut ParserState,
) -> ParserResult<(ast::Prefix, Vec<ast::Suffix>)> {
    let prefix = match parse_prefix(state) {
        ParserResult::Value(prefix) => prefix,
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
        ParserResult::NotFound => return ParserResult::NotFound,
    };

    let mut suffixes = Vec::new();

    loop {
        match parse_suffix(state) {
            ParserResult::Value(suffix) => {
                suffixes.push(suffix);
            }

            ParserResult::LexerMoved => {
                if suffixes.is_empty() {
                    return ParserResult::LexerMoved;
                } else {
                    break;
                }
            }

            ParserResult::NotFound => {
                break;
            }
        };
    }

    ParserResult::Value((prefix, suffixes))
}

fn parse_expression(state: &mut ParserState) -> ParserResult<Expression> {
    let primary_expression = match parse_primary_expression(state) {
        ParserResult::Value(expression) => expression,
        ParserResult::NotFound => return ParserResult::NotFound,
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    parse_expression_with_precedence(state, primary_expression, 0)
}

fn parse_primary_expression(state: &mut ParserState) -> ParserResult<Expression> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Function,
        } => {
            let function_token = state.consume().unwrap();
            let function_body = match parse_function_body(state) {
                ParserResult::Value(body) => body,
                ParserResult::LexerMoved => return ParserResult::LexerMoved,
                ParserResult::NotFound => {
                    state.token_error(function_token, "expected a function body");
                    return ParserResult::LexerMoved;
                }
            };

            ParserResult::Value(Expression::Function((function_token, function_body)))
        }

        TokenType::Symbol {
            symbol: Symbol::True | Symbol::False | Symbol::Nil,
        } => ParserResult::Value(Expression::Symbol(state.consume().unwrap())),

        TokenType::StringLiteral { .. } => {
            let string_token = state.consume().unwrap();
            ParserResult::Value(Expression::String(string_token))
        }

        TokenType::Number { .. } => {
            let number_token = state.consume().unwrap();
            ParserResult::Value(Expression::Number(number_token))
        }

        TokenType::Identifier { .. }
        | TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            let (prefix, suffixes) = match parse_prefix_and_suffixes(state) {
                ParserResult::Value(value) => value,
                ParserResult::LexerMoved => return ParserResult::LexerMoved,
                ParserResult::NotFound => {
                    unreachable!("identifier found but parse_prefix_and_suffixes didn't even move");
                }
            };

            if suffixes.is_empty() {
                match prefix {
                    ast::Prefix::Expression(expression) => ParserResult::Value(*expression),
                    ast::Prefix::Name(name) => {
                        ParserResult::Value(Expression::Var(ast::Var::Name(name)))
                    }
                }
            } else {
                ParserResult::Value(Expression::Var(ast::Var::Expression(Box::new(
                    ast::VarExpression { prefix, suffixes },
                ))))
            }
        }

        TokenType::Symbol {
            symbol: Symbol::Minus | Symbol::Not | Symbol::Hash,
        } => {
            let unary_operator_token = state.consume().unwrap();
            parse_unary_expression(state, unary_operator_token)
        }

        _ => ParserResult::NotFound,
    }
}

// rewrite todo: i think this should be iterative instead of recursive
fn parse_expression_with_precedence(
    state: &mut ParserState,
    mut lhs: Expression,
    precedence: u8,
) -> ParserResult<Expression> {
    loop {
        let Some(bin_op_precedence) = ast::BinOp::precedence_of_token(state.current().unwrap()) else {
            return ParserResult::Value(lhs);
        };

        if bin_op_precedence < precedence {
            return ParserResult::Value(lhs);
        }

        let bin_op = consume_bin_op(state).unwrap();

        let mut rhs = match parse_primary_expression(state) {
            ParserResult::Value(expression) => expression,
            ParserResult::NotFound => {
                state.token_error(
                    bin_op.token().clone(),
                    "expected expression after binary operator",
                );
                return ParserResult::Value(lhs);
            }
            ParserResult::LexerMoved => return ParserResult::LexerMoved,
        };

        loop {
            let next_bin_op_token = state.current().unwrap();

            let Some(next_bin_op_precedence) = ast::BinOp::precedence_of_token(next_bin_op_token) else {
                break;
            };

            let precedence_to_search;

            if next_bin_op_precedence > bin_op_precedence {
                precedence_to_search = bin_op_precedence + 1;
            } else if ast::BinOp::is_right_associative_token(next_bin_op_token)
                && next_bin_op_precedence == bin_op_precedence
            {
                precedence_to_search = bin_op_precedence;
            } else {
                break;
            }

            rhs = match parse_expression_with_precedence(state, rhs, precedence_to_search) {
                ParserResult::Value(expression) => expression,
                ParserResult::NotFound => {
                    state.token_error(
                        bin_op.token().clone(),
                        "expected expression after binary operator",
                    );
                    return ParserResult::Value(lhs);
                }
                ParserResult::LexerMoved => return ParserResult::Value(lhs),
            };
        }

        lhs = Expression::BinaryOperator {
            lhs: Box::new(lhs),
            binop: bin_op,
            rhs: Box::new(rhs),
        };
    }
}

// rewrite todo: should probably all be done by the macro.
// all the copy and paste i've been doing is bad
fn consume_bin_op(state: &mut ParserState) -> Option<ast::BinOp> {
    match state.current().unwrap().token_type() {
        TokenType::Symbol { symbol } => match symbol {
            Symbol::And => Some(ast::BinOp::And(state.consume().unwrap())),
            Symbol::Caret => Some(ast::BinOp::Caret(state.consume().unwrap())),
            Symbol::GreaterThan => Some(ast::BinOp::GreaterThan(state.consume().unwrap())),
            Symbol::GreaterThanEqual => {
                Some(ast::BinOp::GreaterThanEqual(state.consume().unwrap()))
            }
            Symbol::LessThan => Some(ast::BinOp::LessThan(state.consume().unwrap())),
            Symbol::LessThanEqual => Some(ast::BinOp::LessThanEqual(state.consume().unwrap())),
            Symbol::Minus => Some(ast::BinOp::Minus(state.consume().unwrap())),
            Symbol::Or => Some(ast::BinOp::Or(state.consume().unwrap())),
            Symbol::Percent => Some(ast::BinOp::Percent(state.consume().unwrap())),
            Symbol::Plus => Some(ast::BinOp::Plus(state.consume().unwrap())),
            Symbol::Slash => Some(ast::BinOp::Slash(state.consume().unwrap())),
            Symbol::Star => Some(ast::BinOp::Star(state.consume().unwrap())),
            Symbol::TildeEqual => Some(ast::BinOp::TildeEqual(state.consume().unwrap())),
            Symbol::TwoDots => Some(ast::BinOp::TwoDots(state.consume().unwrap())),
            Symbol::TwoEqual => Some(ast::BinOp::TwoEqual(state.consume().unwrap())),
            _ => None,
        },

        _ => None,
    }
}

fn parse_unary_expression(
    state: &mut ParserState,
    unary_operator_token: ast::TokenReference,
) -> ParserResult<ast::Expression> {
    let unary_operator = match unary_operator_token.token_type() {
        TokenType::Symbol { symbol } => match symbol {
            Symbol::Minus => ast::UnOp::Minus(unary_operator_token),
            Symbol::Not => ast::UnOp::Not(unary_operator_token),
            Symbol::Hash => ast::UnOp::Hash(unary_operator_token),
            _ => unreachable!(),
        },

        _ => unreachable!(),
    };

    let expression = match parse_expression(state) {
        ParserResult::Value(expression) => expression,
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
        ParserResult::NotFound => {
            state.token_error(
                unary_operator.token().clone(),
                format!(
                    "expected an expression after {}",
                    unary_operator.token().token()
                ),
            );
            return ParserResult::LexerMoved;
        }
    };

    // rewrite todo: when does unop precedence come into play?
    ParserResult::Value(Expression::UnaryOperator {
        unop: unary_operator,
        expression: Box::new(expression),
    })
}

fn parse_function_body(state: &mut ParserState) -> ParserResult<FunctionBody> {
    let left_parenthesis = match try_parser!(state.current()) {
        Some(token) if token.is_symbol(Symbol::LeftParen) => state.consume().unwrap(),
        _ => return ParserResult::NotFound,
    };

    let parameters = match one_or_more(state, parse_parameter, &[Symbol::Comma]) {
        ParserResult::Value(parameters) => parameters,
        ParserResult::NotFound => Punctuated::new(),
        // rewrite todo: i want to support function x(a, b, c,) as being fallible
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    let right_parenthesis = match try_parser!(state.current()) {
        Some(token) if token.is_symbol(Symbol::RightParen) => state.consume().unwrap(),
        _ => {
            state.token_error(left_parenthesis, "expected a `)`");
            return ParserResult::LexerMoved;
        }
    };

    let (block, end) = match parse_block_with_end(state, &right_parenthesis) {
        Ok((block, end)) => (block, end),
        Err(()) => return ParserResult::LexerMoved,
    };

    ParserResult::Value(FunctionBody {
        parameters_parentheses: ContainedSpan::new(left_parenthesis, right_parenthesis),
        parameters: parameters
            .into_pairs()
            .map(|parameter| parameter.map(|parameter| parameter.parameter))
            .collect(),

        block,
        end_token: end,
    })
}

struct PackedParameter {
    parameter: Parameter,
    // rewrite todo: this is where a type assignment can go
}

fn parse_parameter(state: &mut ParserState) -> ParserResult<PackedParameter> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Ellipse,
        } => ParserResult::Value(PackedParameter {
            parameter: Parameter::Ellipse(state.consume().unwrap()),
        }),

        TokenType::Identifier { identifier } => ParserResult::Value(PackedParameter {
            parameter: Parameter::Name(state.consume().unwrap()),
        }),

        _ => ParserResult::NotFound,
    }
}

struct Name {
    name: TokenReference,
    // rewrite todo: this is where a type assignment can go
}

fn names_to_tokens(names: Punctuated<Name>) -> Punctuated<TokenReference> {
    names
        .into_pairs()
        .map(|pair| match pair {
            Pair::Punctuated(name, punct) => Pair::Punctuated(name.name, punct),
            Pair::End(name) => Pair::End(name.name),
        })
        .collect()
}

fn parse_name(state: &mut ParserState) -> ParserResult<Name> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Identifier { identifier } => ParserResult::Value(Name {
            name: state.consume().unwrap(),
        }),

        _ => ParserResult::NotFound,
    }
}

fn one_or_more<T, F: Fn(&mut ParserState) -> ParserResult<T>>(
    state: &mut ParserState,
    parser: F,
    delimiters: &[Symbol],
) -> ParserResult<Punctuated<T>> {
    let mut values = Punctuated::new();

    loop {
        let value = match parser(state) {
            ParserResult::Value(value) => value,
            ParserResult::NotFound | ParserResult::LexerMoved => break,
        };

        let next_token = match state.current() {
            ParserResult::Value(token) => token,
            ParserResult::NotFound | ParserResult::LexerMoved => break,
        };

        if let TokenType::Symbol { symbol } = next_token.token_type() {
            if delimiters.contains(symbol) {
                values.push(Pair::Punctuated(value, state.consume().unwrap()));
                continue;
            }
        }

        values.push(Pair::End(value));
        break;
    }

    if values.is_empty() {
        return ParserResult::NotFound;
    }

    ParserResult::Value(values)
}

fn parse_name_list(state: &mut ParserState) -> ParserResult<Punctuated<Name>> {
    one_or_more(state, parse_name, &[Symbol::Comma])
}

fn parse_expression_list(state: &mut ParserState) -> ParserResult<Punctuated<Expression>> {
    one_or_more(state, parse_expression, &[Symbol::Comma])
}
