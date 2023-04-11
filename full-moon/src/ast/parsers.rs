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
                // rewrite todo: consume
                let semicolon = state.consume_if(Symbol::Semicolon);
                stmts.push((stmt, semicolon));
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

    let last_stmt = match parse_last_stmt(state) {
        ParserResult::Value(stmt) => Some(stmt),
        ParserResult::LexerMoved | ParserResult::NotFound => None,
    };

    ParserResult::Value(ast::Block { stmts, last_stmt })
}

// Blocks in general are not very fallible. This means, for instance, not finishing `function()`
// will result in a completely ignored function body.
// This is an opinionated choice because I believe selene is going to produce terrible outputs if we don't.
// rewrite todo: I don't love that this only accepts one token. it should be range(s?)
// rewrite todo: expect_?
fn parse_block_with_end(
    state: &mut ParserState,
    // rewrite todo: use this, or the range
    start_for_errors: &TokenReference,
) -> Result<(ast::Block, TokenReference), ()> {
    let block = match parse_block(state) {
        ParserResult::Value(block) => block,
        ParserResult::NotFound => unreachable!("parse_block should always return a value"),
        ParserResult::LexerMoved => return Err(()),
    };

    let Some(end_token) = state.require(Symbol::End, "expected `end` to close block") else {
        return Ok((block, TokenReference::symbol("end").unwrap()));
    };

    Ok((block, end_token))
}

fn parse_stmt(state: &mut ParserState) -> ParserResult<ast::Stmt> {
    let Ok(current_token) = state.current() else {
        return ParserResult::NotFound;
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Local,
        } => {
            let local_token = state.consume().unwrap();
            let next_token = match state.current() {
                Ok(token) => token,
                Err(()) => return ParserResult::LexerMoved,
            };

            match next_token.token_type() {
                TokenType::Identifier { .. } => ParserResult::Value(ast::Stmt::LocalAssignment(
                    try_parser!(expect_local_assignment(state, local_token)).unwrap(),
                )),

                TokenType::Symbol {
                    symbol: Symbol::Function,
                } => {
                    let function_token = state.consume().unwrap();

                    let function_name = match state.current() {
                        Ok(token) if token.token_kind() == TokenKind::Identifier => {
                            state.consume().unwrap()
                        }

                        Ok(token) => {
                            state.token_error(token.clone(), "expected a function name");
                            return ParserResult::LexerMoved;
                        }

                        Err(()) => return ParserResult::LexerMoved,
                    };

                    let function_body = match parse_function_body(state) {
                        ParserResult::Value(function_body) => function_body,
                        ParserResult::NotFound => {
                            state.token_error(function_token, "expected a function body");
                            return ParserResult::LexerMoved;
                        }
                        ParserResult::LexerMoved => return ParserResult::LexerMoved,
                    };

                    ParserResult::Value(ast::Stmt::LocalFunction(ast::LocalFunction {
                        local_token,
                        function_token,
                        name: function_name,
                        body: function_body,
                    }))
                }

                _ => {
                    state.token_error(
                        next_token.clone(),
                        "expected either a variable name or `function`",
                    );

                    ParserResult::LexerMoved
                }
            }
        }

        TokenType::Symbol {
            symbol: Symbol::For,
        } => {
            let for_token = state.consume().unwrap();

            ParserResult::Value(match expect_for_stmt(state, for_token) {
                Ok(for_stmt) => for_stmt,
                Err(()) => return ParserResult::LexerMoved,
            })
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

        TokenType::Symbol { symbol: Symbol::If } => {
            let if_token = state.consume().unwrap();

            ParserResult::Value(ast::Stmt::If(match expect_if_stmt(state, if_token) {
                Ok(if_stmt) => if_stmt,
                Err(()) => return ParserResult::LexerMoved,
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
            symbol: Symbol::Repeat,
        } => {
            let repeat_token = state.consume().unwrap();

            ParserResult::Value(match expect_repeat_stmt(state, repeat_token) {
                Ok(repeat_stmt) => repeat_stmt,
                Err(()) => return ParserResult::LexerMoved,
            })
        }

        TokenType::Symbol {
            symbol: Symbol::While,
        } => {
            let while_token = state.consume().unwrap();

            ParserResult::Value(ast::Stmt::While(
                match expect_while_stmt(state, while_token) {
                    Ok(while_stmt) => while_stmt,
                    Err(()) => return ParserResult::LexerMoved,
                },
            ))
        }

        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        }
        | TokenType::Identifier { .. } => {
            // unwrap() because we're always starting on the right path
            // rewrite todo: i'm very skeptical of the fallibility of this
            let (prefix, suffixes) = try_parser!(parse_prefix_and_suffixes(state)).unwrap();

            let var = match suffixes.last() {
                Some(ast::Suffix::Call(_)) => {
                    return ParserResult::Value(ast::Stmt::FunctionCall(ast::FunctionCall {
                        prefix,
                        suffixes,
                    }));
                }

                Some(ast::Suffix::Index(_)) => {
                    ast::Var::Expression(Box::new(ast::VarExpression { prefix, suffixes }))
                }

                None => match prefix {
                    ast::Prefix::Name(name) => ast::Var::Name(name),
                    ast::Prefix::Expression(_) => todo!("VarExpression?"),
                },
            };

            match state.current() {
                Ok(token) if token.is_symbol(Symbol::Comma) || token.is_symbol(Symbol::Equal) => {}

                Ok(token) => {
                    state.token_error(
                        token.clone(),
                        "unexpected expression when looking for a statement",
                    );

                    return ParserResult::LexerMoved;
                }

                Err(()) => return ParserResult::LexerMoved,
            };

            let mut var_list = Punctuated::new();
            var_list.push(Pair::End(var));

            loop {
                let next_comma = match state.current() {
                    Ok(token) if token.is_symbol(Symbol::Comma) => state.consume().unwrap(),
                    Ok(_) => break,
                    Err(()) => return ParserResult::LexerMoved,
                };

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

            let Some(equal_token) = state.require(Symbol::Equal, "expected `=` after name") else {
                return ParserResult::LexerMoved;
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

fn parse_last_stmt(
    state: &mut ParserState,
) -> ParserResult<(ast::LastStmt, Option<TokenReference>)> {
    let last_stmt = match state.current() {
        Ok(token) if token.is_symbol(Symbol::Return) => {
            let return_token = state.consume().unwrap();

            let expr_list = match parse_expression_list(state) {
                ParserResult::Value(expr_list) => expr_list,
                ParserResult::LexerMoved | ParserResult::NotFound => Punctuated::new(),
            };

            ast::LastStmt::Return(ast::Return {
                token: return_token,
                returns: expr_list,
            })
        }

        Ok(token) if token.is_symbol(Symbol::Break) => {
            let break_token = state.consume().unwrap();
            ast::LastStmt::Break(break_token)
        }

        _ => return ParserResult::NotFound,
    };

    let semicolon = state.consume_if(Symbol::Semicolon);

    ParserResult::Value((last_stmt, semicolon))
}

fn expect_function_name(state: &mut ParserState) -> ParserResult<ast::FunctionName> {
    let mut names = Punctuated::new();

    let name = match state.current() {
        Ok(token) if matches!(token.token_type(), TokenType::Identifier { .. }) => {
            state.consume().unwrap()
        }

        Ok(token) => {
            state.token_error(token.clone(), "expected function name");
            return ParserResult::NotFound;
        }

        Err(()) => return ParserResult::NotFound,
    };

    names.push(Pair::End(name));

    loop {
        let middle_token = match state.current() {
            Ok(token) if token.is_symbol(Symbol::Colon) || token.is_symbol(Symbol::Dot) => {
                state.consume().unwrap()
            }
            Ok(_) => break,
            Err(()) => return ParserResult::LexerMoved,
        };

        let name = match state.current() {
            Ok(token) if matches!(token.token_type(), TokenType::Identifier { .. }) => {
                state.consume().unwrap()
            }

            Ok(token) => {
                state.token_error(
                    token.clone(),
                    format!("expected name after `{}`", middle_token.token()),
                );
                return ParserResult::NotFound;
            }

            Err(()) => {
                return ParserResult::LexerMoved;
            }
        };

        if middle_token.is_symbol(Symbol::Dot) {
            // rewrite todo: i've been doing this more, would be nice to have a helper
            let last_name = names.pop().unwrap();
            names.push(Pair::Punctuated(last_name.into_value(), middle_token));
            names.push(Pair::End(name));
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

fn expect_for_stmt(state: &mut ParserState, for_token: TokenReference) -> Result<ast::Stmt, ()> {
    let name_list = match parse_name_list(state) {
        ParserResult::Value(name_list) => name_list,
        ParserResult::NotFound => {
            state.token_error(for_token, "expected name after `for`");
            return Err(());
        }
        ParserResult::LexerMoved => return Err(()),
    };

    let current_token = match state.current() {
        Ok(token) => token,
        Err(()) => return Err(()),
    };

    if name_list.is_empty() {
        state.token_error(current_token.clone(), "expected name after `for`");
        return Err(());
    }

    if name_list.len() == 1 && current_token.is_symbol(Symbol::Equal) {
        return Ok(ast::Stmt::NumericFor(expect_numeric_for_stmt(
            state,
            for_token,
            name_list.into_iter().next().unwrap(),
        )?));
    }

    let in_token = match current_token {
        token if token.is_symbol(Symbol::In) => state.consume().unwrap(),
        token => {
            state.token_error(token.clone(), "expected `in` after name list");
            return Err(());
        }
    };

    let expressions = match parse_expression_list(state) {
        ParserResult::Value(expressions) => expressions,
        ParserResult::NotFound => {
            state.token_error(in_token, "expected expressions after `in`");
            return Err(());
        }
        ParserResult::LexerMoved => return Err(()),
    };

    let Some(do_token) = state.require(Symbol::Do, "expected `do` after expression list") else {
        return Ok(ast::Stmt::GenericFor(ast::GenericFor {
            for_token,
            names: names_to_tokens(name_list),
            in_token,
            expr_list: expressions,
            do_token: TokenReference::symbol("do").unwrap(),
            block: ast::Block::new(),
            end_token: TokenReference::symbol("end").unwrap(),
        }));
    };

    let (block, end) = match parse_block_with_end(state, &do_token) {
        Ok(block) => block,
        Err(()) => (ast::Block::new(), TokenReference::symbol("end").unwrap()),
    };

    Ok(ast::Stmt::GenericFor(ast::GenericFor {
        for_token,
        names: names_to_tokens(name_list),
        in_token,
        expr_list: expressions,
        do_token,
        block,
        end_token: end,
    }))
}

fn expect_numeric_for_stmt(
    state: &mut ParserState,
    for_token: TokenReference,
    index_variable: Name,
) -> Result<ast::NumericFor, ()> {
    let equal_token = state.consume().unwrap();
    debug_assert!(equal_token.is_symbol(Symbol::Equal));

    let start = match parse_expression(state) {
        ParserResult::Value(start) => start,
        ParserResult::NotFound => {
            state.token_error(equal_token, "expected start expression after `=`");
            return Err(());
        }
        ParserResult::LexerMoved => return Err(()),
    };

    let Some(start_end_comma) = state.require(Symbol::Comma, "expected `,` after start expression") else {
        return Err(());
    };

    let end = match parse_expression(state) {
        ParserResult::Value(end) => end,
        ParserResult::NotFound => {
            state.token_error(start_end_comma, "expected end expression after `,`");
            return Err(());
        }
        ParserResult::LexerMoved => return Err(()),
    };

    // rewrite todo: this can recover into a numeric for loop with no step (or simulate the do..end)
    let (end_step_comma, step) = match state.consume_if(Symbol::Comma) {
        Some(end_step_comma) => match parse_expression(state) {
            ParserResult::Value(step) => (Some(end_step_comma), Some(step)),
            ParserResult::NotFound => {
                state.token_error(start_end_comma, "expected step expression after `,`");
                return Err(());
            }
            ParserResult::LexerMoved => return Err(()),
        },

        None => (None, None),
    };

    let Some(do_token) = state.require(Symbol::Do, "expected `do` after step expression") else {
        return Err(());
    };

    let (block, end_token) = match parse_block_with_end(state, &do_token) {
        Ok(block) => block,
        Err(()) => (ast::Block::new(), TokenReference::symbol("end").unwrap()),
    };

    Ok(ast::NumericFor {
        for_token,
        index_variable: index_variable.name,
        equal_token,
        start,
        start_end_comma,
        end,
        end_step_comma,
        step,
        do_token,
        block,
        end_token,
    })
}

fn expect_if_stmt(state: &mut ParserState, if_token: TokenReference) -> Result<ast::If, ()> {
    let condition = match parse_expression(state) {
        ParserResult::Value(condition) => condition,
        ParserResult::NotFound => {
            state.token_error(if_token, "expected condition after `if`");
            return Err(());
        }
        ParserResult::LexerMoved => return Err(()),
    };

    let Some(then_token) = state.require(Symbol::Then, "expected `then` after condition") else {
        return Err(());
    };

    let then_block = match parse_block(state) {
        ParserResult::Value(block) => block,
        ParserResult::NotFound => {
            state.token_error(then_token, "expected block after `then`");
            return Ok(ast::If::new(condition));
        }
        ParserResult::LexerMoved => {
            return Ok(ast::If::new(condition));
        }
    };

    let mut else_if = Vec::new();

    let else_if_optional = |else_if: Vec<ast::ElseIf>| {
        if else_if.is_empty() {
            None
        } else {
            Some(else_if)
        }
    };

    let unfinished_if =
        |condition, else_if| Ok(ast::If::new(condition).with_else_if(else_if_optional(else_if)));

    loop {
        let else_if_token = match state.current() {
            Ok(else_if_token) if else_if_token.is_symbol(Symbol::ElseIf) => {
                state.consume().unwrap()
            }

            Ok(_) => break,

            Err(()) => {
                return unfinished_if(condition, else_if);
            }
        };

        let condition = match parse_expression(state) {
            ParserResult::Value(condition) => condition,
            ParserResult::NotFound => {
                state.token_error(else_if_token, "expected condition after `elseif`");
                return unfinished_if(condition, else_if);
            }
            ParserResult::LexerMoved => {
                return unfinished_if(condition, else_if);
            }
        };

        let Some(then_token) = state.require(Symbol::Then, "expected `then` after condition") else {
            return unfinished_if(condition, else_if);
        };

        let then_block = match parse_block(state) {
            ParserResult::Value(block) => block,
            ParserResult::NotFound => {
                state.token_error(then_token, "expected block after `then`");
                return unfinished_if(condition, else_if);
            }
            ParserResult::LexerMoved => {
                return unfinished_if(condition, else_if);
            }
        };

        else_if.push(ast::ElseIf {
            else_if_token,
            condition,
            then_token,
            block: then_block,
        });
    }

    let (else_block, else_token) = match state.consume_if(Symbol::Else) {
        Some(else_token) => match parse_block(state) {
            ParserResult::Value(block) => (Some(block), Some(else_token)),
            ParserResult::NotFound => {
                state.token_error(else_token.clone(), "expected block after `else`");
                (Some(ast::Block::new()), Some(else_token))
            }
            ParserResult::LexerMoved => (Some(ast::Block::new()), Some(else_token)),
        },

        None => (None, None),
    };

    let end_token = match state.current() {
        Ok(token) if token.is_symbol(Symbol::End) => state.consume().unwrap(),
        Ok(token) => {
            state.token_error(token.clone(), "expected `end` to conclude `if`");
            TokenReference::symbol("end").unwrap()
        }

        Err(()) => TokenReference::symbol("end").unwrap(),
    };

    Ok(ast::If {
        if_token,
        condition,
        then_token,
        block: then_block,
        else_if: else_if_optional(else_if),
        else_token,
        r#else: else_block,
        end_token,
    })
}

// rewrite todo: just result, i guess
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

    local_assignment.equal_token = match state.consume_if(Symbol::Equal) {
        Some(equal_token) => Some(equal_token),
        None => return ParserResult::Value(local_assignment),
    };

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

fn expect_expression_key(
    state: &mut ParserState,
    left_bracket: TokenReference,
) -> Result<ast::Field, ()> {
    let expression = match parse_expression(state) {
        ParserResult::Value(expression) => expression,

        ParserResult::NotFound => {
            state.token_error(left_bracket, "expected an expression after `[`");

            return Err(());
        }

        ParserResult::LexerMoved => {
            return Err(());
        }
    };

    // rewrite todo: this should take a range ( `[a.b.c()` should have the whole range )
    let Some(right_bracket) = state.require(Symbol::RightBracket, "expected `]` after expression") else {
        return Err(());
    };

    // rewrite todo: we can realistically construct a field in error recovery
    // rewrite todo: this should also be range
    let Some(equal_token) = state.require(Symbol::Equal, "expected `=` after expression") else {
        return Err(());
    };

    let value = match parse_expression(state) {
        ParserResult::Value(expression) => expression,

        ParserResult::NotFound => {
            state.token_error(equal_token, "expected an expression after `=`");

            return Err(());
        }

        ParserResult::LexerMoved => {
            return Err(());
        }
    };

    Ok(ast::Field::ExpressionKey {
        brackets: ContainedSpan::new(left_bracket, right_bracket),
        key: expression,
        equal: equal_token,
        value,
    })
}

fn force_table_constructor(
    state: &mut ParserState,
    left_brace: TokenReference,
) -> ast::TableConstructor {
    let mut fields = Punctuated::new();

    let unfinished_table =
        |left_brace: TokenReference, fields: Punctuated<ast::Field>| ast::TableConstructor {
            braces: ContainedSpan::new(left_brace, ast::TokenReference::symbol("}").unwrap()),
            fields,
        };

    loop {
        let Ok(current_token) = state.current() else {
            return unfinished_table(left_brace, fields);
        };

        let field = match current_token.token_type() {
            TokenType::Symbol {
                symbol: Symbol::RightBrace,
            } => {
                return ast::TableConstructor {
                    braces: ContainedSpan::new(left_brace, state.consume().unwrap()),
                    fields,
                };
            }

            TokenType::Symbol {
                symbol: Symbol::LeftBracket,
            } => {
                let left_bracket = state.consume().unwrap();

                match expect_expression_key(state, left_bracket) {
                    Ok(field) => field,
                    Err(()) => {
                        return unfinished_table(left_brace, fields);
                    }
                }
            }

            TokenType::Identifier { .. } if matches!(state.peek(), Ok(peek_token) if peek_token.is_symbol(Symbol::Equal)) =>
            {
                let key = state.consume().unwrap();

                let equal_token = state.consume().unwrap();

                let value = match parse_expression(state) {
                    ParserResult::Value(expression) => expression,

                    ParserResult::NotFound => {
                        state.token_error(equal_token, "expected an expression after `=`");

                        return unfinished_table(left_brace, fields);
                    }

                    ParserResult::LexerMoved => {
                        return unfinished_table(left_brace, fields);
                    }
                };

                ast::Field::NameKey {
                    key,
                    equal: equal_token,
                    value,
                }
            }

            _ => {
                let value = match parse_expression(state) {
                    ParserResult::Value(expression) => expression,

                    ParserResult::NotFound => {
                        state.token_error(
                            match fields.last() {
                                Some(Pair::End(field)) => field.tokens().last().unwrap().clone(),
                                Some(Pair::Punctuated(field, _)) => {
                                    field.tokens().last().unwrap().clone()
                                }
                                None => left_brace.clone(),
                            },
                            "expected a field",
                        );

                        return unfinished_table(left_brace, fields);
                    }

                    ParserResult::LexerMoved => {
                        return unfinished_table(left_brace, fields);
                    }
                };

                ast::Field::NoKey(value)
            }
        };

        match state.current() {
            Ok(token) => {
                if token.is_symbol(Symbol::Comma) || token.is_symbol(Symbol::Semicolon) {
                    fields.push(Pair::Punctuated(field, state.consume().unwrap()))
                } else {
                    fields.push(Pair::End(field));
                    break;
                }
            }

            Err(()) => {
                fields.push(Pair::End(field));
                break;
            }
        };
    }

    // rewrite todo: range, or at least field.tokens().last().unwrap().clone()
    let right_brace = match state.require(Symbol::RightBrace, "expected `}` after last field") {
        Some(right_brace) => right_brace,
        None => TokenReference::symbol("}").unwrap(),
    };

    ast::TableConstructor {
        braces: ContainedSpan::new(left_brace, right_brace),
        fields,
    }
}

fn expect_repeat_stmt(
    state: &mut ParserState,
    repeat_token: TokenReference,
) -> Result<ast::Stmt, ()> {
    let block = match parse_block(state) {
        ParserResult::Value(block) => block,

        ParserResult::NotFound => {
            state.token_error(repeat_token, "expected a block after `repeat`");

            return Err(());
        }

        ParserResult::LexerMoved => {
            return Err(());
        }
    };

    let Some(until_token) = state.require(Symbol::Until, "expected `until` after block") else {
        return Ok(ast::Stmt::Do(ast::Do::new().with_block(block)));
    };

    let condition = match parse_expression(state) {
        ParserResult::Value(expression) => expression,

        ParserResult::NotFound => {
            state.token_error(until_token, "expected a condition after `until`");
            return Ok(ast::Stmt::Do(ast::Do::new().with_block(block)));
        }

        ParserResult::LexerMoved => {
            return Ok(ast::Stmt::Do(ast::Do::new().with_block(block)));
        }
    };

    Ok(ast::Stmt::Repeat(ast::Repeat {
        repeat_token,
        block,
        until: condition,
        until_token,
    }))
}

fn expect_while_stmt(
    state: &mut ParserState,
    while_token: TokenReference,
) -> Result<ast::While, ()> {
    let condition = match parse_expression(state) {
        ParserResult::Value(expression) => expression,

        ParserResult::NotFound => {
            state.token_error(while_token, "expected a condition after `while`");

            return Err(());
        }

        ParserResult::LexerMoved => {
            return Err(());
        }
    };

    let Some(do_token) = state.require(Symbol::Do, "expected `do` after condition") else {
        return Ok(ast::While::new(condition));
    };

    let (block, end_token) = match parse_block_with_end(state, &do_token) {
        Ok((block, end_token)) => (block, end_token),

        Err(()) => {
            return Ok(ast::While::new(condition));
        }
    };

    Ok(ast::While {
        while_token,
        condition,
        do_token,
        block,
        end_token,
    })
}

fn parse_prefix(state: &mut ParserState) -> ParserResult<ast::Prefix> {
    let current_token = match state.current() {
        Ok(token) => token,
        Err(()) => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            let left_parenthesis = state.consume().unwrap();

            let expression = Box::new(match try_parser!(parse_expression(state)) {
                Some(expression) => expression,

                None => {
                    state.token_error(left_parenthesis, "expected an expression after `(`");
                    return ParserResult::LexerMoved;
                }
            });

            let Some(right_parenthesis) = state.require(Symbol::RightParen, "expected `)` after expression") else {
                return ParserResult::Value(ast::Prefix::Expression(Box::new(
                    ast::Expression::Parentheses {
                        contained: ContainedSpan::new(
                            left_parenthesis,
                            TokenReference::symbol(")").unwrap(),
                        ),
                        expression,
                    },
                )));
            };

            ParserResult::Value(ast::Prefix::Expression(Box::new(
                ast::Expression::Parentheses {
                    contained: ContainedSpan::new(left_parenthesis, right_parenthesis),
                    expression,
                },
            )))
        }

        TokenType::Identifier { .. } => {
            ParserResult::Value(ast::Prefix::Name(state.consume().unwrap()))
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_arguments(state: &mut ParserState) -> ParserResult<ast::FunctionArgs> {
    let current = match state.current() {
        Ok(token) => token,
        Err(()) => return ParserResult::NotFound,
    };

    match current.token_type() {
        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            let left_parenthesis = state.consume().unwrap();
            let arguments = try_parser!(parse_expression_list(state)).unwrap_or_default();
            let right_parenthesis = match state.require_with_reference_token(
                Symbol::RightParen,
                "expected `)` to close function call",
                &left_parenthesis,
            ) {
                Some(token) => token,

                None => TokenReference::symbol(")").unwrap(),
            };

            ParserResult::Value(ast::FunctionArgs::Parentheses {
                parentheses: ContainedSpan::new(left_parenthesis, right_parenthesis),
                arguments,
            })
        }

        TokenType::Symbol {
            symbol: Symbol::LeftBrace,
        } => {
            let left_brace = state.consume().unwrap();

            ParserResult::Value(ast::FunctionArgs::TableConstructor(
                force_table_constructor(state, left_brace),
            ))
        }

        TokenType::StringLiteral { .. } => {
            ParserResult::Value(ast::FunctionArgs::String(state.consume().unwrap()))
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_suffix(state: &mut ParserState) -> ParserResult<ast::Suffix> {
    let Ok(current) = state.current() else {
        return ParserResult::NotFound;
    };

    match current.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Dot,
        } => {
            let dot = state.consume().unwrap();
            let name = match state.current() {
                Ok(token) if token.token_kind() == TokenKind::Identifier => {
                    state.consume().unwrap()
                }

                Ok(_) => {
                    state.token_error(dot, "expected identifier after `.`");
                    return ParserResult::LexerMoved;
                }

                Err(()) => return ParserResult::LexerMoved,
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

            // rewrite todo: this should use `[abc` as the error range
            let right_bracket = match state.require(
                Symbol::RightBracket,
                "expected `]` to close index expression",
            ) {
                Some(right_bracket) => right_bracket,

                None => {
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
                Ok(token) if token.token_kind() == TokenKind::Identifier => {
                    state.consume().unwrap()
                }

                Ok(_) => {
                    state.token_error(colon_token, "expected identifier after `:`");
                    return ParserResult::LexerMoved;
                }

                Err(()) => return ParserResult::LexerMoved,
            };

            let args = match parse_arguments(state) {
                ParserResult::Value(args) => args,
                ParserResult::LexerMoved => ast::FunctionArgs::empty(),
                ParserResult::NotFound => {
                    state.token_error(name.clone(), "expected arguments after `:`");
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
                break;
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
    let current_token = match state.current() {
        Ok(token) => token,
        Err(()) => return ParserResult::NotFound,
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
            symbol: Symbol::True | Symbol::False | Symbol::Nil | Symbol::Ellipse,
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
            } else if matches!(suffixes.last().unwrap(), ast::Suffix::Call(_)) {
                ParserResult::Value(Expression::FunctionCall(ast::FunctionCall {
                    prefix,
                    suffixes,
                }))
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

        TokenType::Symbol {
            symbol: Symbol::LeftBrace,
        } => {
            let left_brace = state.consume().unwrap();
            ParserResult::Value(ast::Expression::TableConstructor(force_table_constructor(
                state, left_brace,
            )))
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
        let Some(bin_op_precedence) = ast::BinOp::precedence_of_token(match state.current() {
            Ok(token) => token,
            Err(()) => return ParserResult::Value(lhs),
        }) else {
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
            let next_bin_op_token = match state.current() {
                Ok(token) => token,
                Err(()) => break,
            }

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
    const NO_TRAILING_COMMAS_ERROR: &str = "trailing commas in arguments are not allowed";

    let Some(left_parenthesis) = state.consume_if(Symbol::LeftParen) else {
        return ParserResult::NotFound;
    };

    let mut parameters = Punctuated::new();
    let right_parenthesis;

    let unfinished_function_body =
        |left_parenthesis: TokenReference, mut parameters: Punctuated<Parameter>| {
            if matches!(parameters.last(), Some(Pair::Punctuated(..))) {
                let last_parameter = parameters.pop().unwrap();
                parameters.push(Pair::End(last_parameter.into_value()));
            }

            ParserResult::Value(FunctionBody {
                parameters_parentheses: ContainedSpan::new(
                    left_parenthesis,
                    TokenReference::symbol(")").unwrap(),
                ),
                parameters,
                block: ast::Block::new(),
                end_token: TokenReference::symbol("end").unwrap(),
            })
        };

    loop {
        match state.current() {
            Ok(token) if token.is_symbol(Symbol::RightParen) => {
                right_parenthesis = state.consume().unwrap();
                break;
            }

            Ok(token) if token.is_symbol(Symbol::Ellipse) => {
                let ellipse = state.consume().unwrap();
                parameters.push(Pair::End(ast::Parameter::Ellipse(ellipse)));
                right_parenthesis = match state.require(Symbol::RightParen, "expected a `)`") {
                    Some(right_parenthesis) => right_parenthesis,
                    None => return unfinished_function_body(left_parenthesis, parameters),
                };

                break;
            }

            Ok(TokenReference {
                token:
                    Token {
                        token_type: TokenType::Identifier { .. },
                        ..
                    },
                ..
            }) => {
                let name = state.consume().unwrap();
                let name_parameter = ast::Parameter::Name(force_name(state, name).name);

                let Some(comma) = state.consume_if(Symbol::Comma) else {
                    parameters.push(Pair::End(name_parameter));

                    match state.require(Symbol::RightParen, "expected a `)`") {
                        Some(new_right_parenthesis) => {
                            right_parenthesis = new_right_parenthesis;
                            break;
                        }

                        None => return unfinished_function_body(left_parenthesis, parameters),
                    };
                };

                parameters.push(Pair::Punctuated(name_parameter, comma));
            }

            Ok(token) => {
                state.token_error(token.clone(), "expected a parameter name or `)`");

                return unfinished_function_body(left_parenthesis, parameters);
            }

            Err(()) => {
                return unfinished_function_body(left_parenthesis, parameters);
            }
        }
    }

    // rewrite todo: accept `,)`, and discard?
    if matches!(parameters.last(), Some(Pair::Punctuated(..))) {
        let last_parameter = parameters.pop().unwrap();

        state.token_error(
            last_parameter.punctuation().unwrap().clone(),
            NO_TRAILING_COMMAS_ERROR,
        );

        parameters.push(Pair::End(last_parameter.into_value()));
    }

    let (block, end) = match parse_block_with_end(state, &right_parenthesis) {
        Ok((block, end)) => (block, end),
        Err(()) => return ParserResult::LexerMoved,
    };

    ParserResult::Value(FunctionBody {
        parameters_parentheses: ContainedSpan::new(left_parenthesis, right_parenthesis),
        parameters,
        block,
        end_token: end,
    })
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
    let Ok(current_token) =  state.current() else {
        return ParserResult::NotFound;
    };

    match current_token.token_type() {
        TokenType::Identifier { .. } => {
            let name_token = state.consume().unwrap();
            ParserResult::Value(force_name(state, name_token))
        }

        _ => ParserResult::NotFound,
    }
}

fn force_name(_state: &mut ParserState, name: TokenReference) -> Name {
    Name { name }
}

fn one_or_more<T, F: Fn(&mut ParserState) -> ParserResult<T>>(
    state: &mut ParserState,
    parser: F,
    delimiter: Symbol,
) -> ParserResult<Punctuated<T>> {
    let mut values = Punctuated::new();

    loop {
        let value = match parser(state) {
            ParserResult::Value(value) => value,
            ParserResult::NotFound | ParserResult::LexerMoved => break,
        };

        match state.consume_if(delimiter) {
            Some(delimiter) => {
                values.push(Pair::Punctuated(value, delimiter));
            }

            None => {
                values.push(Pair::End(value));
                break;
            }
        }
    }

    if values.is_empty() {
        return ParserResult::NotFound;
    }

    if let Some(Pair::Punctuated(..)) = values.last() {
        let last_value = values.pop().unwrap();

        state.token_error(
            last_value.punctuation().unwrap().clone(),
            "trailing commas are not allowed",
        );

        values.push(Pair::End(last_value.into_value()));
    }

    ParserResult::Value(values)
}

fn parse_name_list(state: &mut ParserState) -> ParserResult<Punctuated<Name>> {
    one_or_more(state, parse_name, Symbol::Comma)
}

fn parse_expression_list(state: &mut ParserState) -> ParserResult<Punctuated<Expression>> {
    one_or_more(state, parse_expression, Symbol::Comma)
}
