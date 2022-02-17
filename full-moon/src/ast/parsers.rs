use super::{
    parser_util::{InternalAstError, Parser},
    span::ContainedSpan,
    *,
};

#[cfg(feature = "roblox")]
use super::types::*;

#[cfg(feature = "lua52")]
use super::lua52::*;

use crate::tokenizer::{TokenKind, TokenReference, TokenType};

use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
struct ParseSymbol(Symbol);

define_parser!(ParseSymbol, TokenReference, |this, state| {
    let expecting = TokenType::Symbol { symbol: this.0 };
    let token = state.peek();

    if *token.token_type() == expecting {
        Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            token.clone(),
        ))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseNumber;

define_parser!(ParseNumber, TokenReference, |_, state| {
    let token = state.peek();
    if token.token_kind() == TokenKind::Number {
        Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            token.clone(),
        ))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseStringLiteral;

define_parser!(ParseStringLiteral, TokenReference, |_, state| {
    let token = state.peek();
    if token.token_kind() == TokenKind::StringLiteral {
        Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            token.clone(),
        ))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParseBlock;
define_parser!(ParseBlock, Block, |_, state| {
    let mut stmts = Vec::new();
    while let Ok((new_state, stmt)) = keep_going!(ParseStmt.parse(state)) {
        state = new_state;
        let mut semicolon = None;

        if let Ok((new_state, new_semicolon)) = ParseSymbol(Symbol::Semicolon).parse(state) {
            state = new_state;
            semicolon = Some(new_semicolon);
        }

        stmts.push((stmt, semicolon));
    }

    if let Ok((mut state, last_stmt)) = keep_going!(ParseLastStmt.parse(state)) {
        let mut semicolon = None;

        if let Ok((new_state, new_semicolon)) = ParseSymbol(Symbol::Semicolon).parse(state) {
            state = new_state;
            semicolon = Some(new_semicolon)
        }

        Ok((
            state,
            Block {
                stmts,
                last_stmt: Some((last_stmt, semicolon)),
            },
        ))
    } else {
        Ok((
            state,
            Block {
                stmts,
                last_stmt: None,
            },
        ))
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseLastStmt;
define_parser!(
    ParseLastStmt,
    LastStmt,
    |_, state| if let Ok((state, token)) = ParseSymbol(Symbol::Return).parse(state) {
        let (state, returns) = expect!(
            state,
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
            "return values"
        );

        Ok((state, LastStmt::Return(Return { token, returns })))
    } else if let Ok((state, token)) = ParseSymbol(Symbol::Break).parse(state) {
        Ok((state, LastStmt::Break(token)))
    } else {
        cfg_if::cfg_if! {
            if #[cfg(feature = "roblox")] {
                let (state, continue_token) = ParseIdentifier.parse(state)?;
                if continue_token.token().to_string() == "continue" {
                    Ok((state, LastStmt::Continue(continue_token)))
                } else {
                    Err(InternalAstError::NoMatch)
                }
            } else {
                Err(InternalAstError::NoMatch)
            }
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseField;
define_parser!(ParseField, Field, |_, state| {
    if let Ok((state, start_bracket)) = ParseSymbol(Symbol::LeftBracket).parse(state) {
        let (state, key) = expect!(state, ParseExpression.parse(state), "expected key");
        let (state, end_bracket) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state),
            "expected ']'"
        );
        let (state, equal) = expect!(
            state,
            ParseSymbol(Symbol::Equal).parse(state),
            "expected '='"
        );
        let (state, value) = expect!(state, ParseExpression.parse(state), "expected value");

        return Ok((
            state,
            Field::ExpressionKey {
                brackets: ContainedSpan::new(start_bracket, end_bracket),
                key,
                equal,
                value,
            },
        ));
    } else if let Ok((state, key)) = keep_going!(ParseIdentifier.parse(state)) {
        if let Ok((state, equal)) = ParseSymbol(Symbol::Equal).parse(state) {
            let (state, value) = expect!(state, ParseExpression.parse(state), "expected value");

            return Ok((state, Field::NameKey { key, equal, value }));
        }
    }

    if let Ok((state, expr)) = keep_going!(ParseExpression.parse(state)) {
        return Ok((state, Field::NoKey(expr)));
    }

    Err(InternalAstError::NoMatch)
});

struct ParseTableConstructor;
define_parser!(ParseTableConstructor, TableConstructor, |_, state| {
    let (mut state, start_brace) = ParseSymbol(Symbol::LeftBrace).parse(state)?;
    let mut fields = Punctuated::new();

    while let Ok((new_state, field)) = keep_going!(ParseField.parse(state)) {
        let field_sep = if let Ok((new_state, separator)) =
            ParseSymbol(Symbol::Comma).parse(new_state)
        {
            state = new_state;
            Some(separator)
        } else if let Ok((new_state, separator)) = ParseSymbol(Symbol::Semicolon).parse(new_state) {
            state = new_state;
            Some(separator)
        } else {
            state = new_state;
            None
        };

        let is_end = field_sep.is_none();
        fields.push(Pair::new(field, field_sep));
        if is_end {
            break;
        }
    }

    let (state, end_brace) = expect!(
        state,
        ParseSymbol(Symbol::RightBrace).parse(state),
        "expected '}'"
    );

    Ok((
        state,
        TableConstructor {
            braces: ContainedSpan::new(start_brace, end_brace),
            fields,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseUnaryExpression;
define_parser!(ParseUnaryExpression, Expression, |_, state| {
    let (state, unop) = keep_going!(ParseUnOp.parse(state))?;
    let (state, expression) = expect!(
        state,
        ParseExpressionAtPrecedence(unop.precedence()).parse(state),
        "expected expression"
    );
    let expression = Box::new(expression);

    Ok((state, Expression::UnaryOperator { unop, expression }))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseParenExpression;
define_parser!(ParseParenExpression, Expression, |_, state| {
    let (state, left_paren) = ParseSymbol(Symbol::LeftParen).parse(state)?;
    let (state, expression) = expect!(state, ParseExpression.parse(state), "expected expression");

    let (state, right_paren) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state),
        "expected ')'"
    );

    Ok((
        state,
        Expression::Parentheses {
            contained: ContainedSpan::new(left_paren, right_paren),
            expression: Box::new(expression),
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseValueExpression;
define_parser!(ParseValueExpression, Expression, |_, state| {
    let (state, value) = keep_going!(ParseValue.parse(state))?;
    #[cfg(feature = "roblox")]
    let (state, type_assertion) =
        if let Ok((state, type_assertion)) = keep_going!(ParseTypeAssertion.parse(state)) {
            (state, Some(type_assertion))
        } else {
            (state, None)
        };

    let value = Box::new(value);

    Ok((
        state,
        Expression::Value {
            value,
            #[cfg(feature = "roblox")]
            type_assertion,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParsePartExpression;
define_parser!(ParsePartExpression, Expression, |_, state| {
    if let Ok((state, expression)) = keep_going!(ParseUnaryExpression.parse(state)) {
        Ok((state, expression))
    } else if let Ok((state, expression)) = keep_going!(ParseValueExpression.parse(state)) {
        Ok((state, expression))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseExpressionAtPrecedence(u8);
define_parser!(ParseExpressionAtPrecedence, Expression, |this, state| {
    let min_precedence = this.0;
    let (mut state, mut current_expr) = ParsePartExpression.parse(state)?;

    // See if we can find a Binary Operator
    while let Ok((next_state, operator)) = ParseBinOp.parse(state) {
        if operator.precedence() < min_precedence {
            break;
        }

        let next_min_precedence = if operator.is_right_associative() {
            operator.precedence()
        } else {
            operator.precedence() + 1
        };

        let (next_state, rhs) = expect!(
            next_state,
            ParseExpressionAtPrecedence(next_min_precedence).parse(next_state),
            "expected expression"
        );
        state = next_state;
        current_expr = Expression::BinaryOperator {
            lhs: Box::new(current_expr),
            binop: operator,
            rhs: Box::new(rhs),
        };
    }

    Ok((state, current_expr))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(ParseExpression, Expression, |_, state| {
    ParseExpressionAtPrecedence(1).parse(state)
});

#[derive(Clone, Debug, PartialEq)]
struct ParseTypeAssertion;

#[rustfmt::skip]
define_roblox_parser!(
    ParseTypeAssertion,
    TypeAssertion,
    TokenReference,
    |_, state| {
        let (state, assertion_op) = ParseSymbol(Symbol::TwoColons).parse(state)?;
        let (state, cast_to) = expect!(
            state,
            ParseTypeInfo(TypeInfoContext::None).parse(state),
            "expected type in type assertion"
        );

        Ok((state, TypeAssertion { assertion_op, cast_to }))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseValue;
define_parser!(ParseValue, Value, |_, state| parse_first_of!(state, {
    ParseSymbol(Symbol::Nil) => Value::Symbol,
    ParseSymbol(Symbol::False) => Value::Symbol,
    ParseSymbol(Symbol::True) => Value::Symbol,
    ParseNumber => Value::Number,
    ParseStringLiteral => Value::String,
    ParseSymbol(Symbol::Ellipse) => Value::Symbol,
    ParseFunction => Value::Function,
    ParseTableConstructor => Value::TableConstructor,
    ParseFunctionCall => Value::FunctionCall,
    ParseVar => Value::Var,
    ParseParenExpression => Value::ParenthesesExpression,
    @#[cfg(feature = "roblox")]
    ParseIfExpression => Value::IfExpression,
}));

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseStmt;
define_parser!(ParseStmt, Stmt, |_, state| parse_first_of!(state, {
    ParseAssignment => Stmt::Assignment,
    ParseFunctionCall => Stmt::FunctionCall,
    ParseDo => Stmt::Do,
    ParseWhile => Stmt::While,
    ParseRepeat => Stmt::Repeat,
    ParseIf => Stmt::If,
    ParseNumericFor => Stmt::NumericFor,
    ParseGenericFor => Stmt::GenericFor,
    ParseFunctionDeclaration => Stmt::FunctionDeclaration,
    ParseLocalFunction => Stmt::LocalFunction,
    ParseLocalAssignment => Stmt::LocalAssignment,
    @#[cfg(feature = "roblox")]
    ParseCompoundAssignment => Stmt::CompoundAssignment,
    @#[cfg(feature = "roblox")]
    ParseExportedTypeDeclaration => Stmt::ExportedTypeDeclaration,
    @#[cfg(feature = "roblox")]
    ParseTypeDeclaration => Stmt::TypeDeclaration,
    @#[cfg(feature = "lua52")]
    ParseGoto => Stmt::Goto,
    @#[cfg(feature = "lua52")]
    ParseLabel => Stmt::Label,
}));

#[derive(Clone, Debug, PartialEq)]
struct ParsePrefix;
define_parser!(ParsePrefix, Prefix, |_, state| parse_first_of!(state, {
    ParseParenExpression => Prefix::Expression,
    ParseIdentifier => Prefix::Name,
}));

struct ParseIndex;
define_parser!(
    ParseIndex,
    Index,
    |_, state| if let Ok((state, start_bracket)) = ParseSymbol(Symbol::LeftBracket).parse(state) {
        let (state, expression) =
            expect!(state, ParseExpression.parse(state), "expected expression");
        let (state, end_bracket) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state),
            "expected ']'"
        );
        Ok((
            state,
            Index::Brackets {
                brackets: ContainedSpan::new(start_bracket, end_bracket),
                expression,
            },
        ))
    } else if let Ok((state, dot)) = ParseSymbol(Symbol::Dot).parse(state) {
        let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected name");
        Ok((state, Index::Dot { dot, name }))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionArgs;
define_parser!(
    ParseFunctionArgs,
    FunctionArgs,
    |_, state| if let Ok((state, left_paren)) =
        keep_going!(ParseSymbol(Symbol::LeftParen).parse(state))
    {
        let (state, arguments) = expect!(
            state,
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected arguments"
        );
        let (state, right_paren) = expect!(
            state,
            ParseSymbol(Symbol::RightParen).parse(state),
            "expected ')'"
        );
        Ok((
            state,
            FunctionArgs::Parentheses {
                arguments,
                parentheses: ContainedSpan::new(left_paren, right_paren),
            },
        ))
    } else if let Ok((state, table_constructor)) = keep_going!(ParseTableConstructor.parse(state)) {
        Ok((state, FunctionArgs::TableConstructor(table_constructor)))
    } else if let Ok((state, string)) = keep_going!(ParseStringLiteral.parse(state)) {
        Ok((state, FunctionArgs::String(string)))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor, |_, state| {
    let (mut state, for_token) = ParseSymbol(Symbol::For).parse(state)?;

    let index_variable;

    #[cfg(feature = "roblox")]
    let type_specifier;

    cfg_if::cfg_if! {
        if #[cfg(feature = "roblox")] {
            let (new_state, (new_index_variable, new_type_specifier)) =
                expect!(state, ParseNameWithType.parse(state), "expected names");

            state = new_state;
            index_variable = new_index_variable;
            type_specifier = new_type_specifier;
        } else {
            let (new_state, new_index_variable) =
                expect!(state, ParseIdentifier.parse(state), "expected names");

            state = new_state;
            index_variable = new_index_variable;
        }
    }

    let (state, equal_token) = ParseSymbol(Symbol::Equal).parse(state)?; // Numeric fors run before generic fors, so we can't guarantee this
    let (state, start) = expect!(
        state,
        ParseExpression.parse(state),
        "expected start expression"
    );
    let (state, start_end_comma) = expect!(
        state,
        ParseSymbol(Symbol::Comma).parse(state),
        "expected comma"
    );
    let (state, end) = expect!(
        state,
        ParseExpression.parse(state),
        "expected end expression"
    );
    let (state, step, end_step_comma) =
        if let Ok((state, comma)) = ParseSymbol(Symbol::Comma).parse(state) {
            let (state, expression) = expect!(
                state,
                ParseExpression.parse(state),
                "expected limit expression"
            );
            (state, Some(expression), Some(comma))
        } else {
            (state, None, None)
        };
    let (state, do_token) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        NumericFor {
            for_token,
            index_variable,
            equal_token,
            start,
            start_end_comma,
            end,
            end_step_comma,
            step,
            do_token,
            block,
            end_token,
            #[cfg(feature = "roblox")]
            type_specifier,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseGenericFor;
define_parser!(ParseGenericFor, GenericFor, |_, state| {
    let (mut state, for_token) = ParseSymbol(Symbol::For).parse(state)?;

    let mut names;
    let mut type_specifiers = Vec::new();

    if cfg!(feature = "roblox") {
        names = Punctuated::new();

        let (new_state, full_name_list) = expect!(
            state,
            OneOrMore(ParseNameWithType, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected names"
        );

        for mut pair in full_name_list.into_pairs() {
            type_specifiers.push(pair.value_mut().1.take());
            names.push(pair.map(|(name, _)| name));
        }

        state = new_state;
    } else {
        let (new_state, new_names) = expect!(
            state,
            OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected names"
        );

        state = new_state;
        names = new_names;
    }

    let (state, in_token) = expect!(state, ParseSymbol(Symbol::In).parse(state), "expected 'in'"); // Numeric fors run before here, so there has to be an in
    let (state, expr_list) = expect!(
        state,
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected expression"
    );
    let (state, do_token) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        GenericFor {
            for_token,
            names,
            in_token,
            expr_list,
            do_token,
            block,
            end_token,
            #[cfg(feature = "roblox")]
            type_specifiers,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseIf;
define_parser!(ParseIf, If, |_, state| {
    let (state, if_token) = ParseSymbol(Symbol::If).parse(state)?;
    let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
    let (state, then_token) = expect!(
        state,
        ParseSymbol(Symbol::Then).parse(state),
        "expected 'then'"
    );
    let (mut state, block) = expect!(state, ParseBlock.parse(state), "expected block");

    let mut else_ifs = Vec::new();
    while let Ok((new_state, else_if_token)) = ParseSymbol(Symbol::ElseIf).parse(state) {
        let (new_state, condition) = expect!(
            state,
            ParseExpression.parse(new_state),
            "expected condition"
        );
        let (new_state, then_token) = expect!(
            state,
            ParseSymbol(Symbol::Then).parse(new_state),
            "expected 'then'"
        );
        let (new_state, block) = expect!(state, ParseBlock.parse(new_state), "expected block");
        state = new_state;
        else_ifs.push(ElseIf {
            else_if_token,
            condition,
            then_token,
            block,
        });
    }

    let (state, else_token, r#else) =
        if let Ok((state, else_token)) = ParseSymbol(Symbol::Else).parse(state) {
            let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
            (state, Some(else_token), Some(block))
        } else {
            (state, None, None)
        };

    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        If {
            if_token,
            condition,
            then_token,
            block,
            else_token,
            r#else,
            else_if: if else_ifs.is_empty() {
                None
            } else {
                Some(else_ifs)
            },
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseWhile;
define_parser!(ParseWhile, While, |_, state| {
    let (state, while_token) = ParseSymbol(Symbol::While).parse(state)?;
    let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
    let (state, do_token) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        While {
            while_token,
            condition,
            do_token,
            block,
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseRepeat;
define_parser!(ParseRepeat, Repeat, |_, state| {
    let (state, repeat_token) = ParseSymbol(Symbol::Repeat).parse(state)?;
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, until_token) = expect!(
        state,
        ParseSymbol(Symbol::Until).parse(state),
        "expected 'until'"
    );
    let (state, until) = expect!(state, ParseExpression.parse(state), "expected condition");
    Ok((
        state,
        Repeat {
            repeat_token,
            block,
            until_token,
            until,
        },
    ))
});

struct ParseMethodCall;
define_parser!(ParseMethodCall, MethodCall, |_, state| {
    let (state, colon_token) = ParseSymbol(Symbol::Colon).parse(state)?;
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected method");
    let (state, args) = expect!(state, ParseFunctionArgs.parse(state), "expected args");
    Ok((
        state,
        MethodCall {
            colon_token,
            name,
            args,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseCall;
define_parser!(ParseCall, Call, |_, state| parse_first_of!(state, {
    ParseFunctionArgs => Call::AnonymousCall,
    ParseMethodCall => Call::MethodCall,
}));

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionBody;
#[rustfmt::skip]
define_parser!(ParseFunctionBody, FunctionBody, |_, state| {
    #[cfg(feature = "roblox")]
    let (state, generics) =
        if let Ok((state, generics)) = keep_going!(ParseGenericDeclaration.parse(state)) {
            (state, Some(generics))
        } else {
            (state, None)
        };

    let (mut state, start_parenthese) = expect!(
        state,
        ParseSymbol(Symbol::LeftParen).parse(state),
        "expected '('"
    );

    let mut parameters = Punctuated::new();
    let mut name_list = None;
    let mut type_specifiers = Vec::new();

    if cfg!(feature = "roblox") {
        if let Ok((new_state, full_name_list)) = keep_going!(
            OneOrMore(ParseNameWithType, ParseSymbol(Symbol::Comma), false).parse(state)
        ) {
            let mut new_name_list = Punctuated::new();

            for mut pair in full_name_list.into_pairs() {
                type_specifiers.push(pair.value_mut().1.take());
                new_name_list.push(pair.map(|(name, _)| name));
            }

            state = new_state;
            name_list = Some(new_name_list);
        }
    } else if let Ok((new_state, new_name_list)) = keep_going!(
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state)
    ) {
        state = new_state;
        name_list = Some(new_name_list);
    }

    if let Some(names) = name_list {
        parameters.extend(names.into_pairs().map(|pair| {
            let tuple = pair.into_tuple();
            Pair::new(Parameter::Name(tuple.0), tuple.1)
        }));

        if let Ok((new_state, comma)) = ParseSymbol(Symbol::Comma).parse(state) {
            if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(new_state) {
                state = new_state;

                let mut last_parameter = parameters.pop().expect("comma parsed and accepted, but no arguments before it?");
                last_parameter = Pair::new(last_parameter.into_value(), Some(comma));
                parameters.push(last_parameter);

                parameters.push(Pair::new(Parameter::Ellipse(ellipse), None));

                // Parse ellipse type if in Luau
                if cfg!(feature = "roblox") {
                    if let Ok((new_state, type_specifier)) = ParseTypeSpecifier(TypeInfoContext::VarArgSpecifier).parse(state) {
                        state = new_state;
                        type_specifiers.push(Some(type_specifier));
                    } else {
                        type_specifiers.push(None);
                    }
                }
            }
        }
    } else if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
        state = new_state;
        parameters.push(Pair::new(Parameter::Ellipse(ellipse), None));

        // Parse ellipse type if in Luau
        if cfg!(feature = "roblox") {
            if let Ok((new_state, type_specifier)) = ParseTypeSpecifier(TypeInfoContext::VarArgSpecifier).parse(state) {
                state = new_state;
                type_specifiers.push(Some(type_specifier));
            } else {
                type_specifiers.push(None);
            }
        }
    }

    let (state, end_parenthese) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state),
        "expected ')'"
    );

    #[cfg_attr(not(feature = "roblox"), allow(unused_variables))]
    let (state, return_type) = if let Ok((state, return_type)) = ParseTypeSpecifier(TypeInfoContext::ReturnType).parse(state) {
        (state, Some(return_type))
    } else {
        (state, None)
    };

    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        FunctionBody {
            #[cfg(feature = "roblox")]
            generics,
            parameters_parentheses: ContainedSpan::new(start_parenthese, end_parenthese),
            parameters,
            block,
            end_token,
            #[cfg(feature = "roblox")]
            type_specifiers,
            #[cfg(feature = "roblox")]
            return_type,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunction;
define_parser!(ParseFunction, (TokenReference, FunctionBody), |_, state| {
    let (state, token) = ParseSymbol(Symbol::Function).parse(state)?;
    let (state, body) = expect!(
        state,
        ParseFunctionBody.parse(state),
        "expected function body"
    );
    Ok((state, (token, body)))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseSuffix;
define_parser!(ParseSuffix, Suffix, |_, state| parse_first_of!(state, {
    ParseCall => Suffix::Call,
    ParseIndex => Suffix::Index,
}));

#[derive(Clone, Debug, PartialEq)]
struct ParseVarExpression;
define_parser!(ParseVarExpression, VarExpression, |_, state| {
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

    if let Some(Suffix::Index(_)) = suffixes.last() {
        Ok((state, VarExpression { prefix, suffixes }))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseVar;
define_parser!(ParseVar, Var, |_, state| parse_first_of!(state, {
    ParseVarExpression => Var::Expression,
    ParseIdentifier => Var::Name,
}));

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseAssignment;
define_parser!(ParseAssignment, Assignment, |_, state| {
    let (state, var_list) = OneOrMore(ParseVar, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, equal_token) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, expr_list) = expect!(
        state,
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected values"
    );

    Ok((
        state,
        Assignment {
            var_list,
            equal_token,
            expr_list,
        },
    ))
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalFunction;
define_parser!(ParseLocalFunction, LocalFunction, |_, state| {
    let (state, local_token) = ParseSymbol(Symbol::Local).parse(state)?;
    let (state, function_token) = ParseSymbol(Symbol::Function).parse(state)?;
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected name");
    let (state, body) = ParseFunctionBody.parse(state)?;
    Ok((
        state,
        LocalFunction {
            local_token,
            function_token,
            name,
            body,
        },
    ))
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalAssignment;
define_parser!(ParseLocalAssignment, LocalAssignment, |_, state| {
    let (mut state, local_token) = ParseSymbol(Symbol::Local).parse(state)?;

    let mut name_list;
    let mut type_specifiers = Vec::new();

    if cfg!(feature = "roblox") {
        name_list = Punctuated::new();

        let (new_state, full_name_list) = expect!(
            state,
            OneOrMore(ParseNameWithType, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected name"
        );

        for mut pair in full_name_list.into_pairs() {
            type_specifiers.push(pair.value_mut().1.take());
            name_list.push(pair.map(|(name, _)| name));
        }

        state = new_state;
    } else {
        let (new_state, new_name_list) = expect!(
            state,
            OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected name"
        );

        state = new_state;
        name_list = new_name_list;
    }

    let ((state, expr_list), equal_token) = match ParseSymbol(Symbol::Equal).parse(state) {
        Ok((state, equal_token)) => (
            OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false)
                .parse(state)
                .map_err(|_| InternalAstError::UnexpectedToken {
                    token: (*state.peek()).to_owned(),
                    additional: Some(Cow::from("expected expression")),
                })?,
            Some(equal_token),
        ),
        Err(InternalAstError::NoMatch) => ((state, Punctuated::new()), None),
        Err(other) => return Err(other),
    };

    Ok((
        state,
        LocalAssignment {
            local_token,
            #[cfg(feature = "roblox")]
            type_specifiers,
            name_list,
            equal_token,
            expr_list,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseDo;
define_parser!(ParseDo, Do, |_, state| {
    let (state, do_token) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        Do {
            do_token,
            block,
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionCall;
define_parser!(ParseFunctionCall, FunctionCall, |_, state| {
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

    if let Some(Suffix::Call(_)) = suffixes.last() {
        Ok((state, FunctionCall { prefix, suffixes }))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionName;
define_parser!(ParseFunctionName, FunctionName, |_, state| {
    let (state, names) =
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Dot), false).parse(state)?;
    let (state, colon_name) = if let Ok((state, colon)) = ParseSymbol(Symbol::Colon).parse(state) {
        let (state, colon_name) =
            expect!(state, ParseIdentifier.parse(state), "expected method name");
        (state, Some((colon, colon_name)))
    } else {
        (state, None)
    };

    Ok((state, FunctionName { names, colon_name }))
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseFunctionDeclaration;
define_parser!(ParseFunctionDeclaration, FunctionDeclaration, |_, state| {
    let (state, function_token) = ParseSymbol(Symbol::Function).parse(state)?;
    let (state, name) = expect!(
        state,
        ParseFunctionName.parse(state),
        "expected function name"
    );
    let (state, body) = expect!(
        state,
        ParseFunctionBody.parse(state),
        "expected function body"
    );
    Ok((
        state,
        FunctionDeclaration {
            function_token,
            name,
            body,
        },
    ))
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseIdentifier;
#[rustfmt::skip]
define_parser!(ParseIdentifier, TokenReference, |_, state| {
    let next_token = state.peek();
    match next_token.token_kind() {
        TokenKind::Identifier => Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            next_token.clone(),
        )),
        _ => Err(InternalAstError::NoMatch),
    }
});

#[derive(Clone, Copy, Debug, PartialEq)]
enum TypeInfoContext {
    /// A standard type info, with no context
    #[cfg(feature = "roblox")]
    None,
    /// A type inside of parentheses, either for the parameters in a `TypeInfo::Callback`, or for a `TypeInfo::Tuple`
    /// Variadic type infos are only permitted inside of here
    #[cfg(feature = "roblox")]
    ParenthesesType,
    /// The return type of a function declaration or callback type, such as `function foo(bar) -> number`.
    /// In addition to standard types, type packs/variadic type packs are allowed here
    ReturnType,
    /// A type specifier for the variadic argument `...` in a function definition parameter list
    /// In these cases, we are allowed a generic variadic pack `T...` to be specified
    VarArgSpecifier,
    /// An argument passed into a generic type, e.g. `string` or `...number` in `type X = Foo<string, ...number>`.
    /// In these cases, generic type packs, variadic type packs, and explicit type packs (using parentheses) are allowed
    #[cfg(feature = "roblox")]
    GenericArgument,
}

// Roblox Types
#[derive(Clone, Debug, PartialEq)]
struct ParseNameWithType;
define_roblox_parser!(
    ParseNameWithType,
    (TokenReference, Option<TypeSpecifier>),
    (TokenReference, Option<TokenReference>),
    |_, state| {
        let (state, name) = ParseIdentifier.parse(state)?;
        let (state, type_specifier) = if let Ok((state, type_specifier)) =
            keep_going!(ParseTypeSpecifier(TypeInfoContext::None).parse(state))
        {
            (state, Some(type_specifier))
        } else {
            (state, None)
        };

        Ok((state, (name, type_specifier)))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseTypeSpecifier(TypeInfoContext);
define_roblox_parser!(
    ParseTypeSpecifier,
    TypeSpecifier,
    TokenReference,
    |this, state| {
        let (state, punctuation) = ParseSymbol(Symbol::Colon).parse(state)?;
        let (state, type_info) = expect!(
            state,
            ParseTypeInfo(this.0).parse(state),
            "expected type after colon"
        );

        Ok((
            state,
            TypeSpecifier {
                punctuation,
                type_info,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseGenericDeclaration;
define_roblox_parser!(
    ParseGenericDeclaration,
    GenericDeclaration,
    TokenReference,
    |_, state| {
        let (state, start_arrow) = ParseSymbol(Symbol::LessThan).parse(state)?;
        let mut generics: Punctuated<GenericDeclarationParameter> = Punctuated::new();

        let mut have_seen_pack = false; // We have seen a generic type pack parameter. We can only now accept generic type pack parameters
        let mut have_seen_default = false; // We have seen a default type, all future parameters must define a default type

        let mut loop_state = state;
        loop {
            let (state, name) = expect!(
                loop_state,
                ParseIdentifier.parse(loop_state),
                "expected name"
            );

            // Look to see if is a type pack
            let (state, ellipse) = if let Ok((state, ellipse)) =
                keep_going!(ParseSymbol(Symbol::Ellipse).parse(state))
            {
                have_seen_pack = true;
                (state, Some(ellipse))
            } else {
                // Must only be type packs after we have seen one
                if have_seen_pack {
                    return Err(InternalAstError::UnexpectedToken {
                        token: state.peek().clone(),
                        additional: Some("generic types come before generic type packs".into()),
                    });
                };

                (state, None)
            };

            // Look to see if a default type has been specified
            let (state, default) =
                if let Ok((state, equals)) = keep_going!(ParseSymbol(Symbol::Equal).parse(state)) {
                    have_seen_default = true;

                    // If we are parsing a type pack, allow type pack default types
                    let parse_context = if ellipse.is_some() {
                        TypeInfoContext::GenericArgument
                    } else {
                        TypeInfoContext::None
                    };

                    let (state, type_info) = expect!(
                        state,
                        ParseTypeInfo(parse_context).parse(state),
                        "expected type after `=`"
                    );

                    (state, Some((equals, type_info)))
                } else {
                    // Defaults must always be specified once one has been
                    if have_seen_default {
                        return Err(InternalAstError::UnexpectedToken {
                            token: state.peek().clone(),
                            additional: Some("expected default type after type name".into()),
                        });
                    }
                    (state, None)
                };

            let parameter = match ellipse {
                Some(ellipse) => GenericParameterInfo::Variadic { name, ellipse },
                None => GenericParameterInfo::Name(name),
            };
            let declaration_parameter = GenericDeclarationParameter { parameter, default };

            if let Ok((state, punctuation)) = keep_going!(ParseSymbol(Symbol::Comma).parse(state)) {
                generics.push(Pair::Punctuated(declaration_parameter, punctuation));
                loop_state = state;
            } else {
                generics.push(Pair::End(declaration_parameter));
                loop_state = state;
                break;
            }
        }

        let (state, end_arrow) = expect!(
            loop_state,
            ParseSymbol(Symbol::GreaterThan).parse(loop_state),
            "expected `>` to match `<`"
        );

        Ok((
            state,
            GenericDeclaration {
                arrows: ContainedSpan::new(start_arrow, end_arrow),
                generics,
            },
        ))
    }
);

cfg_if::cfg_if! {
    if #[cfg(feature = "roblox")] {
        // Roblox Compound Assignment
        #[derive(Clone, Debug, Default, PartialEq)]
        struct ParseCompoundAssignment;
        define_parser!(
            ParseCompoundAssignment,
            CompoundAssignment,
            |_, state| {
                let (state, lhs) = ParseVar.parse(state)?;
                let (state, compound_operator) = ParseCompoundOp.parse(state)?;
                let (state, rhs) = expect!(
                    state,
                    ParseExpression.parse(state),
                    "expected value"
                );

                Ok((
                    state,
                    CompoundAssignment {
                        lhs,
                        compound_operator,
                        rhs,
                    },
                ))
            }
        );

        // Roblox If Expression
        #[derive(Clone, Debug, Default, PartialEq)]
        struct ParseIfExpression;
        define_parser!(
            ParseIfExpression,
            IfExpression,
            |_, state| {
                let (state, if_token) = ParseSymbol(Symbol::If).parse(state)?;
                let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
                let (state, then_token) = expect!(state, ParseSymbol(Symbol::Then).parse(state), "expected `then`");
                let (mut state, if_expression) = expect!(state, ParseExpression.parse(state), "expected expression");

                let mut else_if_expressions = Vec::new();
                while let Ok((new_state, else_if_token)) = ParseSymbol(Symbol::ElseIf).parse(state) {
                    let (new_state, condition) = expect!(
                        state,
                        ParseExpression.parse(new_state),
                        "expected condition"
                    );
                    let (new_state, then_token) = expect!(
                        state,
                        ParseSymbol(Symbol::Then).parse(new_state),
                        "expected 'then'"
                    );
                    let (new_state, expression) = expect!(state, ParseExpression.parse(new_state), "expected expression");
                    state = new_state;
                    else_if_expressions.push(ElseIfExpression {
                        else_if_token,
                        condition,
                        then_token,
                        expression,
                    });
                }

                let (state, else_token) = expect!(state, ParseSymbol(Symbol::Else).parse(state), "expected `else` in if expression");
                let (state, else_expression) = expect!(state, ParseExpression.parse(state), "expected expression");

                Ok((
                    state,
                    IfExpression {
                        if_token,
                        condition,
                        then_token,
                        if_expression,
                        else_if_expressions: if else_if_expressions.is_empty() { None } else { Some(else_if_expressions) },
                        else_token,
                        else_expression,
                    },
                ))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeDeclaration;
        define_parser!(
            ParseTypeDeclaration,
            TypeDeclaration,
            |_, state| {
                let (state, type_token) = ParseIdentifier.parse(state)?;
                if type_token.token().to_string() != "type" {
                    return Err(InternalAstError::NoMatch);
                }

                let (state, base) = ParseIdentifier.parse(state)?;

                let (state, generics) = if let Ok((state, generics)) =
                    keep_going!(ParseGenericDeclaration.parse(state))
                {
                    (state, Some(generics))
                } else {
                    (state, None)
                };

                let (state, equal_token) = expect!(
                    state,
                    ParseSymbol(Symbol::Equal).parse(state),
                    "expected `=` while parsing type alias"
                );

                let (state, declare_as) =
                    expect!(state, ParseTypeInfo(TypeInfoContext::None).parse(state), "expected type");

                Ok((
                    state,
                    TypeDeclaration {
                        type_token,
                        base,
                        generics,
                        equal_token,
                        declare_as,
                    },
                ))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseExportedTypeDeclaration;
        define_parser!(
            ParseExportedTypeDeclaration,
            ExportedTypeDeclaration,
            |_, state| {
                let (state, export_token) = ParseIdentifier.parse(state)?;
                if export_token.token().to_string() != "export" {
                    return Err(InternalAstError::NoMatch);
                }

                let (state, type_declaration) =
                    expect!(state, ParseTypeDeclaration.parse(state), "expected type declaration");

                Ok((
                    state,
                    ExportedTypeDeclaration {
                        export_token,
                        type_declaration
                    },
                ))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseIndexedTypeInfo;
        define_parser!(ParseIndexedTypeInfo, IndexedTypeInfo, |_, state| {
            let (state, base_type) = if let Ok((state, identifier)) = {
                ParseIdentifier.parse(state)
            } {
                if let Ok((state, start_arrow)) = ParseSymbol(Symbol::LessThan).parse(state)
                {
                    let (state, generics) = expect!(
                        state,
                        OneOrMore(ParseTypeInfo(TypeInfoContext::GenericArgument), ParseSymbol(Symbol::Comma), false).parse(state),
                        "expected type parameters"
                    );

                    let (state, end_arrow) = expect!(
                        state,
                        ParseSymbol(Symbol::GreaterThan).parse(state),
                        "expected `>` to close `<`"
                    );

                    (
                        state,
                        IndexedTypeInfo::Generic {
                            base: identifier,
                            arrows: ContainedSpan::new(start_arrow, end_arrow),
                            generics,
                        },
                    )
                } else {
                    (state, IndexedTypeInfo::Basic(identifier))
                }
            } else {
                return Err(InternalAstError::NoMatch);
            };

            Ok((state, base_type))
        });

        #[derive(Clone, Debug, PartialEq)]
        /// A parentheses type info atom, such as `(string)` in `type Foo = (string)?`. This excludes parentheses used to indicate
        /// a return type, or arguments in a callback type. An opening parentheses should have already been consumed,
        /// and is passed to this struct.
        struct ParseParenthesesTypeInfo(TokenReference);
        define_parser!(ParseParenthesesTypeInfo, TypeInfo, |this, state| {
            let (state, type_info) = expect!(state, ParseTypeInfo(TypeInfoContext::None).parse(state), "expected type within parentheses");
            let (state, end_parenthese) = expect!(state, ParseSymbol(Symbol::RightParen).parse(state), "expected `)`");

            // TODO: should we separate out a single type inside parentheses into a TypeInfo::Parentheses?
            let mut types = Punctuated::new();
            types.push(Pair::new(type_info, None));

            Ok((state, TypeInfo::Tuple {
                parentheses: ContainedSpan::new(this.0.clone(), end_parenthese),
                types,
            }))
        });

        #[derive(Clone, Debug, PartialEq)]
        /// A tuple type info, such as `(string, foo)` in `(string) -> (string, foo)`.
        /// This is only used for return types. An opening parentheses should have already been consumed,
        /// and is passed to this struct.
        struct ParseTupleTypeInfo(TokenReference);
        define_parser!(ParseTupleTypeInfo, TypeInfo, |this, state| {
            let (state, types) = expect!(
                state,
                ZeroOrMoreDelimited(ParseTypeInfo(TypeInfoContext::ParenthesesType), ParseSymbol(Symbol::Comma), false)
                    .parse(state),
                "expected types within parentheses"
            );
            let (state, end_parenthese) = expect!(state, ParseSymbol(Symbol::RightParen).parse(state), "expected `)`");

            Ok((state, TypeInfo::Tuple {
                parentheses: ContainedSpan::new(this.0.clone(), end_parenthese),
                types,
            }))
        });

        /// A type array atom, such as `{ string }`.
        /// An opening brace should have already been consumed, and is passed to this struct.
        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeArray(TokenReference);
        define_parser!(ParseTypeArray, TypeInfo, |this, state| {
            let (state, type_info) = expect!(
                state,
                ParseTypeInfo(TypeInfoContext::None).parse(state),
                "expected type in array"
            );

            let (state, end_brace) = expect!(
                state,
                ParseSymbol(Symbol::RightBrace).parse(state),
                "expected `}` to match `{`"
            );

            Ok((
                state,
                TypeInfo::Array {
                    braces: ContainedSpan::new(this.0.clone(), end_brace),
                    type_info: Box::new(type_info)
                },
            ))
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeArgument;
        define_parser!(ParseTypeArgument, TypeArgument, |_, state| {
            // Attempt to parse an identifier and then a `:`
            if let Ok((new_state, identifier)) = ParseIdentifier.parse(state) {
                if let Ok((state, colon)) = ParseSymbol(Symbol::Colon).parse(new_state) {
                    let (state, type_info) = expect!(state, ParseTypeInfo(TypeInfoContext::None).parse(state), "expected type");
                    return Ok((
                        state,
                        TypeArgument {
                            name: Some((identifier, colon)),
                            type_info
                        }
                    ))
                }
            };

            // If failed, fall back to original state and parse a type_info normally
            let (state, type_info) = ParseTypeInfo(TypeInfoContext::ParenthesesType).parse(state)?;
            Ok((
                state,
                TypeArgument {
                    name: None,
                    type_info,
                }
            ))
        });

        #[derive(Clone, Debug, PartialEq)]
        /// A callback type info atom, such as `(count: number) -> string` in `type Foo = (count: number) -> string`.
        /// An opening parentheses should have already been consumed, and is passed to this struct.
        struct ParseCallbackTypeInfo(TokenReference, Option<GenericDeclaration>);
        define_parser!(ParseCallbackTypeInfo, TypeInfo, |this, state| {
            let (state, types) = expect!(
                state,
                ZeroOrMoreDelimited(ParseTypeArgument, ParseSymbol(Symbol::Comma), false)
                    .parse(state),
                "expected types within parentheses"
            );

            let (state, end_parenthese) = expect!(
                state,
                ParseSymbol(Symbol::RightParen).parse(state),
                "expected `)` to match `(`"
            );

            let (state, arrow) = expect!(state, ParseSymbol(Symbol::ThinArrow).parse(state), "expected `->` after `()` when parsing function type");
            let (state, return_value) = expect!(
                state,
                ParseTypeInfo(TypeInfoContext::ReturnType).parse(state),
                "expected return type after `->`"
            );

            Ok((
                state,
                TypeInfo::Callback {
                    generics: this.1.clone(),
                    arguments: types,
                    parentheses: ContainedSpan::new(this.0.clone(), end_parenthese),
                    arrow,
                    return_type: Box::new(return_value),
                },
            ))
        });

        /// A type table atom, such as `{ foo: string, bar: number }`.
        /// An opening brace should have already been consumed, and is passed to this struct.
        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeTable(TokenReference);
        define_parser!(ParseTypeTable, TypeInfo, |this, state| {
            let mut state = state;
            let mut fields = Punctuated::new();

            while let Ok((new_state, field)) = keep_going!(ParseTypeField.parse(state)) {
                let field_sep = if let Ok((new_state, separator)) =
                    ParseSymbol(Symbol::Comma).parse(new_state)
                {
                    state = new_state;
                    Some(separator)
                } else if let Ok((new_state, separator)) = ParseSymbol(Symbol::Semicolon).parse(new_state) {
                    state = new_state;
                    Some(separator)
                } else {
                    state = new_state;
                    None
                };

                let is_end = field_sep.is_none();
                fields.push(Pair::new(field, field_sep));
                if is_end {
                    break;
                }
            }

            let (state, end_brace) = expect!(
                state,
                ParseSymbol(Symbol::RightBrace).parse(state),
                "expected `}` to match `{`"
            );

            Ok((state, TypeInfo::Table {
                braces: ContainedSpan::new(this.0.clone(), end_brace),
                fields,
            }))
        });

        // A type info atom, excluding compound types such as Union and Intersection
        #[derive(Clone, Debug, PartialEq)]
        struct ParseSingleTypeInfo(TypeInfoContext);
        define_parser!(ParseSingleTypeInfo, TypeInfo, |this, state| {
            // Singleton type info: `"yes" | "no"` in `(string) -> "yes" | "no"`, `"$$typeof"` in `{ ["$$typeof"]: number }`,
            let (state, base_type) = if let Ok((state, string_singleton)) = ParseStringLiteral.parse(state) {
                (state, TypeInfo::String(string_singleton))
            // Singleton type info: `true` in `type X = Error & { handled: true }`
            } else if let Ok((state, true_singleton)) = ParseSymbol(Symbol::True).parse(state) {
                (state, TypeInfo::Boolean(true_singleton))
            } else if let Ok((state, false_singleton)) = ParseSymbol(Symbol::False).parse(state) {
                (state, TypeInfo::Boolean(false_singleton))
            // Singleton type info: `nil` in `local function get(x: string, y: nil) end`
            } else if let Ok((state, nil_singleton)) = ParseSymbol(Symbol::Nil).parse(state) {
                (state, TypeInfo::Basic(nil_singleton))
            } else if let Ok((state, identifier)) = ParseIdentifier
                    .parse(state)
            {
                if identifier.token().to_string() == "typeof" {
                    let (state, start_parenthese) = expect!(
                        state,
                        ParseSymbol(Symbol::LeftParen).parse(state),
                        "expected '(' when parsing typeof type"
                    );

                    let (state, expression) = expect!(
                        state,
                        ParseExpression.parse(state),
                        "expected expression when parsing typeof type"
                    );

                    let (state, end_parenthese) = expect!(
                        state,
                        ParseSymbol(Symbol::RightParen).parse(state),
                        "expected ')' when parsing typeof type"
                    );

                    (
                        state,
                        TypeInfo::Typeof {
                            typeof_token: identifier,
                            parentheses: ContainedSpan::new(start_parenthese, end_parenthese),
                            inner: Box::new(expression),
                        },
                    )
                } else if let Ok((state, punctuation)) = ParseSymbol(Symbol::Dot).parse(state)
                {
                    let (state, type_info) = expect!(
                        state,
                        ParseIndexedTypeInfo.parse(state),
                        "expected type when parsing type index"
                    );

                    (
                        state,
                        TypeInfo::Module {
                            module: identifier,
                            punctuation,
                            type_info: Box::new(type_info),
                        },
                    )
                } else if let Ok((state, start_arrow)) = ParseSymbol(Symbol::LessThan).parse(state)
                {
                    let (state, generics) = expect!(
                        state,
                        OneOrMore(ParseTypeInfo(TypeInfoContext::GenericArgument), ParseSymbol(Symbol::Comma), false).parse(state),
                        "expected type parameters"
                    );

                    let (state, end_arrow) = expect!(
                        state,
                        ParseSymbol(Symbol::GreaterThan).parse(state),
                        "expected `>` to close `<`"
                    );

                    (
                        state,
                        TypeInfo::Generic {
                            base: identifier,
                            arrows: ContainedSpan::new(start_arrow, end_arrow),
                            generics,
                        },
                    )
                } else if matches!(this.0, TypeInfoContext::ParenthesesType | TypeInfoContext::ReturnType | TypeInfoContext::VarArgSpecifier | TypeInfoContext::GenericArgument) {
                    // Check for a generic type pack
                    if let Ok((state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
                        (state, TypeInfo::GenericPack {
                            name: identifier,
                            ellipse
                        })
                    } else {
                        (state, TypeInfo::Basic(identifier))
                    }
                } else {
                    (state, TypeInfo::Basic(identifier))
                }
            } else if let Ok((state, generics)) = ParseGenericDeclaration.parse(state) {
                // Callback with a generic type
                let (state, start_parenthese) = ParseSymbol(Symbol::LeftParen).parse(state)?;
                ParseCallbackTypeInfo(start_parenthese, Some(generics)).parse(state)?
            } else if let Ok((state, start_parenthese)) =
                ParseSymbol(Symbol::LeftParen).parse(state)
            {
                // Parse types encapsulated in parentheses as an atom. If we are allowing type packs (i.e. as a return type or a generic type argument),
                // this could be a tuple, otherwise this can only be a singular type specified within parentheses.
                let atom =
                    if matches!(this.0, TypeInfoContext::ReturnType | TypeInfoContext::GenericArgument) {
                        ParseTupleTypeInfo(start_parenthese.clone()).parse(state)
                    } else {
                        ParseParenthesesTypeInfo(start_parenthese.clone()).parse(state)
                    };

                if let Ok((state, parentheses_type)) = atom {
                    // Single token lookahead: see if we have a `->` ahead. If we do, we need to parse this
                    // as a callback type, using what we already have from the parentheses type
                    if let Ok((state, arrow)) = ParseSymbol(Symbol::ThinArrow).parse(state) {
                        // Unwrap the parsed parentheses type into its parentheses and its enclosed type
                        let (parentheses, arguments) = match parentheses_type {
                            TypeInfo::Tuple { parentheses, types } => {
                                // `types` is currently `Punctuated<TypeInfo>`, but we need to map it to `Punctuated<TypeArgument>`
                                // where each argument has no name.
                                let arguments = types.into_pairs().map(|pair| pair.map(|type_info| TypeArgument {
                                    name: None,
                                    type_info
                                })).collect::<Punctuated<_>>();
                                (parentheses, arguments)
                            },
                            _ => unreachable!("parsed a non-tuple as a parentheses type"),
                        };

                        let (state, return_value) = expect!(
                            state,
                            ParseTypeInfo(TypeInfoContext::ReturnType).parse(state),
                            "expected return type after `->`"
                        );

                        (
                            state,
                            TypeInfo::Callback {
                                generics: None,
                                arguments,
                                parentheses,
                                arrow,
                                return_type: Box::new(return_value),
                            },
                        )
                    } else {
                        (state, parentheses_type)
                    }
                } else {
                    ParseCallbackTypeInfo(start_parenthese, None).parse(state)?
                }
            } else if let Ok((state, start_brace)) = ParseSymbol(Symbol::LeftBrace).parse(state) {
                if let Ok((state, type_array)) = ParseTypeArray(start_brace.clone()).parse(state) {
                    (state, type_array)
                } else {
                    ParseTypeTable(start_brace).parse(state)?
                }
            } else if matches!(this.0, TypeInfoContext::GenericArgument) {
                // Only allowed variadic type packs as a generic argument
                // Note, this is `...T`, but `T` is a token, not a type info (e.g. `...string` is allowed but `...{}` is not)
                if let Ok((state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
                    let (state, name) = expect!(
                        state,
                        ParseIdentifier.parse(state),
                        "expected name after `...`"
                    );

                    (state, TypeInfo::VariadicPack { ellipse, name })
                } else {
                    return Err(InternalAstError::NoMatch);
                }
            } else if matches!(this.0, TypeInfoContext::ParenthesesType | TypeInfoContext::ReturnType) {
                // Only allow variadic type annotation for a return type or a tuple type
                if let Ok((state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
                    let (state, type_info) = expect!(
                        state,
                        ParseSingleTypeInfo(TypeInfoContext::None).parse(state),
                        "expected type info after `...`"
                    );

                    (
                        state,
                        TypeInfo::Variadic {
                            ellipse,
                            type_info: Box::new(type_info),
                        }
                    )
                } else {
                    return Err(InternalAstError::NoMatch);
                }
            } else {
                return Err(InternalAstError::NoMatch);
            };

            Ok((state, base_type))
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseSingleTypeInfoAtom(TypeInfoContext);
        define_parser!(ParseSingleTypeInfoAtom, TypeInfo, |this, state| {
            let (mut state, mut base_type) = ParseSingleTypeInfo(this.0).parse(state)?;

            if let Ok((new_state, question_mark)) = ParseSymbol(Symbol::QuestionMark).parse(state) {
                base_type = TypeInfo::Optional {
                    base: Box::new(base_type),
                    question_mark,
                };

                state = new_state;
            }

            Ok((state, base_type))
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeInfo(TypeInfoContext);
        define_parser!(ParseTypeInfo, TypeInfo, |this, state| {
            let (state, base_type) = ParseSingleTypeInfoAtom(this.0).parse(state)?;

            if let Ok((state, pipe)) = ParseSymbol(Symbol::Pipe).parse(state) {
                let (state, right) = expect!(
                    state,
                    ParseTypeInfo(this.0).parse(state),
                    "expected type after `|` for union type"
                );
                Ok((
                    state,
                    TypeInfo::Union {
                        left: Box::new(base_type),
                        right: Box::new(right),
                        pipe,
                    },
                ))
            } else if let Ok((state, ampersand)) = ParseSymbol(Symbol::Ampersand).parse(state) {
                let (state, right) = expect!(
                    state,
                    ParseTypeInfo(this.0).parse(state),
                    "expected type after `&` for intersection type"
                );
                Ok((
                    state,
                    TypeInfo::Intersection {
                        left: Box::new(base_type),
                        right: Box::new(right),
                        ampersand,
                    },
                ))
            } else {
                Ok((state, base_type))
            }
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeField;
        define_parser!(
            ParseTypeField,
            TypeField,
            |_, state| {
                let (state, key) = ParseTypeFieldKey.parse(state)?;

                let (state, colon) = expect!(
                    state,
                    ParseSymbol(Symbol::Colon).parse(state),
                    "expected `:` after key"
                );

                let (state, value) = expect!(
                    state,
                    ParseTypeInfo(TypeInfoContext::None).parse(state),
                    "expected value type for key"
                );

                Ok((state, TypeField { key, colon, value }))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeFieldKey;
        #[rustfmt::skip]
        define_parser!(ParseTypeFieldKey, TypeFieldKey, |_, state| {
            if let Ok((state, identifier)) = ParseIdentifier.parse(state) {
                Ok((state, TypeFieldKey::Name(identifier)))
            } else if let Ok((state, start_bracket)) = ParseSymbol(Symbol::LeftBracket).parse(state)
            {
                let (state, inner) = expect!(
                    state,
                    ParseTypeInfo(TypeInfoContext::None).parse(state),
                    "expected type within brackets for index signature"
                );

                let (state, end_bracket) = expect!(
                    state,
                    ParseSymbol(Symbol::RightBracket).parse(state),
                    "expected `]` to match `[`"
                );

                Ok((
                    state,
                    TypeFieldKey::IndexSignature {
                        brackets: ContainedSpan::new(start_bracket, end_bracket),
                        inner,
                    },
                ))
            } else {
                Err(InternalAstError::NoMatch)
            }
        });
    }
}

// Lua 5.2 related syntax
#[derive(Clone, Debug, PartialEq)]
struct ParseGoto;
define_lua52_parser!(ParseGoto, Goto, TokenReference, |_, state| {
    let (state, goto_token) = ParseSymbol(Symbol::Goto).parse(state)?;
    let (state, label_name) = expect!(
        state,
        ParseIdentifier.parse(state),
        "expected identifier after `goto`"
    );

    Ok((
        state,
        Goto {
            goto_token,
            label_name,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseLabel;
define_lua52_parser!(ParseLabel, Label, TokenReference, |_, state| {
    let (state, left_colons) = ParseSymbol(Symbol::TwoColons).parse(state)?;
    let (state, name) = expect!(
        state,
        ParseIdentifier.parse(state),
        "expected identifier after `::`"
    );
    let (state, right_colons) = expect!(
        state,
        ParseSymbol(Symbol::TwoColons).parse(state),
        "expected `::`"
    );

    Ok((
        state,
        Label {
            left_colons,
            name,
            right_colons,
        },
    ))
});

macro_rules! make_op_parser {
	($enum:ident, $parser:ident, { $($operator:ident,)+ }) => {
		#[derive(Clone, Debug, PartialEq)]
        struct $parser;
        define_parser!($parser, $enum, |_, state| {
            $(
                if let Ok((state, operator)) = ParseSymbol(Symbol::$operator).parse(state) {
                    return Ok((state, $enum::$operator(operator)));
                }
            )+

			// This is to ensure the operators ALWAYS match those in the actual operator
			// It won't compile if they don't match up
			if let Some(x) = None {
				match x {
					$(
						$enum::$operator(_) => {},
					)+
				}
			}

            Err(InternalAstError::NoMatch)
        });
	};
}

make_op_parser!(BinOp, ParseBinOp,
    {
        And,
        Caret,
        GreaterThan,
        GreaterThanEqual,
        LessThan,
        LessThanEqual,
        Minus,
        Or,
        Percent,
        Plus,
        Slash,
        Star,
        TildeEqual,
        TwoDots,
        TwoEqual,
    }
);

make_op_parser!(UnOp, ParseUnOp,
    {
        Minus,
        Not,
        Hash,
    }
);

#[cfg(feature = "roblox")]
make_op_parser!(CompoundOp, ParseCompoundOp,
    {
        PlusEqual,
        MinusEqual,
        StarEqual,
        SlashEqual,
        PercentEqual,
        CaretEqual,
        TwoDotsEqual,
    }
);

// TODO

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::{ast::extract_token_references, tokenizer::tokens};
//     use pretty_assertions::assert_eq;

//     macro_rules! assert_state_eq {
//         ($state: expr, $index: expr, $tokens: ident) => {
//             assert_eq!($state.index, $index);
//             assert_eq!($state.len, $tokens.len());
//         };
//     }

//     macro_rules! tokens {
//         ($body: expr) => {
//             extract_token_references(tokens($body).expect("couldn't tokenize'"))
//         };
//     }

//     #[test]
//     fn test_zero_or_more_empty() {
//         let tokens = tokens!("local x");
//         let state = ParserState::new(&tokens);

//         let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma))
//             .parse(state)
//             .unwrap();

//         assert_state_eq!(state, 0, tokens);
//         assert_eq!(commas.len(), 0);
//     }

//     #[test]
//     fn test_zero_or_more_exists() {
//         let tokens = tokens!(",,, , ,\t ,local x");
//         let state = ParserState::new(&tokens);

//         let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma))
//             .parse(state)
//             .unwrap();

//         assert_state_eq!(state, 9, tokens);
//         assert_eq!(commas.len(), 6);
//     }

//     #[test]
//     fn test_one_or_more_empty() {
//         let tokens = tokens!("local x");
//         let state = ParserState::new(&tokens);

//         assert!(
//             OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
//                 .parse(state)
//                 .is_err()
//         );
//     }

//     #[test]
//     fn test_one_or_more_exists_no_delimiter() {
//         let tokens = tokens!("end,end, end,\t\tend local");
//         let state = ParserState::new(&tokens);

//         let (state, commas) =
//             OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
//                 .parse(state)
//                 .expect("OneOrMore failed");

//         assert_state_eq!(state, 10, tokens);
//         assert_eq!(commas.len(), 4);
//     }

//     #[test]
//     fn test_one_or_more_exists_with_delimiter() {
//         let tokens = tokens!("end,end, end,\t\tend, local");
//         let state = ParserState::new(&tokens);

//         let (state, commas) = OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
//             .parse(state)
//             .unwrap();

//         assert_state_eq!(state, 11, tokens);
//         assert_eq!(commas.len(), 4);
//     }

//     #[test]
//     fn test_one_or_more_exists_with_nothing() {
//         let tokens = tokens!("local");
//         let state = ParserState::new(&tokens);

//         assert!(
//             OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
//                 .parse(state)
//                 .is_err()
//         );
//     }
// }
