//! Contains the types for if expressions
use super::*;
use crate::{
    tokenizer::{TokenReference},
};
use derive_more::Display;

/// An if statement
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    "{}{}{}{}{}{}{}{}",
    if_token,
    condition,
    then_token,
    if_expression,
    display_option(else_if_expressions.as_ref().map(join_vec)),
    else_token,
    else_expression,
    display_option(end_token),
)]
pub struct IfExpression {
    pub(crate) if_token: TokenReference,
    pub(crate) condition: Box<Expression>,
    pub(crate) then_token: TokenReference,
    pub(crate) if_expression: Box<Expression>,
    pub(crate) else_if_expressions: Option<Vec<ElseIfExpression>>,
    pub(crate) else_token: TokenReference,
    pub(crate) else_expression: Box<Expression>,
    pub(crate) end_token: Option<TokenReference>,
}

impl IfExpression {
    /// Creates a new If from the given condition
    pub fn new(
        condition: Expression,
        if_expression: Expression,
        else_expression: Expression,
    ) -> Self {
        Self {
            if_token: TokenReference::symbol("if ").unwrap(),
            condition: Box::new(condition),
            then_token: TokenReference::symbol(" then").unwrap(),
            if_expression: Box::new(if_expression),
            else_if_expressions: None,
            else_token: TokenReference::symbol(" else ").unwrap(),
            else_expression: Box::new(else_expression),
            end_token: None,
        }
    }

    /// The `if` token
    pub fn if_token(&self) -> &TokenReference {
        &self.if_token
    }

    /// The condition of the if expression, `condition` in `if condition then`
    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    /// The `then` token
    pub fn then_token(&self) -> &TokenReference {
        &self.then_token
    }

    /// The expression evaluated if the initial if condition holds
    pub fn if_expression(&self) -> &Expression {
        &self.if_expression
    }

    /// The `else` token
    pub fn else_token(&self) -> &TokenReference {
        &self.else_token
    }

    /// If there are `elseif` conditions, returns a vector of them
    // TODO: Make this return an iterator, and remove Option part entirely?
    pub fn else_if_expressions(&self) -> Option<&Vec<ElseIfExpression>> {
        self.else_if_expressions.as_ref()
    }

    /// The else expression if all other conditions do not hold
    pub fn else_expression(&self) -> &Expression {
        &self.else_expression
    }

    /// Returns a new IfExpression with the given `if` token
    pub fn with_if_token(self, if_token: TokenReference) -> Self {
        Self { if_token, ..self }
    }

    /// Returns a new IfExpression with the given condition
    pub fn with_condition(self, condition: Expression) -> Self {
        Self {
            condition: Box::new(condition),
            ..self
        }
    }

    /// Returns a new IfExpression with the given `then` token
    pub fn with_then_token(self, then_token: TokenReference) -> Self {
        Self { then_token, ..self }
    }

    /// Returns a new IfExpression with the given if expression
    pub fn with_if_expression(self, if_expression: Expression) -> Self {
        Self {
            if_expression: Box::new(if_expression),
            ..self
        }
    }

    /// Returns a new If with the given list of `elseif` expressions
    pub fn with_else_if(self, else_if_expressions: Option<Vec<ElseIfExpression>>) -> Self {
        Self {
            else_if_expressions,
            ..self
        }
    }

    /// Returns a new IfExpression with the given `else` token
    pub fn with_else_token(self, else_token: TokenReference) -> Self {
        Self { else_token, ..self }
    }

    /// Returns a new IfExpression with the given `else` expression
    pub fn with_else(self, else_expression: Expression) -> Self {
        Self {
            else_expression: Box::new(else_expression),
            ..self
        }
    }

    /// Returns a new IfExpression with the given `end` toke
    pub fn with_end_token(self, end_token: TokenReference) -> Self {
        Self { end_token: Some(end_token), ..self }
    }
}

/// An elseif expression in a bigger [`IfExpression`] expression
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{else_if_token}{condition}{then_token}{expression}")]
pub struct ElseIfExpression {
    pub(crate) else_if_token: TokenReference,
    pub(crate) condition: Expression,
    pub(crate) then_token: TokenReference,
    pub(crate) expression: Expression,
}

impl ElseIfExpression {
    /// Creates a new ElseIf from the given condition
    pub fn new(condition: Expression, expression: Expression) -> Self {
        Self {
            else_if_token: TokenReference::symbol(" elseif ").unwrap(),
            condition,
            then_token: TokenReference::symbol(" then ").unwrap(),
            expression,
        }
    }

    /// The `elseif` token
    pub fn else_if_token(&self) -> &TokenReference {
        &self.else_if_token
    }

    /// The condition of the `elseif`, `condition` in `elseif condition then`
    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    /// The `then` token
    pub fn then_token(&self) -> &TokenReference {
        &self.then_token
    }

    /// The evaluated expression of the `elseif` when condition is true
    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    /// Returns a new ElseIfExpression with the given `elseif` token
    pub fn with_else_if_token(self, else_if_token: TokenReference) -> Self {
        Self {
            else_if_token,
            ..self
        }
    }

    /// Returns a new ElseIfExpression with the given condition
    pub fn with_condition(self, condition: Expression) -> Self {
        Self { condition, ..self }
    }

    /// Returns a new ElseIfExpression with the given `then` token
    pub fn with_then_token(self, then_token: TokenReference) -> Self {
        Self { then_token, ..self }
    }

    /// Returns a new ElseIfExpression with the given expression
    pub fn with_block(self, expression: Expression) -> Self {
        Self { expression, ..self }
    }
}
