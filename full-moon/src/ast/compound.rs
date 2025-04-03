use crate::ast::{Expression, Var};
use crate::tokenizer::{Symbol, TokenReference};
use derive_more::Display;
use full_moon_derive::{Node, Visit};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Display, PartialEq, Eq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
#[allow(missing_docs)]
#[display("{_0}")]
/// Compound operators, such as X += Y or X -= Y
pub enum CompoundOp {
    PlusEqual(TokenReference),
    MinusEqual(TokenReference),
    StarEqual(TokenReference),
    SlashEqual(TokenReference),
    CaretEqual(TokenReference),

    // luau specific
    #[cfg(feature = "luau")]
    DoubleSlashEqual(TokenReference),
    #[cfg(feature = "luau")]
    PercentEqual(TokenReference),
    #[cfg(feature = "luau")]
    TwoDotsEqual(TokenReference),

    // cfxlua specific
    #[cfg(feature = "cfxlua")]
    DoubleLessThanEqual(TokenReference),
    #[cfg(feature = "cfxlua")]
    DoubleGreaterThanEqual(TokenReference),
    #[cfg(feature = "cfxlua")]
    AmpersandEqual(TokenReference),
    #[cfg(feature = "cfxlua")]
    PipeEqual(TokenReference),
}

impl CompoundOp {
    /// The token associated with the operator
    pub fn token(&self) -> &TokenReference {
        match self {
            Self::PlusEqual(token)
            | Self::MinusEqual(token)
            | Self::StarEqual(token)
            | Self::SlashEqual(token)
            | Self::CaretEqual(token) => token,
            
            #[cfg(feature = "luau")]
            Self::DoubleSlashEqual(token) => token,
            #[cfg(feature = "luau")]
            Self::PercentEqual(token) => token,
            #[cfg(feature = "luau")]
            Self::TwoDotsEqual(token) => token,
            
            #[cfg(feature = "cfxlua")]
            Self::DoubleLessThanEqual(token) => token,
            #[cfg(feature = "cfxlua")]
            Self::DoubleGreaterThanEqual(token) => token,
            #[cfg(feature = "cfxlua")]
            Self::AmpersandEqual(token) => token,
            #[cfg(feature = "cfxlua")]
            Self::PipeEqual(token) => token,
        }
    }

    pub(crate) fn from_token(token: TokenReference) -> Self {
        if token.is_symbol(Symbol::PlusEqual) {
            return Self::PlusEqual(token);
        } else if token.is_symbol(Symbol::MinusEqual) {
            return Self::MinusEqual(token);
        } else if token.is_symbol(Symbol::StarEqual) {
            return Self::StarEqual(token);
        } else if token.is_symbol(Symbol::SlashEqual) {
            return Self::SlashEqual(token);
        } else if token.is_symbol(Symbol::CaretEqual) {
            return Self::CaretEqual(token);
        }

        #[cfg(feature = "luau")]
        if token.is_symbol(Symbol::DoubleSlashEqual) {
            return Self::DoubleSlashEqual(token);
        } else if token.is_symbol(Symbol::PercentEqual) {
            return Self::PercentEqual(token);
        } else if token.is_symbol(Symbol::TwoDotsEqual) {
            return Self::TwoDotsEqual(token);
        }

        #[cfg(feature = "cfxlua")]
        if token.is_symbol(Symbol::DoubleLessThanEqual) {
            return Self::DoubleLessThanEqual(token);
        } else if token.is_symbol(Symbol::DoubleGreaterThanEqual) {
            return Self::DoubleGreaterThanEqual(token);
        } else if token.is_symbol(Symbol::AmpersandEqual) {
            return Self::AmpersandEqual(token);
        } else if token.is_symbol(Symbol::PipeEqual) {
            return Self::PipeEqual(token);
        }

        unreachable!("converting an unknown token into a compound operator")
    }
}

/// A Compound Assignment statement, such as `x += 1` or `x -= 1`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{lhs}{compound_operator}{rhs}")]
pub struct CompoundAssignment {
    pub(crate) lhs: Var,
    pub(crate) compound_operator: CompoundOp,
    pub(crate) rhs: Expression,
}

impl CompoundAssignment {
    /// Creates a new CompoundAssignment from the left and right hand side
    pub fn new(lhs: Var, compound_operator: CompoundOp, rhs: Expression) -> Self {
        Self {
            lhs,
            compound_operator,
            rhs,
        }
    }

    /// The variable assigned to, the `x` part of `x += 1`
    pub fn lhs(&self) -> &Var {
        &self.lhs
    }

    /// The operator used, the `+=` part of `x += 1`
    pub fn compound_operator(&self) -> &CompoundOp {
        &self.compound_operator
    }

    /// The value being assigned, the `1` part of `x += 1`
    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }

    /// Returns a new CompoundAssignment with the given variable being assigned to
    pub fn with_lhs(self, lhs: Var) -> Self {
        Self { lhs, ..self }
    }

    /// Returns a new CompoundAssignment with the given operator used
    pub fn with_compound_operator(self, compound_operator: CompoundOp) -> Self {
        Self {
            compound_operator,
            ..self
        }
    }

    /// Returns a new CompoundAssignment with the given value being assigned
    pub fn with_rhs(self, rhs: Expression) -> Self {
        Self { rhs, ..self }
    }
}
