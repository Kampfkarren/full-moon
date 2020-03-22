// Implementations of Visit and VisitMut that are not able to be automatically derived yet.
// Ideally everything would be derived.
use super::*;
use crate::visitors::{Visit, VisitMut, Visitor, VisitorMut};

// The following have `ContainedSpan`, which when automatically derived will visit the tokens containing
// before they visit what they're actually containing.
// For example, if there is an AST node that represents `(foo)`...
// Then visitors will visit this as `()foo`.
// This is fixed for structs with `#[visit(contains = "...")], but this is not supported on enums.

impl<'a> Visit<'a> for Field<'a> {
    fn visit<V: Visitor<'a>>(&self, visitor: &mut V) {
        visitor.visit_field(self);
        match self {
            Field::ExpressionKey {
                brackets,
                key,
                equal,
                value,
            } => {
                brackets.tokens.0.visit(visitor);
                key.visit(visitor);
                brackets.tokens.1.visit(visitor);
                equal.visit(visitor);
                value.visit(visitor);
            }

            Field::NameKey { key, equal, value } => {
                key.visit(visitor);
                equal.visit(visitor);
                value.visit(visitor);
            }

            Field::NoKey(__self_0) => {
                __self_0.visit(visitor);
            }
        };

        visitor.visit_field_end(self);
    }
}

impl<'a> VisitMut<'a> for Field<'a> {
    fn visit_mut<V: VisitorMut<'a>>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_field(self);
        self = match self {
            Field::ExpressionKey {
                mut brackets,

                mut key,
                equal,
                value,
            } => {
                brackets.tokens.0 = brackets.tokens.0.visit_mut(visitor);
                key = key.visit_mut(visitor);
                brackets.tokens.1 = brackets.tokens.1.visit_mut(visitor);

                Field::ExpressionKey {
                    brackets,
                    key,
                    equal: equal.visit_mut(visitor),
                    value: value.visit_mut(visitor),
                }
            }

            Field::NameKey { key, equal, value } => Field::NameKey {
                key: key.visit_mut(visitor),
                equal: equal.visit_mut(visitor),
                value: value.visit_mut(visitor),
            },

            Field::NoKey(__self_0) => Field::NoKey(__self_0.visit_mut(visitor)),
        };

        self = visitor.visit_field_end(self);
        self
    }
}

impl<'a> Visit<'a> for Expression<'a> {
    fn visit<V: Visitor<'a>>(&self, visitor: &mut V) {
        visitor.visit_expression(self);
        match self {
            Expression::Parentheses {
                contained,
                expression,
            } => {
                contained.tokens.0.visit(visitor);
                expression.visit(visitor);
                contained.tokens.1.visit(visitor);
            }
            Expression::UnaryOperator { unop, expression } => {
                unop.visit(visitor);
                expression.visit(visitor);
            }
            Expression::Value {
                value,
                binop,
                #[cfg(feature = "roblox")]
                as_assertion,
            } => {
                value.visit(visitor);
                binop.visit(visitor);
                #[cfg(feature = "roblox")]
                as_assertion.visit(visitor);
            }
        };

        visitor.visit_expression_end(self);
    }
}

impl<'a> VisitMut<'a> for Expression<'a> {
    fn visit_mut<V: VisitorMut<'a>>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_expression(self);
        self = match self {
            Expression::Parentheses {
                mut contained,
                mut expression,
            } => {
                contained.tokens.0 = contained.tokens.0.visit_mut(visitor);
                expression = expression.visit_mut(visitor);
                contained.tokens.1 = contained.tokens.1.visit_mut(visitor);

                Expression::Parentheses {
                    contained,
                    expression,
                }
            }

            Expression::UnaryOperator { unop, expression } => Expression::UnaryOperator {
                unop: unop.visit_mut(visitor),
                expression: expression.visit_mut(visitor),
            },

            Expression::Value {
                value,
                binop,
                #[cfg(feature = "roblox")]
                as_assertion,
            } => Expression::Value {
                value: value.visit_mut(visitor),
                binop: binop.visit_mut(visitor),
                #[cfg(feature = "roblox")]
                as_assertion: as_assertion.visit_mut(visitor),
            },
        };

        self = visitor.visit_expression_end(self);
        self
    }
}

impl<'a> Visit<'a> for Index<'a> {
    fn visit<V: Visitor<'a>>(&self, visitor: &mut V) {
        visitor.visit_index(self);
        match self {
            Index::Brackets {
                brackets,
                expression,
            } => {
                brackets.tokens.0.visit(visitor);
                expression.visit(visitor);
                brackets.tokens.1.visit(visitor);
            }
            Index::Dot { dot, name } => {
                dot.visit(visitor);
                name.visit(visitor);
            }
        };

        visitor.visit_index_end(self);
    }
}

impl<'a> VisitMut<'a> for Index<'a> {
    fn visit_mut<V: VisitorMut<'a>>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_index(self);
        self = match self {
            Index::Brackets {
                mut brackets,
                mut expression,
            } => {
                brackets.tokens.0 = brackets.tokens.0.visit_mut(visitor);
                expression = expression.visit_mut(visitor);
                brackets.tokens.1 = brackets.tokens.1.visit_mut(visitor);

                Index::Brackets {
                    brackets,
                    expression,
                }
            }

            Index::Dot { dot, name } => Index::Dot {
                dot: dot.visit_mut(visitor),
                name: name.visit_mut(visitor),
            },
        };

        self = visitor.visit_index_end(self);
        self
    }
}

impl<'a> Visit<'a> for FunctionArgs<'a> {
    fn visit<V: Visitor<'a>>(&self, visitor: &mut V) {
        visitor.visit_function_args(self);
        match self {
            FunctionArgs::Parentheses {
                parentheses,
                arguments,
            } => {
                parentheses.tokens.0.visit(visitor);
                arguments.visit(visitor);
                parentheses.tokens.1.visit(visitor);
            }
            FunctionArgs::String(__self_0) => {
                __self_0.visit(visitor);
            }
            FunctionArgs::TableConstructor(__self_0) => {
                __self_0.visit(visitor);
            }
        };

        visitor.visit_function_args_end(self);
    }
}

impl<'a> VisitMut<'a> for FunctionArgs<'a> {
    fn visit_mut<V: VisitorMut<'a>>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_function_args(self);
        self = match self {
            FunctionArgs::Parentheses {
                mut parentheses,
                mut arguments,
            } => {
                parentheses.tokens.0 = parentheses.tokens.0.visit_mut(visitor);
                arguments = arguments.visit_mut(visitor);
                parentheses.tokens.1 = parentheses.tokens.1.visit_mut(visitor);
                FunctionArgs::Parentheses {
                    parentheses,
                    arguments,
                }
            }
            FunctionArgs::String(__self_0) => FunctionArgs::String(__self_0.visit_mut(visitor)),
            FunctionArgs::TableConstructor(__self_0) => {
                FunctionArgs::TableConstructor(__self_0.visit_mut(visitor))
            }
        };

        self = visitor.visit_function_args_end(self);
        self
    }
}
