// Implementations of Visit and VisitMut that are not able to be automatically derived yet.
// Ideally everything would be derived.
use super::*;
use crate::visitors::{Visit, VisitMut, Visitor, VisitorMut};

// The following have `ContainedSpan`, which when automatically derived will visit the tokens containing
// before they visit what they're actually containing.
// For example, if there is an AST node that represents `(foo)`...
// Then visitors will visit this as `()foo`.
// This is fixed for structs with `#[visit(contains = "...")], but this is not supported on enums.

impl Visit for Field {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
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

impl VisitMut for Field {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
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

impl Visit for Expression {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_expression(self);
        match self {
            Expression::BinaryOperator { lhs, binop, rhs } => {
                lhs.visit(visitor);
                binop.visit(visitor);
                rhs.visit(visitor);
            }

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
                #[cfg(feature = "roblox")]
                type_assertion,
            } => {
                value.visit(visitor);
                #[cfg(feature = "roblox")]
                type_assertion.visit(visitor);
            }
        };

        visitor.visit_expression_end(self);
    }
}

impl VisitMut for Expression {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_expression(self);
        self = match self {
            Expression::BinaryOperator { lhs, binop, rhs } => Expression::BinaryOperator {
                lhs: lhs.visit_mut(visitor),
                binop: binop.visit_mut(visitor),
                rhs: rhs.visit_mut(visitor),
            },

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
                #[cfg(feature = "roblox")]
                type_assertion,
            } => Expression::Value {
                value: value.visit_mut(visitor),
                #[cfg(feature = "roblox")]
                type_assertion: type_assertion.visit_mut(visitor),
            },
        };

        self = visitor.visit_expression_end(self);
        self
    }
}

impl Visit for Index {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
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

impl VisitMut for Index {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
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

impl Visit for FunctionArgs {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
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

impl VisitMut for FunctionArgs {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
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

// The following contain type signatures, which are addendums to previous identities
impl Visit for FunctionBody {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_function_body(self);

        #[cfg(feature = "roblox")]
        self.generics.visit(visitor);

        self.parameters_parentheses.tokens.0.visit(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement Visit for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        for parameter in &self.parameters {
            parameter.visit(visitor);
            type_specifiers.next().visit(visitor);
        }

        self.parameters_parentheses.tokens.1.visit(visitor);

        #[cfg(feature = "roblox")]
        self.return_type.visit(visitor);

        self.block.visit(visitor);
        self.end_token.visit(visitor);
        visitor.visit_function_body_end(self);
    }
}

impl VisitMut for FunctionBody {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_function_body(self);

        #[cfg(feature = "roblox")]
        {
            self.generics = self.generics.visit_mut(visitor);
        }

        self.parameters_parentheses.tokens.0 =
            self.parameters_parentheses.tokens.0.visit_mut(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers.into_iter();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement VisitMut for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        let mut new_type_specifiers = Vec::new();
        let mut new_parameters = Punctuated::new();

        for parameter_pair in self.parameters.into_pairs() {
            let parameter_tuple = parameter_pair.into_tuple();
            let parameter = parameter_tuple.0.visit_mut(visitor);

            let type_specifier = type_specifiers
                .next()
                .and_then(|type_specifier| type_specifier)
                .map(|type_specifier| type_specifier.visit_mut(visitor));
            new_type_specifiers.push(type_specifier);
            let punctuation = parameter_tuple.1.visit_mut(visitor);

            new_parameters.push(Pair::new(parameter, punctuation));
        }

        self.parameters = new_parameters;

        #[cfg(feature = "roblox")]
        {
            self.type_specifiers = new_type_specifiers;
        }

        self.parameters_parentheses.tokens.1 =
            self.parameters_parentheses.tokens.1.visit_mut(visitor);

        #[cfg(feature = "roblox")]
        {
            self.return_type = self.return_type.visit_mut(visitor);
        }

        self.block = self.block.visit_mut(visitor);
        self.end_token = self.end_token.visit_mut(visitor);
        self = visitor.visit_function_body_end(self);
        self
    }
}

impl Visit for LocalAssignment {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_local_assignment(self);
        self.local_token.visit(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement Visit for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        for name in &self.name_list {
            name.visit(visitor);
            type_specifiers.next().visit(visitor);
        }

        self.equal_token.visit(visitor);
        self.expr_list.visit(visitor);
        visitor.visit_local_assignment_end(self);
    }
}

impl VisitMut for LocalAssignment {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_local_assignment(self);
        self.local_token = self.local_token.visit_mut(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers.into_iter();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement VisitMut for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        let mut new_type_specifiers = Vec::new();
        let mut new_names = Punctuated::new();

        for parameter_pair in self.name_list.into_pairs() {
            let parameter_tuple = parameter_pair.into_tuple();
            let parameter = parameter_tuple.0.visit_mut(visitor);
            let type_specifier = type_specifiers
                .next()
                .and_then(|type_specifier| type_specifier)
                .map(|type_specifier| type_specifier.visit_mut(visitor));

            let punctuation = parameter_tuple.1.visit_mut(visitor);
            new_type_specifiers.push(type_specifier);
            new_names.push(Pair::new(parameter, punctuation));
        }

        self.name_list = new_names;

        #[cfg(feature = "roblox")]
        {
            self.type_specifiers = new_type_specifiers;
        }

        self.equal_token = self.equal_token.visit_mut(visitor);
        self.expr_list = self.expr_list.visit_mut(visitor);
        self = visitor.visit_local_assignment_end(self);
        self
    }
}

impl Visit for GenericFor {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_generic_for(self);
        self.for_token.visit(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement Visit for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        for name in &self.names {
            name.visit(visitor);
            type_specifiers.next().visit(visitor);
        }

        self.in_token.visit(visitor);
        self.expr_list.visit(visitor);
        self.do_token.visit(visitor);
        self.block.visit(visitor);
        self.end_token.visit(visitor);

        visitor.visit_generic_for_end(self);
    }
}

impl VisitMut for GenericFor {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_generic_for(self);
        self.for_token = self.for_token.visit_mut(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers.into_iter();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement VisitMut for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        let mut new_type_specifiers = Vec::new();
        let mut new_names = Punctuated::new();

        for parameter_pair in self.names.into_pairs() {
            let parameter_tuple = parameter_pair.into_tuple();
            let parameter = parameter_tuple.0.visit_mut(visitor);
            let type_specifier = type_specifiers
                .next()
                .and_then(|type_specifier| type_specifier)
                .map(|type_specifier| type_specifier.visit_mut(visitor));

            let punctuation = parameter_tuple.1.visit_mut(visitor);
            new_type_specifiers.push(type_specifier);
            new_names.push(Pair::new(parameter, punctuation));
        }

        self.names = new_names;

        #[cfg(feature = "roblox")]
        {
            self.type_specifiers = new_type_specifiers;
        }

        self.in_token = self.in_token.visit_mut(visitor);
        self.expr_list = self.expr_list.visit_mut(visitor);
        self.do_token = self.do_token.visit_mut(visitor);
        self.block = self.block.visit_mut(visitor);
        self.end_token = self.end_token.visit_mut(visitor);

        self = visitor.visit_generic_for_end(self);
        self
    }
}

impl Visit for NumericFor {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_numeric_for(self);
        self.for_token.visit(visitor);
        self.index_variable.visit(visitor);

        #[cfg(feature = "roblox")]
        self.type_specifier.visit(visitor);

        self.equal_token.visit(visitor);
        self.start.visit(visitor);
        self.start_end_comma.visit(visitor);
        self.end.visit(visitor);
        self.end_step_comma.visit(visitor);
        self.step.visit(visitor);
        self.do_token.visit(visitor);
        self.block.visit(visitor);
        self.end_token.visit(visitor);

        visitor.visit_numeric_for_end(self);
    }
}

impl VisitMut for NumericFor {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_numeric_for(self);
        self.for_token = self.for_token.visit_mut(visitor);
        self.index_variable = self.index_variable.visit_mut(visitor);

        #[cfg(feature = "roblox")]
        {
            self.type_specifier = self.type_specifier.visit_mut(visitor);
        }

        self.equal_token = self.equal_token.visit_mut(visitor);
        self.start = self.start.visit_mut(visitor);
        self.start_end_comma = self.start_end_comma.visit_mut(visitor);
        self.end = self.end.visit_mut(visitor);
        self.end_step_comma = self.end_step_comma.visit_mut(visitor);
        self.step = self.step.visit_mut(visitor);
        self.do_token = self.do_token.visit_mut(visitor);
        self.block = self.block.visit_mut(visitor);
        self.end_token = self.end_token.visit_mut(visitor);

        self = visitor.visit_numeric_for_end(self);
        self
    }
}
