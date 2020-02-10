use crate::{
    ast::{span::ContainedSpan, *},
    private::Sealed,
    tokenizer::TokenReference,
};
use std::{borrow::Cow, sync::Arc};

#[cfg(feature = "roblox")]
use crate::ast::types::*;

macro_rules! create_visitor {
    (ast: {
        $($visit_name:ident => $ast_type:ident,)+

        $(#[$meta:meta] {
            $($meta_visit_name:ident => $meta_ast_type:ident,)+
        })+
    }, token: {
        $($visit_token:ident,)+
    }) => {
        /// A trait that implements functions to listen for specific nodes/tokens.
        /// Unlike [`VisitorMut`](trait.VisitorMut.html), nodes/tokens passed are immutable.
        ///
        /// ```rust
        /// # use full_moon::ast;
        /// # use full_moon::visitors::*;
        /// # fn main() -> Result<(), Box<std::error::Error>> {
        /// // A visitor that logs every local assignment made
        /// #[derive(Default)]
        /// struct LocalVariableVisitor {
        ///     names: Vec<String>,
        /// }
        ///
        /// impl<'ast> Visitor<'ast> for LocalVariableVisitor {
        ///     fn visit_local_assignment(&mut self, local_assignment: &ast::LocalAssignment<'ast>) {
        ///         self.names.extend(&mut local_assignment.name_list().iter().map(|name| name.to_string()));
        ///     }
        /// }
        ///
        /// let mut visitor = LocalVariableVisitor::default();
        /// visitor.visit_ast(&full_moon::parse("local x = 1; local y, z = 2, 3")?);
        /// assert_eq!(visitor.names, vec!["x", "y", "z"]);
        /// # Ok(())
        /// # }
        /// ```
        pub trait Visitor<'ast> {
            /// Visit the nodes of an [`Ast`](../ast/struct.Ast.html)
            fn visit_ast(&mut self, ast: &Ast<'ast>) where Self: Sized {
                // TODO: Visit tokens?
                ast.nodes().visit(self);
            }

            paste::item! {
                $(
                    #[allow(missing_docs)]
                    fn $visit_name(&mut self, _node: &$ast_type<'ast>) { }
                    #[allow(missing_docs)]
                    fn [<$visit_name _end>](&mut self, _node: &$ast_type<'ast>) { }
                )+

                $(
                    $(
                        #[$meta]
                        #[allow(missing_docs)]
                        fn $meta_visit_name(&mut self, _node: &$meta_ast_type<'ast>) { }
                        #[$meta]
                        #[allow(missing_docs)]
                        fn [<$meta_visit_name _end>](&mut self, _node: &$meta_ast_type<'ast>) { }
                    )+
                )+
            }

            $(
                #[allow(missing_docs)]
                fn $visit_token(&mut self, _token: &TokenReference<'ast>) { }
            )+
        }

        /// A trait that implements functions to listen for specific nodes/tokens.
        /// Unlike [`Visitor`](trait.Visitor.html), nodes/tokens passed are mutable.
        pub trait VisitorMut<'ast> {
            /// Visit the nodes of an [`Ast`](../ast/struct.Ast.html)
            fn visit_ast(&mut self, ast: Ast<'ast>) -> Ast<'ast> where Self: Sized {
                // TODO: Visit tokens?
                let eof = ast.eof().to_owned();
                let nodes = ast.nodes.visit_mut(self);

                Ast {
                    nodes,
                    // Everything gets cloned with this visitor, so there's no original tokens
                    tokens: vec![self.visit_eof(eof)],
                }
            }

            paste::item! {
                $(
                    #[allow(missing_docs)]
                    fn $visit_name(&mut self, node: $ast_type<'ast>) -> $ast_type<'ast> {
                        node
                    }

                    #[allow(missing_docs)]
                    fn [<$visit_name _end>](&mut self, node: $ast_type<'ast>) -> $ast_type<'ast> {
                        node
                    }
                )+

                $(
                    #[$meta]
                    $(
                        #[$meta]
                        #[allow(missing_docs)]
                        fn $meta_visit_name(&mut self, node: $meta_ast_type<'ast>) -> $meta_ast_type<'ast> {
                            node
                        }

                        #[$meta]
                        #[allow(missing_docs)]
                        fn [<$meta_visit_name _end>](&mut self, node: $meta_ast_type<'ast>) -> $meta_ast_type<'ast> {
                            node
                        }
                    )+
                )+
            }

            $(
                #[allow(missing_docs)]
                fn $visit_token(&mut self, token: TokenReference<'ast>) -> TokenReference<'ast> {
                    token
                }
            )+
        }
    };
}

#[doc(hidden)]
pub trait Visit<'ast>: Sealed {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V);
}

#[doc(hidden)]
pub trait VisitMut<'ast>: Sealed
where
    Self: Sized,
{
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self;
}

impl<'ast, T: Visit<'ast>> Visit<'ast> for Vec<T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        for item in self {
            item.visit(visitor);
        }
    }
}

impl<'ast, T: VisitMut<'ast>> VisitMut<'ast> for Vec<T> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        self.into_iter()
            .map(|item| item.visit_mut(visitor))
            .collect()
    }
}

impl<'ast, T: Visit<'ast>> Visit<'ast> for Option<T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        if let Some(item) = self {
            item.visit(visitor);
        }
    }
}

impl<'ast, T: VisitMut<'ast>> VisitMut<'ast> for Option<T> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        self.map(|item| item.visit_mut(visitor))
    }
}

impl<'ast, A: Visit<'ast>, B: Visit<'ast>> Visit<'ast> for (A, B) {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        self.0.visit(visitor);
        self.1.visit(visitor);
    }
}

impl<'ast, A: VisitMut<'ast>, B: VisitMut<'ast>> VisitMut<'ast> for (A, B) {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        (self.0.visit_mut(visitor), self.1.visit_mut(visitor))
    }
}

impl<'ast, T: Clone + Visit<'ast>> Visit<'ast> for Cow<'ast, T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

// TODO: This clones every Cow every visit, and there are a lot
// Can this be remedied? Is this an issue?
impl<'ast, T: Clone + VisitMut<'ast>> VisitMut<'ast> for Cow<'ast, T> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        Cow::Owned(self.into_owned().visit_mut(visitor).to_owned())
    }
}

impl<'ast, T: Visit<'ast>> Visit<'ast> for Box<T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<'ast, T: VisitMut<'ast>> VisitMut<'ast> for Box<T> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        Box::new((*self).visit_mut(visitor))
    }
}

create_visitor!(ast: {
    visit_anonymous_call => FunctionArgs,
    visit_assignment => Assignment,
    visit_bin_op => BinOpRhs,
    visit_block => Block,
    visit_call => Call,
    visit_contained_span => ContainedSpan,
    visit_do => Do,
    visit_else_if => ElseIf,
    visit_expression => Expression,
    visit_field => Field,
    visit_function_args => FunctionArgs,
    visit_function_body => FunctionBody,
    visit_function_call => FunctionCall,
    visit_function_declaration => FunctionDeclaration,
    visit_function_name => FunctionName,
    visit_generic_for => GenericFor,
    visit_if => If,
    visit_index => Index,
    visit_local_assignment => LocalAssignment,
    visit_local_function => LocalFunction,
    visit_last_stmt => LastStmt,
    visit_method_call => MethodCall,
    visit_numeric_for => NumericFor,
    visit_parameter => Parameter,
    visit_prefix => Prefix,
    visit_return => Return,
    visit_repeat => Repeat,
    visit_stmt => Stmt,
    visit_suffix => Suffix,
    visit_table_constructor => TableConstructor,
    visit_un_op => UnOp,
    visit_value => Value,
    visit_var => Var,
    visit_var_expression => VarExpression,
    visit_while => While,

    // Types
    #[cfg(feature = "roblox")] {
        visit_as_assertion => AsAssertion,
        visit_generic_declaration => GenericDeclaration,
        visit_type_declaration => TypeDeclaration,
        visit_type_field => TypeField,
        visit_type_field_key => TypeFieldKey,
        visit_type_info => TypeInfo,
        visit_type_specifier => TypeSpecifier,
    }
}, token: {
    visit_eof,
    visit_identifier,
    visit_multi_line_comment,
    visit_number,
    visit_single_line_comment,
    visit_string_literal,
    visit_symbol,
    visit_token,
    visit_whitespace,
});
