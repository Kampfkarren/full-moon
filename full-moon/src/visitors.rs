use crate::{
    ast::{span::ContainedSpan, *},
    private::Sealed,
    tokenizer::{Token, TokenReference},
};

#[cfg(feature = "lua52")]
use crate::ast::lua52::*;
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
        /// Unlike [`VisitorMut`], nodes/tokens passed are immutable.
        ///
        /// ```rust
        /// # use full_moon::ast;
        /// # use full_moon::visitors::*;
        /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
        /// // A visitor that logs every local assignment made
        /// #[derive(Default)]
        /// struct LocalVariableVisitor {
        ///     names: Vec<String>,
        /// }
        ///
        /// impl Visitor for LocalVariableVisitor {
        ///     fn visit_local_assignment(&mut self, local_assignment: &ast::LocalAssignment) {
        ///         self.names.extend(&mut local_assignment.names().iter().map(|name| name.token().to_string()));
        ///     }
        /// }
        ///
        /// let mut visitor = LocalVariableVisitor::default();
        /// visitor.visit_ast(&full_moon::parse("local x = 1; local y, z = 2, 3")?);
        /// assert_eq!(visitor.names, vec!["x", "y", "z"]);
        /// # Ok(())
        /// # }
        /// ```
        pub trait Visitor {
            /// Visit the nodes of an [`Ast`](crate::ast::Ast)
            fn visit_ast(&mut self, ast: &Ast) where Self: Sized {
                ast.nodes().visit(self);
                ast.eof().visit(self);
            }

            paste::item! {
                $(
                    #[allow(missing_docs)]
                    fn $visit_name(&mut self, _node: &$ast_type) { }
                    #[allow(missing_docs)]
                    fn [<$visit_name _end>](&mut self, _node: &$ast_type) { }
                )+

                $(
                    $(
                        #[$meta]
                        #[allow(missing_docs)]
                        fn $meta_visit_name(&mut self, _node: &$meta_ast_type) { }
                        #[$meta]
                        #[allow(missing_docs)]
                        fn [<$meta_visit_name _end>](&mut self, _node: &$meta_ast_type) { }
                    )+
                )+
            }

            $(
                #[allow(missing_docs)]
                fn $visit_token(&mut self, _token: &Token) { }
            )+
        }

        /// A trait that implements functions to listen for specific nodes/tokens.
        /// Unlike [`Visitor`], nodes/tokens passed are mutable.
        pub trait VisitorMut {
            /// Visit the nodes of an [`Ast`](crate::ast::Ast)
            fn visit_ast(&mut self, ast: Ast) -> Ast where Self: Sized {
                // TODO: Visit tokens?
                let eof = ast.eof().to_owned();
                let nodes = ast.nodes.visit_mut(self);

                Ast {
                    nodes,
                    // Everything gets cloned with this visitor, so there's no original tokens
                    eof: self.visit_eof(eof),
                }
            }

            paste::item! {
                $(
                    #[allow(missing_docs)]
                    fn $visit_name(&mut self, node: $ast_type) -> $ast_type {
                        node
                    }

                    #[allow(missing_docs)]
                    fn [<$visit_name _end>](&mut self, node: $ast_type) -> $ast_type {
                        node
                    }
                )+

                $(
                    #[$meta]
                    $(
                        #[$meta]
                        #[allow(missing_docs)]
                        fn $meta_visit_name(&mut self, node: $meta_ast_type) -> $meta_ast_type {
                            node
                        }

                        #[$meta]
                        #[allow(missing_docs)]
                        fn [<$meta_visit_name _end>](&mut self, node: $meta_ast_type) -> $meta_ast_type {
                            node
                        }
                    )+
                )+
            }

            $(
                #[allow(missing_docs)]
                fn $visit_token(&mut self, token: Token) -> Token {
                    token
                }
            )+
        }
    };
}

#[doc(hidden)]
pub trait Visit: Sealed {
    fn visit<V: Visitor>(&self, visitor: &mut V);
}

#[doc(hidden)]
pub trait VisitMut: Sealed
where
    Self: Sized,
{
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self;
}

impl<T: Visit> Visit for &T {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<T: Visit> Visit for &mut T {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<T: Visit> Visit for Vec<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        for item in self {
            item.visit(visitor);
        }
    }
}

impl<T: VisitMut> VisitMut for Vec<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        self.into_iter()
            .map(|item| item.visit_mut(visitor))
            .collect()
    }
}

impl<T: Visit> Visit for Option<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        if let Some(item) = self {
            item.visit(visitor);
        }
    }
}

impl<T: VisitMut> VisitMut for Option<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        self.map(|item| item.visit_mut(visitor))
    }
}

impl<A: Visit, B: Visit> Visit for (A, B) {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        self.0.visit(visitor);
        self.1.visit(visitor);
    }
}

impl<A: VisitMut, B: VisitMut> VisitMut for (A, B) {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        (self.0.visit_mut(visitor), self.1.visit_mut(visitor))
    }
}

impl<T: Visit> Visit for Box<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<T: VisitMut> VisitMut for Box<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        Box::new((*self).visit_mut(visitor))
    }
}

create_visitor!(ast: {
    visit_anonymous_call => FunctionArgs,
    visit_assignment => Assignment,
    visit_block => Block,
    visit_call => Call,
    visit_contained_span => ContainedSpan,
    visit_do => Do,
    visit_else_if => ElseIf,
    visit_eof => TokenReference,
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
    visit_token_reference => TokenReference,
    visit_un_op => UnOp,
    visit_value => Value,
    visit_var => Var,
    visit_var_expression => VarExpression,
    visit_while => While,

    // Types
    #[cfg(feature = "roblox")] {
        visit_compound_assignment => CompoundAssignment,
        visit_compound_op => CompoundOp,
        visit_else_if_expression => ElseIfExpression,
        visit_exported_type_declaration => ExportedTypeDeclaration,
        visit_generic_declaration => GenericDeclaration,
        visit_generic_declaration_parameter => GenericDeclarationParameter,
        visit_generic_parameter_info => GenericParameterInfo,
        visit_if_expression => IfExpression,
        visit_indexed_type_info => IndexedTypeInfo,
        visit_type_argument => TypeArgument,
        visit_type_assertion => TypeAssertion,
        visit_type_declaration => TypeDeclaration,
        visit_type_field => TypeField,
        visit_type_field_key => TypeFieldKey,
        visit_type_info => TypeInfo,
        visit_type_specifier => TypeSpecifier,
    }

    // Lua 5.2
    #[cfg(feature = "lua52")] {
        visit_goto => Goto,
        visit_label => Label,
    }
}, token: {
    visit_identifier,
    visit_multi_line_comment,
    visit_number,
    visit_single_line_comment,
    visit_string_literal,
    visit_symbol,
    visit_token,
    visit_whitespace,
});
