// Implementations of Visit and VisitMut that are not able to be automatically derived yet.
// Ideally everything would be derived.
use super::*;
use crate::visitors::{Visit, VisitMut, Visitor, VisitorMut};

// The following have `ContainedSpan`, which when automatically derived will visit the tokens containing
// before they visit what they're actually containing.
// For example, if there is an AST node that represents `(foo)`...
// Then visitors will visit this as `()foo`.
// This is fixed for structs with `#[visit(contains = "...")], but this is not supported on enums.
impl Visit for TypeInfo {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_type_info(self);
        match self {
            TypeInfo::Array { braces, type_info } => {
                braces.tokens.0.visit(visitor);
                type_info.visit(visitor);
                braces.tokens.1.visit(visitor);
            }
            TypeInfo::Basic(__self_0) => {
                __self_0.visit(visitor);
            }
            TypeInfo::String(__self_0) => {
                __self_0.visit(visitor);
            }
            TypeInfo::Boolean(__self_0) => {
                __self_0.visit(visitor);
            }
            TypeInfo::Callback {
                generics,
                parentheses,
                arguments,
                arrow,
                return_type,
            } => {
                generics.visit(visitor);
                parentheses.tokens.0.visit(visitor);
                arguments.visit(visitor);
                parentheses.tokens.1.visit(visitor);
                arrow.visit(visitor);
                return_type.visit(visitor);
            }
            TypeInfo::Generic {
                base,
                arrows,
                generics,
            } => {
                base.visit(visitor);
                arrows.tokens.0.visit(visitor);
                generics.visit(visitor);
                arrows.tokens.1.visit(visitor);
            }
            TypeInfo::GenericPack { name, ellipse } => {
                name.visit(visitor);
                ellipse.visit(visitor);
            }
            TypeInfo::Module {
                module,
                punctuation,
                type_info,
            } => {
                module.visit(visitor);
                punctuation.visit(visitor);
                type_info.visit(visitor);
            }
            TypeInfo::Optional {
                base,
                question_mark,
            } => {
                base.visit(visitor);
                question_mark.visit(visitor);
            }
            TypeInfo::Table { braces, fields } => {
                braces.tokens.0.visit(visitor);
                fields.visit(visitor);
                braces.tokens.1.visit(visitor);
            }
            TypeInfo::Typeof {
                typeof_token,
                parentheses,
                inner,
            } => {
                typeof_token.visit(visitor);
                parentheses.tokens.0.visit(visitor);
                inner.visit(visitor);
                parentheses.tokens.1.visit(visitor);
            }
            TypeInfo::Tuple { parentheses, types } => {
                parentheses.tokens.0.visit(visitor);
                types.visit(visitor);
                parentheses.tokens.1.visit(visitor);
            }
            TypeInfo::Union { left, pipe, right } => {
                left.visit(visitor);
                pipe.visit(visitor);
                right.visit(visitor);
            }
            TypeInfo::Intersection {
                left,
                ampersand,
                right,
            } => {
                left.visit(visitor);
                ampersand.visit(visitor);
                right.visit(visitor);
            }
            TypeInfo::Variadic { ellipse, type_info } => {
                ellipse.visit(visitor);
                type_info.visit(visitor);
            }
            TypeInfo::VariadicPack { ellipse, name } => {
                ellipse.visit(visitor);
                name.visit(visitor);
            }
        };
        visitor.visit_type_info_end(self);
    }
}

impl VisitMut for TypeInfo {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_type_info(self);
        self = match self {
            TypeInfo::Array {
                mut braces,
                mut type_info,
            } => {
                braces.tokens.0 = braces.tokens.0.visit_mut(visitor);
                type_info = type_info.visit_mut(visitor);
                braces.tokens.1 = braces.tokens.1.visit_mut(visitor);

                TypeInfo::Array { braces, type_info }
            }
            TypeInfo::Basic(__self_0) => TypeInfo::Basic(__self_0.visit_mut(visitor)),
            TypeInfo::Boolean(__self_0) => TypeInfo::Boolean(__self_0.visit_mut(visitor)),
            TypeInfo::String(__self_0) => TypeInfo::String(__self_0.visit_mut(visitor)),
            TypeInfo::Callback {
                mut generics,
                mut parentheses,
                mut arguments,
                mut arrow,
                mut return_type,
            } => {
                generics = generics.visit_mut(visitor);
                parentheses.tokens.0 = parentheses.tokens.0.visit_mut(visitor);
                arguments = arguments.visit_mut(visitor);
                parentheses.tokens.1 = parentheses.tokens.1.visit_mut(visitor);
                arrow = arrow.visit_mut(visitor);
                return_type = return_type.visit_mut(visitor);

                TypeInfo::Callback {
                    generics,
                    parentheses,
                    arguments,
                    arrow,
                    return_type,
                }
            }

            TypeInfo::Generic {
                mut base,
                mut arrows,
                mut generics,
            } => {
                base = base.visit_mut(visitor);
                arrows.tokens.0 = arrows.tokens.0.visit_mut(visitor);
                generics = generics.visit_mut(visitor);
                arrows.tokens.1 = arrows.tokens.1.visit_mut(visitor);

                TypeInfo::Generic {
                    arrows,
                    base,
                    generics,
                }
            }

            TypeInfo::GenericPack {
                mut name,
                mut ellipse,
            } => {
                name = name.visit_mut(visitor);
                ellipse = ellipse.visit_mut(visitor);

                TypeInfo::GenericPack { name, ellipse }
            }

            TypeInfo::Module {
                mut module,
                mut punctuation,
                mut type_info,
            } => {
                module = module.visit_mut(visitor);
                punctuation = punctuation.visit_mut(visitor);
                type_info = type_info.visit_mut(visitor);

                TypeInfo::Module {
                    module,
                    punctuation,
                    type_info,
                }
            }

            TypeInfo::Optional {
                base,
                question_mark,
            } => TypeInfo::Optional {
                base: base.visit_mut(visitor),
                question_mark: question_mark.visit_mut(visitor),
            },

            TypeInfo::Table {
                mut braces,
                mut fields,
            } => {
                braces.tokens.0 = braces.tokens.0.visit_mut(visitor);
                fields = fields.visit_mut(visitor);
                braces.tokens.1 = braces.tokens.1.visit_mut(visitor);

                TypeInfo::Table { braces, fields }
            }

            TypeInfo::Typeof {
                mut typeof_token,
                mut parentheses,
                mut inner,
            } => {
                typeof_token = typeof_token.visit_mut(visitor);
                parentheses.tokens.0 = parentheses.tokens.0.visit_mut(visitor);
                inner = inner.visit_mut(visitor);
                parentheses.tokens.1 = parentheses.tokens.1.visit_mut(visitor);

                TypeInfo::Typeof {
                    typeof_token,
                    parentheses,
                    inner,
                }
            }

            TypeInfo::Tuple {
                mut parentheses,
                mut types,
            } => {
                parentheses.tokens.0 = parentheses.tokens.0.visit_mut(visitor);
                types = types.visit_mut(visitor);
                parentheses.tokens.1 = parentheses.tokens.1.visit_mut(visitor);

                TypeInfo::Tuple { parentheses, types }
            }

            TypeInfo::Union { left, pipe, right } => TypeInfo::Union {
                left: left.visit_mut(visitor),
                pipe: pipe.visit_mut(visitor),
                right: right.visit_mut(visitor),
            },

            TypeInfo::Intersection {
                left,
                ampersand,
                right,
            } => TypeInfo::Intersection {
                left: left.visit_mut(visitor),
                ampersand: ampersand.visit_mut(visitor),
                right: right.visit_mut(visitor),
            },

            TypeInfo::Variadic { ellipse, type_info } => TypeInfo::Variadic {
                ellipse: ellipse.visit_mut(visitor),
                type_info: type_info.visit_mut(visitor),
            },

            TypeInfo::VariadicPack { ellipse, name } => TypeInfo::VariadicPack {
                ellipse: ellipse.visit_mut(visitor),
                name: name.visit_mut(visitor),
            },
        };
        self = visitor.visit_type_info_end(self);
        self
    }
}

impl Visit for IndexedTypeInfo {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_indexed_type_info(self);
        match self {
            IndexedTypeInfo::Basic(__self_0) => {
                __self_0.visit(visitor);
            }
            IndexedTypeInfo::Generic {
                base,
                arrows,
                generics,
            } => {
                base.visit(visitor);
                arrows.tokens.0.visit(visitor);
                generics.visit(visitor);
                arrows.tokens.1.visit(visitor);
            }
        };
        visitor.visit_indexed_type_info_end(self);
    }
}

impl VisitMut for IndexedTypeInfo {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_indexed_type_info(self);
        self = match self {
            IndexedTypeInfo::Basic(__self_0) => IndexedTypeInfo::Basic(__self_0.visit_mut(visitor)),

            IndexedTypeInfo::Generic {
                mut base,
                mut arrows,
                mut generics,
            } => {
                base = base.visit_mut(visitor);
                arrows.tokens.0 = arrows.tokens.0.visit_mut(visitor);
                generics = generics.visit_mut(visitor);
                arrows.tokens.1 = arrows.tokens.1.visit_mut(visitor);

                IndexedTypeInfo::Generic {
                    arrows,
                    base,
                    generics,
                }
            }
        };
        self = visitor.visit_indexed_type_info_end(self);
        self
    }
}

impl Visit for TypeFieldKey {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_type_field_key(self);
        match self {
            TypeFieldKey::Name(__self_0) => {
                __self_0.visit(visitor);
            }
            TypeFieldKey::IndexSignature { brackets, inner } => {
                brackets.tokens.0.visit(visitor);
                inner.visit(visitor);
                brackets.tokens.1.visit(visitor);
            }
        };
        visitor.visit_type_field_key_end(self);
    }
}

impl VisitMut for TypeFieldKey {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_type_field_key(self);
        self = match self {
            TypeFieldKey::Name(__self_0) => TypeFieldKey::Name(__self_0.visit_mut(visitor)),
            TypeFieldKey::IndexSignature {
                mut brackets,
                mut inner,
            } => {
                brackets.tokens.0 = brackets.tokens.0.visit_mut(visitor);
                inner = inner.visit_mut(visitor);
                brackets.tokens.1 = brackets.tokens.1.visit_mut(visitor);

                TypeFieldKey::IndexSignature { brackets, inner }
            }
        };
        self = visitor.visit_type_field_key_end(self);
        self
    }
}
