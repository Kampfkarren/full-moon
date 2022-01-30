//! Contains the types necessary to parse [Roblox's typed Lua](https://devforum.roblox.com/t/luau-type-checking-beta/435382).
//! Only usable when the "roblox" feature flag is enabled.
use super::{punctuated::Punctuated, span::ContainedSpan, *};
use crate::{util::display_option, ShortString};
use derive_more::Display;

/// Any type, such as `string`, `boolean?`, `number | boolean`, etc.
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum TypeInfo {
    /// A shorthand type annotating the structure of an array: { number }
    #[display(fmt = "{}{}{}", "braces.tokens().0", "type_info", "braces.tokens().1")]
    Array {
        /// The braces (`{}`) containing the type info.
        braces: ContainedSpan,
        /// The type info for the values in the Array
        type_info: Box<TypeInfo>,
    },

    /// A standalone type, such as `string` or `Foo`.
    #[display(fmt = "{}", "_0")]
    Basic(TokenReference),

    /// A singleton string type, such as `"hello"`
    #[display(fmt = "{}", "_0")]
    String(TokenReference),

    /// A singleton boolean type, such as `true`
    #[display(fmt = "{}", "_0")]
    Boolean(TokenReference),

    /// A callback type, such as `(string, number) => boolean`.
    #[display(
        fmt = "{}{}{}{}{}{}",
        "display_option(generics)",
        "parentheses.tokens().0",
        "arguments",
        "parentheses.tokens().1",
        "arrow",
        "return_type"
    )]
    Callback {
        /// Optional generics provided for the arguments, such as in `<T>(T) -> string`
        generics: Option<GenericDeclaration>,
        /// The parentheses for the arguments.
        parentheses: ContainedSpan,
        /// The argument types: `(string, number)`.
        arguments: Punctuated<TypeArgument>,
        /// The "thin arrow" (`->`) in between the arguments and the return type.
        arrow: TokenReference,
        /// The return type: `boolean`.
        return_type: Box<TypeInfo>,
    },

    /// A type using generics, such as `map<number, string>`.
    #[display(
        fmt = "{}{}{}{}",
        "base",
        "arrows.tokens().0",
        "generics",
        "arrows.tokens().1"
    )]
    Generic {
        /// The type that has generics: `map`.
        base: TokenReference,
        /// The arrows (`<>`) containing the type parameters.
        arrows: ContainedSpan,
        /// The type parameters: `number, string`.
        generics: Punctuated<TypeInfo>,
    },

    /// A generic pack: `T...`.
    /// Note, these are only available as return types, when annotating a vararg (`...`) in a function parameter, or as a generic type argument.
    #[display(fmt = "{}{}", "name", "ellipse")]
    GenericPack {
        /// The name of the type that is generic: `T`.
        name: TokenReference,
        /// The ellipse: `...`.
        ellipse: TokenReference,
    },

    /// An intersection type: `string & number`, denoting both types.
    #[display(fmt = "{}{}{}", "left", "ampersand", "right")]
    Intersection {
        /// The left hand side: `string`.
        left: Box<TypeInfo>,
        /// The ampersand (`&`) to separate the types.
        ampersand: TokenReference,
        /// The right hand side: `number`.
        right: Box<TypeInfo>,
    },

    /// A type coming from a module, such as `module.Foo`
    #[display(fmt = "{}{}{}", "module", "punctuation", "type_info")]
    Module {
        /// The module the type is coming from: `module`.
        module: TokenReference,
        /// The punctuation (`.`) to index the module.
        punctuation: TokenReference,
        /// The indexed type info: `Foo`.
        type_info: Box<IndexedTypeInfo>,
    },

    /// An optional type, such as `string?`.
    #[display(fmt = "{}{}", "base", "question_mark")]
    Optional {
        /// The type that is optional: `string`.
        base: Box<TypeInfo>,
        /// The question mark: `?`.
        question_mark: TokenReference,
    },

    /// A type annotating the structure of a table: { foo: number, bar: string }
    #[display(fmt = "{}{}{}", "braces.tokens().0", "fields", "braces.tokens().1")]
    Table {
        /// The braces (`{}`) containing the fields.
        braces: ContainedSpan,
        /// The fields: `foo: number, bar: string`.
        fields: Punctuated<TypeField>,
    },

    /// A type in the form of `typeof(foo)`.
    #[display(
        fmt = "{}{}{}{}",
        "typeof_token",
        "parentheses.tokens().0",
        "inner",
        "parentheses.tokens().1"
    )]
    Typeof {
        /// The token `typeof`.
        typeof_token: TokenReference,
        /// The parentheses used to contain the expression.
        parentheses: ContainedSpan,
        /// The inner expression: `foo`.
        inner: Box<Expression>,
    },

    /// A tuple expression: `(string, number)`.
    #[display(
        fmt = "{}{}{}",
        "parentheses.tokens().0",
        "types",
        "parentheses.tokens().1"
    )]
    Tuple {
        /// The parentheses used to contain the types
        parentheses: ContainedSpan,
        /// The types: `(string, number)`.
        types: Punctuated<TypeInfo>,
    },

    /// A union type: `string | number`, denoting one or the other.
    #[display(fmt = "{}{}{}", "left", "pipe", "right")]
    Union {
        /// The left hand side: `string`.
        left: Box<TypeInfo>,
        /// The pipe (`|`) to separate the types.
        pipe: TokenReference,
        /// The right hand side: `number`.
        right: Box<TypeInfo>,
    },

    /// A variadic type: `...number`.
    #[display(fmt = "{}{}", "ellipse", "type_info")]
    Variadic {
        /// The ellipse: `...`.
        ellipse: TokenReference,
        /// The type that is variadic: `number`.
        type_info: Box<TypeInfo>,
    },

    /// A variadic type pack: `...T` in `Function<...T>`
    #[display(fmt = "{}{}", "ellipse", "name")]
    VariadicPack {
        /// The ellipse: `...`
        ellipse: TokenReference,
        /// The name of the type that is variadic: `T`
        name: TokenReference,
    },
}

/// A subset of TypeInfo that consists of items which can only be used as an index, such as `Foo` and `Foo<Bar>`,
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum IndexedTypeInfo {
    /// A standalone type, such as `string` or `Foo`.
    #[display(fmt = "{}", "_0")]
    Basic(TokenReference),

    /// A type using generics, such as `map<number, string>`.
    #[display(
        fmt = "{}{}{}{}",
        "base",
        "arrows.tokens().0",
        "generics",
        "arrows.tokens().1"
    )]
    Generic {
        /// The type that has generics: `map`.
        base: TokenReference,
        /// The arrows (`<>`) containing the type parameters.
        arrows: ContainedSpan,
        /// The type parameters: `number, string`.
        generics: Punctuated<TypeInfo>,
    },
}

/// A type field used within table types.
/// The `foo: number` in `{ foo: number }`.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "key", "colon", "value")]
pub struct TypeField {
    pub(crate) key: TypeFieldKey,
    pub(crate) colon: TokenReference,
    pub(crate) value: TypeInfo,
}

impl TypeField {
    /// Creates a new TypeField from the given key and value
    pub fn new(key: TypeFieldKey, value: TypeInfo) -> Self {
        Self {
            key,
            colon: TokenReference::symbol(": ").unwrap(),
            value,
        }
    }

    /// The key of the field, `foo` in `foo: number`.
    pub fn key(&self) -> &TypeFieldKey {
        &self.key
    }

    /// The colon in between the key name and the value type.
    pub fn colon_token(&self) -> &TokenReference {
        &self.colon
    }

    /// The type for the field, `number` in `foo: number`.
    pub fn value(&self) -> &TypeInfo {
        &self.value
    }

    /// Returns a new TypeField with the given key
    pub fn with_key(self, key: TypeFieldKey) -> Self {
        Self { key, ..self }
    }

    /// Returns a new TypeField with the `:` token
    pub fn with_colon_token(self, colon_token: TokenReference) -> Self {
        Self {
            colon: colon_token,
            ..self
        }
    }

    /// Returns a new TypeField with the `:` token
    pub fn with_value(self, value: TypeInfo) -> Self {
        Self { value, ..self }
    }
}

/// A key in a [`TypeField`]. Can either be a name or an index signature.
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum TypeFieldKey {
    /// A name, such as `foo`.
    #[display(fmt = "{}", "_0")]
    Name(TokenReference),

    /// An index signature, such as `[number]`.
    #[display(fmt = "{}{}{}", "brackets.tokens().0", "inner", "brackets.tokens().1")]
    IndexSignature {
        /// The brackets (`[]`) used to contain the type.
        brackets: ContainedSpan,

        /// The type for the index signature, `number` in `[number]`.
        inner: TypeInfo,
    },
}

/// A type assertion using `::`, such as `:: number`.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", "assertion_op", "cast_to")]
pub struct TypeAssertion {
    pub(crate) assertion_op: TokenReference,
    pub(crate) cast_to: TypeInfo,
}

impl TypeAssertion {
    /// Creates a new TypeAssertion from the given cast to TypeInfo
    pub fn new(cast_to: TypeInfo) -> Self {
        Self {
            assertion_op: TokenReference::symbol("::").unwrap(),
            cast_to,
        }
    }

    /// The token `::`.
    pub fn assertion_op(&self) -> &TokenReference {
        &self.assertion_op
    }

    /// The type to cast the expression into, `number` in `:: number`.
    pub fn cast_to(&self) -> &TypeInfo {
        &self.cast_to
    }

    /// Returns a new TypeAssertion with the given `::` token
    pub fn with_assertion_op(self, assertion_op: TokenReference) -> Self {
        Self {
            assertion_op,
            ..self
        }
    }

    /// Returns a new TypeAssertion with the given TypeInfo to cast to
    pub fn with_cast_to(self, cast_to: TypeInfo) -> Self {
        Self { cast_to, ..self }
    }
}

/// A type declaration, such as `type Meters = number`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}{}{}",
    "type_token",
    "base",
    "display_option(generics)",
    "equal_token",
    "declare_as"
)]
pub struct TypeDeclaration {
    pub(crate) type_token: TokenReference,
    pub(crate) base: TokenReference,
    pub(crate) generics: Option<GenericDeclaration>,
    pub(crate) equal_token: TokenReference,
    pub(crate) declare_as: TypeInfo,
}

impl TypeDeclaration {
    /// Creates a new TypeDeclaration from the given type name and type declaration
    pub fn new(type_name: TokenReference, type_definition: TypeInfo) -> Self {
        Self {
            type_token: TokenReference::symbol("type ").unwrap(),
            base: type_name,
            generics: None,
            equal_token: TokenReference::symbol(" = ").unwrap(),
            declare_as: type_definition,
        }
    }

    /// The token `type`.
    pub fn type_token(&self) -> &TokenReference {
        &self.type_token
    }

    /// The name of the type, `Meters` in `type Meters = number`.
    pub fn type_name(&self) -> &TokenReference {
        &self.base
    }

    /// The generics of the type, if there are any. `<T>` in `type Foo<T> = T`.
    pub fn generics(&self) -> Option<&GenericDeclaration> {
        self.generics.as_ref()
    }

    /// The `=` token in between the type name and the definition.
    pub fn equal_token(&self) -> &TokenReference {
        &self.equal_token
    }

    /// The definition of the type, `number` in `type Meters = number`.
    pub fn type_definition(&self) -> &TypeInfo {
        &self.declare_as
    }

    /// Returns a new TypeDeclaration with the given `type` token
    pub fn with_type_token(self, type_token: TokenReference) -> Self {
        Self { type_token, ..self }
    }

    /// Returns a new TypeDeclaration with the given type name
    pub fn with_type_name(self, type_name: TokenReference) -> Self {
        Self {
            base: type_name,
            ..self
        }
    }

    /// Returns a new TypeDeclaration with the given generics of the type
    pub fn with_generics(self, generics: Option<GenericDeclaration>) -> Self {
        Self { generics, ..self }
    }

    /// Returns a new TypeDeclaration with the given generics of the type
    pub fn with_equal_token(self, equal_token: TokenReference) -> Self {
        Self {
            equal_token,
            ..self
        }
    }

    /// Returns a new TypeDeclaration with the given generics of the type
    pub fn with_type_definition(self, type_definition: TypeInfo) -> Self {
        Self {
            declare_as: type_definition,
            ..self
        }
    }
}

/// A generic declaration parameter used in [`GenericDeclaration`]. Can either be a name or a variadic pack.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum GenericParameterInfo {
    /// A name, such as `foo`.
    #[display(fmt = "{}", "_0")]
    Name(TokenReference),

    /// A variadic type pack: `T...`.
    #[display(fmt = "{}{}", "name", "ellipse")]
    Variadic {
        /// The name of the type that is variadic: `T`.
        name: TokenReference,
        /// The ellipse: `...`.
        ellipse: TokenReference,
    },
}
/// A generic declaration parameter us in [`GenericDeclaration`]. Consists of a [`GenericParameterInfo`] and an optional default type.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}",
    "parameter",
    "display_option(self.equals())",
    "display_option(self.default_type())"
)]
pub struct GenericDeclarationParameter {
    pub(crate) parameter: GenericParameterInfo,
    pub(crate) default: Option<(TokenReference, TypeInfo)>,
}

impl GenericDeclarationParameter {
    /// Creates a new GenericDeclarationParameter
    pub fn new(parameter: GenericParameterInfo) -> Self {
        Self {
            parameter,
            default: None,
        }
    }

    /// The generic parameter
    pub fn parameter(&self) -> &GenericParameterInfo {
        &self.parameter
    }

    /// The equals symbol denoting a default type, if present
    pub fn equals(&self) -> Option<&TokenReference> {
        self.default.as_ref().map(|(equals, _)| equals)
    }

    /// The default type, if present
    pub fn default_type(&self) -> Option<&TypeInfo> {
        self.default.as_ref().map(|(_, default_type)| default_type)
    }

    /// Returns a new GenericDeclarationParameter with the given parameter info
    pub fn with_parameter(self, parameter: GenericParameterInfo) -> Self {
        Self { parameter, ..self }
    }

    /// Returns a new GenericDeclarationParameter with the given default type
    pub fn with_default(self, default: Option<(TokenReference, TypeInfo)>) -> Self {
        Self { default, ..self }
    }
}

/// The generics used in a [`TypeDeclaration`].
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "arrows.tokens().0", "generics", "arrows.tokens().1")]
pub struct GenericDeclaration {
    #[visit(contains = "generics")]
    pub(crate) arrows: ContainedSpan,
    pub(crate) generics: Punctuated<GenericDeclarationParameter>,
}

impl GenericDeclaration {
    /// Creates a new GenericDeclaration
    pub fn new() -> Self {
        Self {
            arrows: ContainedSpan::new(
                TokenReference::symbol("<").unwrap(),
                TokenReference::symbol(">").unwrap(),
            ),
            generics: Punctuated::new(),
        }
    }

    /// The arrows (`<>`) containing the types.
    pub fn arrows(&self) -> &ContainedSpan {
        &self.arrows
    }

    /// The names of the generics: `T, U` in `<T, U>`.
    pub fn generics(&self) -> &Punctuated<GenericDeclarationParameter> {
        &self.generics
    }

    /// Returns a new GenericDeclaration with the given arrows containing the types
    pub fn with_arrows(self, arrows: ContainedSpan) -> Self {
        Self { arrows, ..self }
    }

    /// Returns a new TypeDeclaration with the given names of the generics
    pub fn with_generics(self, generics: Punctuated<GenericDeclarationParameter>) -> Self {
        Self { generics, ..self }
    }
}

impl Default for GenericDeclaration {
    fn default() -> Self {
        Self::new()
    }
}

/// A type specifier, the `: number` in `local foo: number`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", "punctuation", "type_info")]
pub struct TypeSpecifier {
    pub(crate) punctuation: TokenReference,
    pub(crate) type_info: TypeInfo,
}

impl TypeSpecifier {
    /// Creates a new TypeSpecifier with the given type info
    pub fn new(type_info: TypeInfo) -> Self {
        Self {
            punctuation: TokenReference::symbol(": ").unwrap(),
            type_info,
        }
    }

    /// The punctuation being used.
    /// `:` for `local foo: number`.
    pub fn punctuation(&self) -> &TokenReference {
        &self.punctuation
    }

    /// The type being specified: `number` in `local foo: number`.
    pub fn type_info(&self) -> &TypeInfo {
        &self.type_info
    }

    /// Returns a new TypeSpecifier with the given punctuation
    pub fn with_punctuation(self, punctuation: TokenReference) -> Self {
        Self {
            punctuation,
            ..self
        }
    }

    /// Returns a new TypeSpecifier with the given type being specified
    pub fn with_type_info(self, type_info: TypeInfo) -> Self {
        Self { type_info, ..self }
    }
}

/// A type argument specified in a callback type, the `count: number` in `(count: number) -> ()`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeArgument {
    pub(crate) name: Option<(TokenReference, TokenReference)>,
    pub(crate) type_info: TypeInfo,
}

impl TypeArgument {
    /// Creates a new TypeArgument with the given type info
    pub fn new(type_info: TypeInfo) -> Self {
        Self {
            name: None,
            type_info,
        }
    }

    /// The name of the argument split into identifier and punctuation: `count:` in `count: number`.
    pub fn name(&self) -> Option<&(TokenReference, TokenReference)> {
        self.name.as_ref()
    }

    /// The type info for the argument: `number` in `count: number`.
    pub fn type_info(&self) -> &TypeInfo {
        &self.type_info
    }

    /// Returns a new TypeArgument with the given punctuation
    pub fn with_name(self, name: Option<(TokenReference, TokenReference)>) -> Self {
        Self { name, ..self }
    }

    /// Returns a new TypeArgument with the given type info
    pub fn with_type_info(self, type_info: TypeInfo) -> Self {
        Self { type_info, ..self }
    }
}

impl fmt::Display for TypeArgument {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if let Some((identifier, punctuation)) = self.name() {
            write!(formatter, "{}{}{}", identifier, punctuation, self.type_info)
        } else {
            write!(formatter, "{}", self.type_info)
        }
    }
}

/// An exported type declaration, such as `export type Meters = number`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", "export_token", "type_declaration")]
pub struct ExportedTypeDeclaration {
    pub(crate) export_token: TokenReference,
    pub(crate) type_declaration: TypeDeclaration,
}

impl ExportedTypeDeclaration {
    /// Creates a new ExportedTypeDeclaration with the given type declaration
    pub fn new(type_declaration: TypeDeclaration) -> Self {
        Self {
            export_token: TokenReference::new(
                vec![],
                Token::new(TokenType::Identifier {
                    identifier: ShortString::new("export"),
                }),
                vec![Token::new(TokenType::spaces(1))],
            ),
            type_declaration,
        }
    }

    /// The token `export`.
    pub fn export_token(&self) -> &TokenReference {
        &self.export_token
    }

    /// The type declaration, `type Meters = number`.
    pub fn type_declaration(&self) -> &TypeDeclaration {
        &self.type_declaration
    }

    /// Returns a new ExportedTypeDeclaration with the `export` token
    pub fn with_export_token(self, export_token: TokenReference) -> Self {
        Self {
            export_token,
            ..self
        }
    }

    /// Returns a new TypeDeclaration with the given type declaration
    pub fn with_type_declaration(self, type_declaration: TypeDeclaration) -> Self {
        Self {
            type_declaration,
            ..self
        }
    }
}

make_op!(CompoundOp,
    #[doc = "Compound operators, such as X += Y or X -= Y"]
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

impl CompoundOp {
    /// The token associated with the operator
    pub fn token(&self) -> &TokenReference {
        match self {
            Self::PlusEqual(token)
            | Self::MinusEqual(token)
            | Self::StarEqual(token)
            | Self::SlashEqual(token)
            | Self::PercentEqual(token)
            | Self::CaretEqual(token)
            | Self::TwoDotsEqual(token) => token,
        }
    }
}

/// A Compound Assignment statement, such as `x += 1` or `x -= 1`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "lhs", "compound_operator", "rhs")]
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

/// An if statement
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}{}{}{}{}",
    "if_token",
    "condition",
    "then_token",
    "if_expression",
    "display_option(else_if_expressions.as_ref().map(join_vec))",
    "else_token",
    "else_expression"
)]
pub struct IfExpression {
    pub(crate) if_token: TokenReference,
    pub(crate) condition: Expression,
    pub(crate) then_token: TokenReference,
    pub(crate) if_expression: Expression,
    pub(crate) else_if_expressions: Option<Vec<ElseIfExpression>>,
    pub(crate) else_token: TokenReference,
    pub(crate) else_expression: Expression,
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
            condition,
            then_token: TokenReference::symbol(" then").unwrap(),
            if_expression,
            else_if_expressions: None,
            else_token: TokenReference::symbol(" else ").unwrap(),
            else_expression,
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
        Self { condition, ..self }
    }

    /// Returns a new IfExpression with the given `then` token
    pub fn with_then_token(self, then_token: TokenReference) -> Self {
        Self { then_token, ..self }
    }

    /// Returns a new IfExpression with the given if expression
    pub fn with_if_expression(self, if_expression: Expression) -> Self {
        Self {
            if_expression,
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
            else_expression,
            ..self
        }
    }
}

/// An elseif expression in a bigger [`IfExpression`] expression
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}{}",
    "else_if_token",
    "condition",
    "then_token",
    "expression"
)]
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
