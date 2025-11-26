//! Contains the types necessary to parse [Luau](https://luau-lang.org/).
//! The module name is a misnomer from when Luau was just types.
//! It will be renamed to "luau" in the future.
use super::{punctuated::Punctuated, span::ContainedSpan, *};
use crate::{
    util::display_option,
    visitors::{Visit, VisitMut},
    ShortString,
};
use derive_more::Display;

/// Any type, such as `string`, `boolean?`, `number | boolean`, etc.
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum TypeInfo {
    /// A shorthand type annotating the structure of an array: { number }
    #[display(
        "{}{}{}{}",
        braces.tokens().0,
        display_option(access),
        type_info,
        braces.tokens().1
    )]
    Array {
        /// The braces (`{}`) containing the type info.
        braces: ContainedSpan,
        /// The access modifer of the array, `read` in `{ read number }`.
        #[cfg_attr(
            feature = "serde",
            serde(default, skip_serializing_if = "Option::is_none")
        )]
        access: Option<TokenReference>,
        /// The type info for the values in the Array
        type_info: Box<TypeInfo>,
    },

    /// A standalone type, such as `string` or `Foo`.
    #[display("{_0}")]
    Basic(TokenReference),

    /// A singleton string type, such as `"hello"`
    #[display("{_0}")]
    String(TokenReference),

    /// A singleton boolean type, such as `true`
    #[display("{_0}")]
    Boolean(TokenReference),

    /// A callback type, such as `(string, number) => boolean`.
    #[display(
        "{}{}{arguments}{}{arrow}{return_type}",
        display_option(generics),
        parentheses.tokens().0,
        parentheses.tokens().1
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
        "{}{}{}{}",
        base,
        arrows.tokens().0,
        generics,
        arrows.tokens().1
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
    #[display("{name}{ellipsis}")]
    GenericPack {
        /// The name of the type that is generic: `T`.
        name: TokenReference,
        /// The ellipsis: `...`.
        ellipsis: TokenReference,
    },

    /// An intersection type, such as `string & number`.
    #[display("{_0}")]
    Intersection(TypeIntersection),

    /// A type coming from a module, such as `module.Foo`
    #[display("{module}{punctuation}{type_info}")]
    Module {
        /// The module the type is coming from: `module`.
        module: TokenReference,
        /// The punctuation (`.`) to index the module.
        punctuation: TokenReference,
        /// The indexed type info: `Foo`.
        type_info: Box<IndexedTypeInfo>,
    },

    /// An optional type, such as `string?`.
    #[display("{base}{question_mark}")]
    Optional {
        /// The type that is optional: `string`.
        base: Box<TypeInfo>,
        /// The question mark: `?`.
        question_mark: TokenReference,
    },

    /// A type annotating the structure of a table: { foo: number, bar: string }
    #[display("{}{}{}", braces.tokens().0, fields, braces.tokens().1)]
    Table {
        /// The braces (`{}`) containing the fields.
        braces: ContainedSpan,
        /// The fields: `foo: number, bar: string`.
        fields: Punctuated<TypeField>,
    },

    /// A type in the form of `typeof(foo)`.
    #[display(
        "{}{}{}{}",
        typeof_token,
        parentheses.tokens().0,
        inner,
        parentheses.tokens().1
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
        "{}{}{}",
        parentheses.tokens().0,
        types,
        parentheses.tokens().1
    )]
    Tuple {
        /// The parentheses used to contain the types
        parentheses: ContainedSpan,
        /// The types: `(string, number)`.
        types: Punctuated<TypeInfo>,
    },

    /// A union type, such as `string | number`.
    #[display("{_0}")]
    Union(TypeUnion),

    /// A variadic type: `...number`.
    #[display("{ellipsis}{type_info}")]
    Variadic {
        /// The ellipsis: `...`.
        ellipsis: TokenReference,
        /// The type that is variadic: `number`.
        type_info: Box<TypeInfo>,
    },

    /// A variadic type pack: `...T` in `Function<...T>`
    #[display("{ellipsis}{name}")]
    VariadicPack {
        /// The ellipsis: `...`
        ellipsis: TokenReference,
        /// The name of the type that is variadic: `T`
        name: TokenReference,
    },
}

/// A union type, such as `string | number`.
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{}{types}", display_option(leading))]
pub struct TypeUnion {
    pub(crate) leading: Option<TokenReference>,
    pub(crate) types: Punctuated<TypeInfo>,
}

impl TypeUnion {
    /// Creates a new Union from the given types and optional leading pipe.
    pub fn new(leading: Option<TokenReference>, types: Punctuated<TypeInfo>) -> Self {
        Self { leading, types }
    }

    /// Returns a new Union with the given types.
    pub fn with_types(self, types: Punctuated<TypeInfo>) -> Self {
        Self { types, ..self }
    }

    /// Returns a new Union with the given leading pipe.
    pub fn with_leading(self, leading: Option<TokenReference>) -> Self {
        Self { leading, ..self }
    }

    /// The leading pipe, if one is present: `|`.
    pub fn leading(&self) -> Option<&TokenReference> {
        self.leading.as_ref()
    }

    /// The types being unioned: `string | number`.
    pub fn types(&self) -> &Punctuated<TypeInfo> {
        &self.types
    }
}

/// An intersection type, such as `string & number`.
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{}{types}", display_option(leading))]
pub struct TypeIntersection {
    pub(crate) leading: Option<TokenReference>,
    pub(crate) types: Punctuated<TypeInfo>,
}

impl TypeIntersection {
    /// Creates a new Intersection from the given types.
    pub fn new(leading: Option<TokenReference>, types: Punctuated<TypeInfo>) -> Self {
        Self { leading, types }
    }

    /// Returns a new Intersection with the given types.
    pub fn with_types(self, types: Punctuated<TypeInfo>) -> Self {
        Self { types, ..self }
    }

    /// Returns a new Intersection with the given leading ampersand.
    pub fn with_leading(self, leading: Option<TokenReference>) -> Self {
        Self { leading, ..self }
    }

    /// The leading ampersand, if one is present: `&`.
    pub fn leading(&self) -> Option<&TokenReference> {
        self.leading.as_ref()
    }

    /// The types being intersected: `string & number`.
    pub fn types(&self) -> &Punctuated<TypeInfo> {
        &self.types
    }
}

/// A subset of TypeInfo that consists of items which can only be used as an index, such as `Foo` and `Foo<Bar>`,
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum IndexedTypeInfo {
    /// A standalone type, such as `string` or `Foo`.
    #[display("{_0}")]
    Basic(TokenReference),

    /// A type using generics, such as `map<number, string>`.
    #[display("{base}{}{generics}{}", arrows.tokens().0, arrows.tokens().1)]
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
#[display("{}{key}{colon}{value}", display_option(access))]
pub struct TypeField {
    #[cfg_attr(
        feature = "serde",
        serde(default, skip_serializing_if = "Option::is_none")
    )]
    pub(crate) access: Option<TokenReference>,
    pub(crate) key: TypeFieldKey,
    pub(crate) colon: TokenReference,
    pub(crate) value: TypeInfo,
}

impl TypeField {
    /// Creates a new TypeField from the given key and value
    pub fn new(key: TypeFieldKey, value: TypeInfo) -> Self {
        Self {
            access: None,
            key,
            colon: TokenReference::symbol(": ").unwrap(),
            value,
        }
    }

    /// The access modifier of the field, `read` in `read foo: number`.
    pub fn access(&self) -> Option<&TokenReference> {
        self.access.as_ref()
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

    /// Returns a new TypeField with the given access modifier.
    pub fn with_access(self, access: Option<TokenReference>) -> Self {
        Self { access, ..self }
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
    #[display("{_0}")]
    Name(TokenReference),

    /// An index signature, such as `[number]`.
    #[display("{}{}{}", brackets.tokens().0, inner, brackets.tokens().1)]
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
#[display("{assertion_op}{cast_to}")]
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
    "{}{}{}{}{}",
    type_token,
    base,
    display_option(generics),
    equal_token,
    declare_as
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
            type_token: TokenReference::new(
                Vec::new(),
                Token::new(TokenType::Identifier {
                    identifier: "type".into(),
                }),
                vec![Token::new(TokenType::spaces(1))],
            ),
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
#[derive(Clone, Debug, Display, PartialEq, Eq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum GenericParameterInfo {
    /// A name, such as `foo`.
    #[display("{_0}")]
    Name(TokenReference),

    /// A variadic type pack: `T...`.
    #[display("{name}{ellipsis}")]
    Variadic {
        /// The name of the type that is variadic: `T`.
        name: TokenReference,
        /// The ellipsis: `...`.
        ellipsis: TokenReference,
    },
}
/// A generic declaration parameter us in [`GenericDeclaration`]. Consists of a [`GenericParameterInfo`] and an optional default type.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    "{}{}{}",
    parameter,
    display_option(self.equals()),
    display_option(self.default_type())
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
#[display("{}{}{}", arrows.tokens().0, generics, arrows.tokens().1)]
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
#[display("{punctuation}{type_info}")]
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
#[display("{export_token}{type_declaration}")]
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

/// A user defined type function, such as `type function foo() ... end`.
///
/// See more: https://github.com/luau-lang/rfcs/blob/master/docs/user-defined-type-functions.md
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{}{}{}{}", type_token, function_token, function_name, function_body)]
pub struct TypeFunction {
    pub(crate) type_token: TokenReference,
    pub(crate) function_token: TokenReference,
    pub(crate) function_name: TokenReference,
    pub(crate) function_body: FunctionBody,
}

impl TypeFunction {
    /// Creates a new TypeFunction from the given function name and body
    pub fn new(function_name: TokenReference, function_body: FunctionBody) -> Self {
        Self {
            type_token: TokenReference::new(
                Vec::new(),
                Token::new(TokenType::Identifier {
                    identifier: "type".into(),
                }),
                vec![Token::new(TokenType::spaces(1))],
            ),
            function_token: TokenReference::basic_symbol("function "),
            function_name,
            function_body,
        }
    }

    /// The token `type`.
    pub fn type_token(&self) -> &TokenReference {
        &self.type_token
    }

    /// The token `function`.
    pub fn function_token(&self) -> &TokenReference {
        &self.function_token
    }

    /// The name of the type function, `Pairs` in `type function Pairs() ... end`.
    pub fn function_name(&self) -> &TokenReference {
        &self.function_name
    }

    /// The body of the type function.
    pub fn function_body(&self) -> &FunctionBody {
        &self.function_body
    }

    /// Returns a new TypeFunction with the given `type` token
    pub fn with_type_token(self, type_token: TokenReference) -> Self {
        Self { type_token, ..self }
    }

    /// Returns a new TypeFunction with the given function name
    pub fn with_function_token(self, function_token: TokenReference) -> Self {
        Self {
            function_token,
            ..self
        }
    }

    /// Returns a new TypeFunction with the given function name
    pub fn with_function_name(self, function_name: TokenReference) -> Self {
        Self {
            function_name,
            ..self
        }
    }

    /// Returns a new TypeFunction with the given function body
    pub fn with_function_body(self, function_body: FunctionBody) -> Self {
        Self {
            function_body,
            ..self
        }
    }
}

/// An exported type function, such as `export type function Pairs() ... end`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{export_token}{type_function}")]
pub struct ExportedTypeFunction {
    pub(crate) export_token: TokenReference,
    pub(crate) type_function: TypeFunction,
}

impl ExportedTypeFunction {
    /// Creates a new ExportedTypeFunction with the given type function
    pub fn new(type_function: TypeFunction) -> Self {
        Self {
            export_token: TokenReference::new(
                vec![],
                Token::new(TokenType::Identifier {
                    identifier: ShortString::new("export"),
                }),
                vec![Token::new(TokenType::spaces(1))],
            ),
            type_function,
        }
    }

    /// The token `export`.
    pub fn export_token(&self) -> &TokenReference {
        &self.export_token
    }

    /// The type function, `type function Pairs() ... end`.
    pub fn type_function(&self) -> &TypeFunction {
        &self.type_function
    }

    /// Returns a new ExportedTypeFunction with the `export` token
    pub fn with_export_token(self, export_token: TokenReference) -> Self {
        Self {
            export_token,
            ..self
        }
    }

    /// Returns a new ExportedTypeFunction with the given type function
    pub fn with_type_function(self, type_function: TypeFunction) -> Self {
        Self {
            type_function,
            ..self
        }
    }
}

/// A compound assignment operator, such as `+=`, `-=`, etc.
/// This has been moved to `compound.rs` since CfxLua makes use of it as well.
#[cfg(not(feature = "luau"))]
#[cfg_attr(docsrs, doc(cfg(not(feature = "luau"))))]
#[deprecated(
    note = "CompoundAssignment has been moved to full_moon::ast::compound::CompoundAssignment"
)]
pub type CompoundAssignment = crate::ast::compound::CompoundAssignment;

/// The operator in a compound assignment, such as `+=`.
/// This has been moved to `compound.rs` since CfxLua makes use of it as well.
#[cfg(not(feature = "luau"))]
#[cfg_attr(docsrs, doc(cfg(not(feature = "luau"))))]
#[deprecated(note = "CompoundOp has been moved to full_moon::ast::compound::CompoundOp")]
pub type CompoundOp = crate::ast::compound::CompoundOp;

/// An if statement
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    "{}{}{}{}{}{}{}",
    if_token,
    condition,
    then_token,
    if_expression,
    display_option(else_if_expressions.as_ref().map(join_vec)),
    else_token,
    else_expression
)]
pub struct IfExpression {
    pub(crate) if_token: TokenReference,
    pub(crate) condition: Box<Expression>,
    pub(crate) then_token: TokenReference,
    pub(crate) if_expression: Box<Expression>,
    pub(crate) else_if_expressions: Option<Vec<ElseIfExpression>>,
    pub(crate) else_token: TokenReference,
    pub(crate) else_expression: Box<Expression>,
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

/// An interpolated string, such as `` `hello, {"world"}!` ``.
/// "segments", made up of [`InterpolatedStringSegment`]s, is each part of the string,
/// up until the `last_string`.
/// The number of segments is the number of expressions used.
/// For example, `` `1{2}3` `` would have one segment, with literal "1" (marked with a
/// [`TokenType`](crate::tokenizer::TokenType) of `InterpolatedString { token: "1", kind: InterpolatedStringKind::Begin }`),
/// and the expression `2`.
/// The `last_string` would be the literal 3, with a backtick afterwards.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{}{}", join_vec(segments), last_string)]
pub struct InterpolatedString {
    pub(crate) segments: Vec<InterpolatedStringSegment>,
    pub(crate) last_string: TokenReference,
}

impl InterpolatedString {
    /// Creates a new InterpolatedString from the given segments and last string
    pub fn new(segments: Vec<InterpolatedStringSegment>, last_string: TokenReference) -> Self {
        Self {
            segments,
            last_string,
        }
    }

    /// The segments of the interpolated string
    pub fn segments(&self) -> impl Iterator<Item = &InterpolatedStringSegment> {
        self.segments.iter()
    }

    /// The last string of the interpolated string
    pub fn last_string(&self) -> &TokenReference {
        &self.last_string
    }

    /// Returns just the expressions
    pub fn expressions(&self) -> impl Iterator<Item = &Expression> {
        ExpressionsIterator {
            segments: &self.segments,
            index: 0,
        }
    }

    /// Returns a new InterpolatedString with the given segments
    pub fn with_segments(self, segments: Vec<InterpolatedStringSegment>) -> Self {
        Self { segments, ..self }
    }

    /// Returns a new InterpolatedString with the given last string
    pub fn with_last_string(self, last_string: TokenReference) -> Self {
        Self {
            last_string,
            ..self
        }
    }
}

/// Segments of an interpolated string, as seen in [`InterpolatedString`].
/// Read the documentation for [`InterpolatedString`] for more information.
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{literal}{expression}")]
pub struct InterpolatedStringSegment {
    /// The literal part of the segment. Guaranteed to be of TokenType::InterpolatedString
    pub literal: TokenReference,

    /// The expression being formatted
    pub expression: Expression,
}

impl Visit for InterpolatedStringSegment {
    fn visit<V: crate::visitors::Visitor>(&self, visitor: &mut V) {
        self.literal.visit(visitor);
        self.expression.visit(visitor);
    }
}

impl VisitMut for InterpolatedStringSegment {
    fn visit_mut<V: crate::visitors::VisitorMut>(self, visitor: &mut V) -> Self {
        Self {
            literal: self.literal.visit_mut(visitor),
            expression: self.expression.visit_mut(visitor),
        }
    }
}

struct ExpressionsIterator<'a> {
    segments: &'a [InterpolatedStringSegment],
    index: usize,
}

impl<'a> Iterator for ExpressionsIterator<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.segments.len() {
            return None;
        }

        let segment = &self.segments[self.index];
        self.index += 1;

        Some(&segment.expression)
    }
}

/// An attribute, such as `@native`
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{at_sign}{name}")]
pub struct LuauAttribute {
    pub(crate) at_sign: TokenReference,
    pub(crate) name: TokenReference,
}

impl LuauAttribute {
    /// Creates a new ElseIf from the given condition
    pub fn new(name: TokenReference) -> Self {
        Self {
            at_sign: TokenReference::symbol("@").unwrap(),
            name,
        }
    }

    /// The `@` token
    pub fn at_sign(&self) -> &TokenReference {
        &self.at_sign
    }

    /// The name of the attribute, `native` in `@native`
    pub fn name(&self) -> &TokenReference {
        &self.name
    }

    /// Returns a new Attribute with the given `@` token
    pub fn with_at_sign(self, at_sign: TokenReference) -> Self {
        Self { at_sign, ..self }
    }

    /// Returns a new Attribute with the given name
    pub fn with_name(self, name: TokenReference) -> Self {
        Self { name, ..self }
    }
}

/// The `<<T>>` in both `f<<T>>`, in which case it is a [`Suffix`](crate::ast::Suffix),
/// or in `f::<<T>>()`, where it is a member of [`MethodCall`](crate::ast::MethodCall).
#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
        "{}{}{}{}{}",
        outer_arrows.tokens().0,
        inner_arrows.tokens().0,
        types,
        inner_arrows.tokens().1,
        outer_arrows.tokens().1
)]
pub struct TypeInstantiation {
    /// The initial `<` and `>`
    #[node(full_range)]
    // #[visit(contains = "inner_arrows")]
    pub(crate) outer_arrows: ContainedSpan,

    /// The inner `<` and `>`
    // #[visit(contains = "types")]
    pub(crate) inner_arrows: ContainedSpan,

    /// The list of types
    pub(crate) types: Punctuated<TypeInfo>,
}

impl TypeInstantiation {
    /// Creates an empty TypeInstantiation
    pub fn new() -> Self {
        Self {
            outer_arrows: ContainedSpan::new(
                TokenReference::symbol("<").unwrap(),
                TokenReference::symbol(">").unwrap(),
            ),

            inner_arrows: ContainedSpan::new(
                TokenReference::symbol("<").unwrap(),
                TokenReference::symbol(">").unwrap(),
            ),

            types: Punctuated::new(),
        }
    }

    /// The first pair of arrows of a type instantiation.
    pub fn outer_arrows(&self) -> &ContainedSpan {
        &self.outer_arrows
    }

    /// The second pair of arrows of a type instantiation.
    pub fn inner_arrows(&self) -> &ContainedSpan {
        &self.inner_arrows
    }

    /// The types as part of the instantiation
    pub fn types(&self) -> &Punctuated<TypeInfo> {
        &self.types
    }

    /// Returns a new TypeInstantiation with the outer arrows replaced
    pub fn with_outer_arrows(self, outer_arrows: ContainedSpan) -> Self {
        Self {
            outer_arrows,
            ..self
        }
    }

    /// Returns a new TypeInstantiation with the inner arrows replaced
    pub fn with_inner_arrows(self, inner_arrows: ContainedSpan) -> Self {
        Self {
            inner_arrows,
            ..self
        }
    }

    /// Returns a new TypeInstantiation with the types replaced
    pub fn with_types(self, types: Punctuated<TypeInfo>) -> Self {
        Self { types, ..self }
    }
}
