//! Contains the types necessary to parse [Roblox's typed Lua](https://devforum.roblox.com/t/luau-type-checking-beta/435382).
//! Only usable when the "roblox" feature flag is enabled.
use super::{punctuated::Punctuated, span::ContainedSpan, *};

/// Any type, such as `string`, `boolean?`, `number | boolean`, etc.
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TypeInfo<'a> {
	/// A standalone type, such as `string` or `Foo`.
	Basic(#[cfg_attr(feature = "serde", serde(borrow))] TokenReference<'a>),

	/// A callback type, such as `(string, number) => boolean`.
	Callback {
		/// The argument types: `(string, number)`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		arguments: Punctuated<'a, TypeInfo<'a>>,
		/// The parentheses for the arguments.
		#[cfg_attr(feature = "serde", serde(borrow))]
		parentheses: ContainedSpan<'a>,
		/// The "fat arrow" (`=>`) in between the arguments and the return type.
		#[cfg_attr(feature = "serde", serde(borrow))]
		arrow: TokenReference<'a>,
		/// The return type: `boolean`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		return_type: Box<TypeInfo<'a>>,
	},

	/// A type using generics, such as `map<number, string>`.
	Generic {
		/// The type that has generics: `map`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		base: TokenReference<'a>,
		/// The arrows (`<>`) containing the type parameters.
		#[cfg_attr(feature = "serde", serde(borrow))]
		arrows: ContainedSpan<'a>,
		/// The type parameters: `number, string`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		generics: Punctuated<'a, TypeInfo<'a>>,
	},

	/// An optional type, such as `string?`.
	Optional {
		/// The type that is optional: `string`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		base: Box<TypeInfo<'a>>,
		/// The question mark: `?`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		question_mark: TokenReference<'a>,
	},

	/// A type annotating the structure of a table: { foo: number, bar: string }
	Table {
		/// The braces (`{}`) containing the fields.
		#[cfg_attr(feature = "serde", serde(borrow))]
		braces: ContainedSpan<'a>,
		/// The fields: `foo: number, bar: string`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		fields: Punctuated<'a, TypeField<'a>>,
	},

	/// A type in the form of `typeof(foo)`.
	Typeof {
		/// The token `typeof`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		typeof_token: TokenReference<'a>,
		/// The parentheses used to contain the expression.
		#[cfg_attr(feature = "serde", serde(borrow))]
		parentheses: ContainedSpan<'a>,
		/// The inner expression: `foo`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		inner: Box<Expression<'a>>,
	},

	/// A tuple expression: `(string, number)`.
	Tuple {
		/// The parentheses used to contain the types
		#[cfg_attr(feature = "serde", serde(borrow))]
		parentheses: ContainedSpan<'a>,
		/// The types: `(string, number)`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		types: Punctuated<'a, TypeInfo<'a>>,
	},

	/// A union type: `string | number`, denoting one or the other.
	Union {
		/// The left hand side: `string`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		left: Box<TypeInfo<'a>>,
		/// The right hand side: `number`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		right: Box<TypeInfo<'a>>,
		/// The pipe (`|`) to separate the types.
		#[cfg_attr(feature = "serde", serde(borrow))]
		pipe: TokenReference<'a>,
	},
}

/// A type field used within table types.
/// The `foo: number` in `{ foo: number }`.
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeField<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) key: TypeFieldKey<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) colon: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) value: TypeInfo<'a>,
}

impl<'a> TypeField<'a> {
	/// The key of the field, `foo` in `foo: number`.
	pub fn key(&self) -> &TypeFieldKey<'a> {
		&self.key
	}

	/// The colon in between the key name and the value type.
	pub fn colon_token(&self) -> &TokenReference<'a> {
		&self.colon
	}

	/// The type for the field, `number` in `foo: number`.
	pub fn value(&self) -> &TypeInfo<'a> {
		&self.value
	}
}

/// A key in a [`TypeField`](struct.TypeField.html). Can either be a name or an index signature.
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TypeFieldKey<'a> {
	/// A name, such as `foo`.
	Name(TokenReference<'a>),

	/// An index signature, such as `[number]`.
	IndexSignature {
		/// The brackets (`[]`) used to contain the type.
		#[cfg_attr(feature = "serde", serde(borrow))]
		brackets: ContainedSpan<'a>,

		/// The type for the index signature, `number` in `[number]`.
		#[cfg_attr(feature = "serde", serde(borrow))]
		inner: TypeInfo<'a>,
	},
}

/// A type assertion using `as`, such as `as number`.
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AsAssertion<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) as_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) cast_to: TypeInfo<'a>,
}

impl<'a> AsAssertion<'a> {
	/// The token `as`.
	pub fn as_token(&self) -> &TokenReference<'a> {
		&self.as_token
	}

	/// The type to cast the expression into, `number` in `as number`.
	pub fn cast_to(&self) -> &TypeInfo<'a> {
		&self.cast_to
	}
}

/// A type declaration, such as `type Meters = number`
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeDeclaration<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) type_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) base: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) generics: Option<GenericDeclaration<'a>>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) equal_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) declare_as: TypeInfo<'a>,
}

impl<'a> TypeDeclaration<'a> {
	/// The token `type`.
	pub fn type_token(&self) -> &TokenReference<'a> {
		&self.type_token
	}

	/// The name of the type, `Meters` in `type Meters = number`.
	pub fn type_name(&self) -> &TokenReference<'a> {
		&self.base
	}

	/// The generics of the type, if there are any. `<T>` in `type Foo<T> = T`.
	pub fn generics(&self) -> Option<&GenericDeclaration<'a>> {
		self.generics.as_ref()
	}

	/// The `=` token in between the type name and the definition.
	pub fn equal_token(&self) -> &TokenReference<'a> {
		&self.equal_token
	}

	/// The definition of the type, `number` in `type Meters = number`.
	pub fn type_definition(&self) -> &TypeInfo<'a> {
		&self.declare_as
	}
}

/// The generics used in a [type declaration](struct.TypeDeclaration.html).
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericDeclaration<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) arrows: ContainedSpan<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) generics: Punctuated<'a, TokenReference<'a>>,
}

impl<'a> GenericDeclaration<'a> {
	/// The arrows (`<>`) containing the types.
	pub fn arrows(&self) -> &ContainedSpan<'a> {
		&self.arrows
	}

	/// The names of the generics: `T, U` in `<T, U>`.
	pub fn generics(&self) -> &Punctuated<'a, TokenReference<'a>> {
		&self.generics
	}
}

/// A type specifier, the `: number` in `local foo: number`
#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeSpecifier<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) punctuation: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) type_info: TypeInfo<'a>,
}

impl<'a> TypeSpecifier<'a> {
	/// The punctuation being used.
	/// `:` for `local foo: number`.
	pub fn punctuation(&self) -> &TokenReference<'a> {
		&self.punctuation
	}

	/// The type being specified: `number` in `local foo: number`.
	pub fn type_info(&self) -> &TypeInfo<'a> {
		&self.type_info
	}
}
