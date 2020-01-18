use super::{punctuated::Punctuated, span::ContainedSpan, *};

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TypeInfo<'a> {
	Basic(#[cfg_attr(feature = "serde", serde(borrow))] TokenReference<'a>),

	Callback {
		#[cfg_attr(feature = "serde", serde(borrow))]
		arguments: Punctuated<'a, TypeInfo<'a>>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		parentheses: ContainedSpan<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		arrow: TokenReference<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		return_type: Box<TypeInfo<'a>>,
	},

	Generic {
		#[cfg_attr(feature = "serde", serde(borrow))]
		base: TokenReference<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		arrows: ContainedSpan<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		generics: Punctuated<'a, TypeInfo<'a>>,
	},

	Optional {
		#[cfg_attr(feature = "serde", serde(borrow))]
		base: Box<TypeInfo<'a>>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		question_mark: TokenReference<'a>,
	},

	Table {
		#[cfg_attr(feature = "serde", serde(borrow))]
		braces: ContainedSpan<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		fields: Punctuated<'a, TypeField<'a>>,
	},

	Typeof {
		#[cfg_attr(feature = "serde", serde(borrow))]
		typeof_token: TokenReference<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		parentheses: ContainedSpan<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		inner: Box<Expression<'a>>,
	},

	Tuple {
		#[cfg_attr(feature = "serde", serde(borrow))]
		parentheses: ContainedSpan<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		types: Punctuated<'a, TypeInfo<'a>>,
	},

	Union {
		#[cfg_attr(feature = "serde", serde(borrow))]
		left: Box<TypeInfo<'a>>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		right: Box<TypeInfo<'a>>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		pipe: TokenReference<'a>,
	},
}

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
	pub fn key(&self) -> &TypeFieldKey<'a> {
		&self.key
	}

	pub fn colon_token(&self) -> &TokenReference<'a> {
		&self.colon
	}

	pub fn value(&self) -> &TypeInfo<'a> {
		&self.value
	}
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TypeFieldKey<'a> {
	Name(TokenReference<'a>),
	IndexSignature {
		#[cfg_attr(feature = "serde", serde(borrow))]
		brackets: ContainedSpan<'a>,
		#[cfg_attr(feature = "serde", serde(borrow))]
		inner: TypeInfo<'a>,
	},
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AsAssertion<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) as_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) coerce_to: TypeInfo<'a>,
}

impl<'a> AsAssertion<'a> {
	pub fn as_token(&self) -> &TokenReference<'a> {
		&self.as_token
	}

	pub fn coerce_to(&self) -> &TypeInfo<'a> {
		&self.coerce_to
	}
}

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
	pub fn type_token(&self) -> &TokenReference<'a> {
		&self.type_token
	}

	pub fn type_name(&self) -> &TokenReference<'a> {
		&self.base
	}

	pub fn generics(&self) -> Option<&GenericDeclaration<'a>> {
		self.generics.as_ref()
	}

	pub fn equal_token(&self) -> &TokenReference<'a> {
		&self.equal_token
	}

	pub fn type_definition(&self) -> &TypeInfo<'a> {
		&self.declare_as
	}
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericDeclaration<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) arrows: ContainedSpan<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) generics: Punctuated<'a, TokenReference<'a>>,
}

impl<'a> GenericDeclaration<'a> {
	pub fn arrows(&self) -> &ContainedSpan<'a> {
		&self.arrows
	}

	pub fn generics(&self) -> &Punctuated<'a, TokenReference<'a>> {
		&self.generics
	}
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeSpecifier<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) punctuation: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub(crate) type_info: TypeInfo<'a>,
}

impl<'a> TypeSpecifier<'a> {
	pub fn punctuation(&self) -> &TokenReference<'a> {
		&self.punctuation
	}

	pub fn type_info(&self) -> &TypeInfo<'a> {
		&self.type_info
	}
}
