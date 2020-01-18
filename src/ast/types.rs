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
	pub key: TypeFieldKey<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub colon: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub value: TypeInfo<'a>,
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
	pub as_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub coerce_to: TypeInfo<'a>,
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeDeclaration<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub type_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub base: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub generics: Option<GenericDeclaration<'a>>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub equal_token: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub declare_as: TypeInfo<'a>,
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericDeclaration<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub arrows: ContainedSpan<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub generics: Punctuated<'a, TokenReference<'a>>,
}

#[derive(Clone, Debug, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeSpecifier<'a> {
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub punctuation: TokenReference<'a>,
	#[cfg_attr(feature = "serde", serde(borrow))]
	pub type_info: TypeInfo<'a>,
}
