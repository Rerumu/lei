use super::node::Expression;
use bumpalo::collections::Vec;
use lasso::Spur;

#[derive(Debug)]
pub enum TypeInfo<'a> {
	Basic(Spur),
	Callback {
		arguments: Vec<'a, TypeInfo<'a>>,
		return_type: &'a TypeInfo<'a>,
	},
	Generic {
		base: Spur,
		generics: Vec<'a, TypeInfo<'a>>,
	},
	Optional {
		base: &'a TypeInfo<'a>,
	},
	Table {
		fields: Vec<'a, TypeField<'a>>,
	},
	Tuple {
		types: Vec<'a, TypeInfo<'a>>,
	},
	Typeof {
		inner: &'a Expression<'a>,
	},
	Union {
		types: Vec<'a, TypeInfo<'a>>,
	},
}

#[derive(Debug)]
pub enum TypeField<'a> {
	ExpressionKey {
		key: TypeInfo<'a>,
		value: TypeInfo<'a>,
	},
	NameKey {
		key: Spur,
		value: TypeInfo<'a>,
	},
}

#[derive(Debug)]
pub struct TypeDeclaration<'a> {
	pub base: Spur,
	pub generics: Vec<'a, Spur>,
	pub declare_as: TypeInfo<'a>,
}
