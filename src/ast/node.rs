use super::types::{TypeDeclaration, TypeInfo};
use bumpalo::collections::Vec;
use lasso::Spur;
use logos::Span;

#[derive(Debug)]
pub struct NodeLine<N> {
	pub node: N,
	pub line: usize,
}

#[derive(Debug)]
pub enum Index<'a> {
	Brackets(Expression<'a>),
	Dot(NodeLine<Spur>),
}

#[derive(Debug)]
pub enum Field<'a> {
	ExpressionKey {
		key: Expression<'a>,
		value: Expression<'a>,
	},
	NameKey {
		key: Spur,
		value: Expression<'a>,
	},
	NoKey {
		value: Expression<'a>,
	},
}

#[derive(Debug)]
pub struct FunctionCall<'a> {
	pub call: Suffixed<'a>,
}

#[derive(Debug)]
pub struct TableConstructor<'a> {
	pub fields: Vec<'a, Field<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum LSymbol {
	Nil,
	True,
	False,
	Ellipse,
}

#[derive(Debug)]
pub enum LNumber {
	Float(f64),
	Integer(i64),
}

#[derive(Debug)]
pub enum Value<'a> {
	Function(FunctionBody<'a>),
	FunctionCall(FunctionCall<'a>),
	Number(LNumber),
	Parentheses(Expression<'a>),
	String(Spur),
	Symbol(NodeLine<LSymbol>),
	TableConstructor(TableConstructor<'a>),
	Var(Var<'a>),
}

#[derive(Debug)]
pub enum Expression<'a> {
	UnaryOp {
		rhs: &'a Expression<'a>,
		un_op: NodeLine<UnOp>,
	},
	BinaryOp {
		lhs: &'a Expression<'a>,
		rhs: &'a Expression<'a>,
		bin_op: NodeLine<BinOp>,
	},
	Value {
		value: &'a Value<'a>,
	},
}

#[derive(Debug)]
pub struct VarName<'a> {
	pub name: Spur,
	pub type_info: Option<TypeInfo<'a>>,
}

#[derive(Debug)]
pub enum FunctionArgs<'a> {
	Parentheses(Vec<'a, Expression<'a>>),
	String(Spur),
	TableConstructor(TableConstructor<'a>),
}

#[derive(Debug)]
pub struct MethodCall<'a> {
	pub name: NodeLine<Spur>,
	pub args: FunctionArgs<'a>,
}

#[derive(Debug)]
pub enum Prefix<'a> {
	Expression(Expression<'a>),
	Name(NodeLine<Spur>),
}

#[derive(Debug)]
pub enum Call<'a> {
	AnonymousCall(FunctionArgs<'a>),
	MethodCall(MethodCall<'a>),
}

#[derive(Debug)]
pub struct Var<'a> {
	pub var: Suffixed<'a>,
}

#[derive(Debug)]
pub struct FunctionBody<'a> {
	pub parameters: Vec<'a, Parameter<'a>>,
	pub return_type: Option<TypeInfo<'a>>,
	pub block: Block<'a>,
}

#[derive(Debug)]
pub enum Parameter<'a> {
	Ellipse,
	Name(VarName<'a>),
}

#[derive(Debug)]
pub enum Suffix<'a> {
	Call(Call<'a>),
	Index(Index<'a>),
}

#[derive(Debug)]
pub struct Suffixed<'a> {
	pub prefix: Prefix<'a>,
	pub suffixes: Vec<'a, Suffix<'a>>,
}

#[derive(Debug)]
pub struct IfStat<'a> {
	pub condition: Expression<'a>,
	pub block: Block<'a>,
}

#[derive(Debug)]
pub struct Assignment<'a> {
	pub var_list: Vec<'a, Var<'a>>,
	pub expr_list: Vec<'a, Expression<'a>>,
}

#[derive(Debug)]
pub struct CompAssignment<'a> {
	pub var: Var<'a>,
	pub expr: Expression<'a>,
	pub op: CompOp,
}

#[derive(Debug)]
pub struct Do<'a> {
	pub block: Block<'a>,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
	pub name: Vec<'a, NodeLine<Spur>>,
	pub body: FunctionBody<'a>,
}

#[derive(Debug)]
pub struct GenericFor<'a> {
	pub name_list: Vec<'a, VarName<'a>>,
	pub expr_list: Vec<'a, Expression<'a>>,
	pub block: Block<'a>,
}

#[derive(Debug)]
pub struct If<'a> {
	pub stat_list: Vec<'a, IfStat<'a>>,
	pub else_opt: Option<Block<'a>>,
}

#[derive(Debug)]
pub struct LocalAssignment<'a> {
	pub name_list: Vec<'a, VarName<'a>>,
	pub expr_list: Vec<'a, Expression<'a>>,
}

#[derive(Debug)]
pub struct LocalFunction<'a> {
	pub name: Spur,
	pub body: FunctionBody<'a>,
}

#[derive(Debug)]
pub struct NumericFor<'a> {
	pub var: VarName<'a>,
	pub start: Expression<'a>,
	pub end: Expression<'a>,
	pub step: Option<Expression<'a>>,
	pub block: Block<'a>,
}

#[derive(Debug)]
pub struct Repeat<'a> {
	pub block: Block<'a>,
	pub until: Expression<'a>,
}

#[derive(Debug)]
pub struct While<'a> {
	pub condition: Expression<'a>,
	pub block: Block<'a>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
	Assignment(Assignment<'a>),
	CompAssignment(CompAssignment<'a>),
	Do(Do<'a>),
	FunctionCall(FunctionCall<'a>),
	FunctionDeclaration(&'a FunctionDeclaration<'a>),
	GenericFor(&'a GenericFor<'a>),
	If(If<'a>),
	LocalAssignment(LocalAssignment<'a>),
	LocalFunction(&'a LocalFunction<'a>),
	NumericFor(&'a NumericFor<'a>),
	Repeat(Repeat<'a>),
	TypeDeclaration(TypeDeclaration<'a>),
	While(While<'a>),
}

#[derive(Debug)]
pub enum LastStmt<'a> {
	Break(Span),
	Continue(Span),
	Return(Vec<'a, Expression<'a>>),
}

#[derive(Debug)]
pub struct Block<'a> {
	pub stmts: Vec<'a, Stmt<'a>>,
	pub last_stmt: Option<LastStmt<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
	And,
	Caret,
	GreaterThan,
	GreaterThanEqual,
	LessThan,
	LessThanEqual,
	Minus,
	Or,
	Percent,
	Plus,
	Slash,
	Star,
	TildeEqual,
	TwoDots,
	TwoEquals,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
	Hash,
	Minus,
	Not,
}

#[derive(Clone, Copy, Debug)]
pub enum CompOp {
	CaretEqual,
	MinusEqual,
	PercentEqual,
	PlusEqual,
	SlashEqual,
	StarEqual,
}
