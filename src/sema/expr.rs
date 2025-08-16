use super::Type;
use crate::parser::expr::*;
use ezcord_derive::DynamicEnum;

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralString {
	pub s: String,
}

impl LiteralString {
	fn get_type(&self) -> Type {
		return Type::String;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FmtString {
	pub fragments: Vec<ResolvedExpr>,
}

impl FmtString {
	fn get_type(&self) -> Type {
		return Type::String;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralNumber {
	pub number: i64,
}

impl LiteralNumber {
	fn get_type(&self) -> Type {
		return Type::Number;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralBool {
	pub value: bool,
}

impl LiteralBool {
	fn get_type(&self) -> Type {
		return Type::Bool;
	}
}

#[derive(DynamicEnum, Debug, Clone, PartialEq)]
#[call(pub fn get_type(&self) -> Type)]
pub enum ResolvedExpr {
	String(LiteralString),
	FmtString(FmtString),
	Number(LiteralNumber),
	Bool(LiteralBool),
	Ident(ResolvedVarExpr),
	Group(ResolvedGroup),
	UnaryOp(ResolvedUnaryOp),
	BinaryOp(ResolvedBinaryOp),
	Call(ResolvedCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedGroup {
	pub expr: Box<ResolvedExpr>,
}

impl ResolvedGroup {
	fn get_type(&self) -> Type {
		return self.expr.get_type();
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedUnaryOp {
	pub expr: Box<ResolvedExpr>,
	pub op: Operation,
}

impl ResolvedUnaryOp {
	fn get_type(&self) -> Type {
		return self.expr.get_type();
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedBinaryOp {
	pub left: Box<ResolvedExpr>,
	pub right: Box<ResolvedExpr>,
	pub op: Operation,
}

impl ResolvedBinaryOp {
	fn get_type(&self) -> Type {
		return match self.op {
			Operation::Binary(ref binary) => match binary {
				BinOperation::Equals | BinOperation::NotEquals => Type::Bool,
				_ => self.left.get_type(),
			},
			Operation::Unary(_) => unreachable!(),
		};
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedVarExpr {
	pub name: String,
	pub typ: Type,
}

impl ResolvedVarExpr {
	fn get_type(&self) -> Type {
		return self.typ;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedCall {
	pub name: String,
	pub args: Vec<ResolvedExpr>,
	pub ret_type: Type,
}

impl ResolvedCall {
	fn get_type(&self) -> Type {
		return self.ret_type;
	}
}
