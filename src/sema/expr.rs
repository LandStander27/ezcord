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
	pub number: f64,
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
	Array(ResolvedArray),
	Option(ResolvedOption),
	Ident(ResolvedVarExpr),
	Group(ResolvedGroup),
	UnaryOp(ResolvedUnaryOp),
	BinaryOp(ResolvedBinaryOp),
	Call(ResolvedCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedOption {
	pub value: Option<Box<ResolvedExpr>>,
	pub inner_type: Type,
}

impl ResolvedOption {
	fn get_type(&self) -> Type {
		return Type::Optional(Box::new(self.inner_type.clone()));
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedArray {
	pub elements: Vec<ResolvedExpr>,
}

impl ResolvedArray {
	fn get_type(&self) -> Type {
		if let Some(first) = self.elements.first() {
			return Type::Array(Box::new(first.get_type()));
		}

		return Type::Array(Box::new(Type::Void));
	}
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
		return match self.op {
			Operation::Unary(ref unary) => match unary {
				UnaryOperation::UnwrapOption => {
					if let Type::Optional(inner_type) = self.expr.get_type() {
						return *inner_type;
					}
					unreachable!();
				}
				UnaryOperation::OptionIsSome => Type::Bool,
				_ => self.expr.get_type(),
			},
			Operation::Binary(_) => unreachable!(),
		};
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
				BinOperation::Equals
				| BinOperation::NotEquals
				| BinOperation::And
				| BinOperation::Or
				| BinOperation::GreaterOrEqualThan
				| BinOperation::GreaterThan
				| BinOperation::LessOrEqualThan
				| BinOperation::LessThan => Type::Bool,
				BinOperation::Index => {
					if let Type::Array(element_type) = self.left.get_type() {
						return *element_type;
					}
					unreachable!()
				}
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
		return self.typ.clone();
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
		return self.ret_type.clone();
	}
}
