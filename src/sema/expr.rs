use super::Type;
use ezcord_derive::DynamicEnum;

#[derive(Debug, Clone)]
pub struct LiteralString {
	pub s: String,
}

impl LiteralString {
	fn get_type(&self) -> Type {
		return Type::String;
	}
}

#[derive(Debug, Clone)]
pub struct FmtString {
	pub fragments: Vec<ResolvedExpr>,
}

impl FmtString {
	fn get_type(&self) -> Type {
		return Type::String;
	}
}

#[derive(Debug, Clone)]
pub struct LiteralNumber {
	pub number: i64,
}

impl LiteralNumber {
	fn get_type(&self) -> Type {
		return Type::Number;
	}
}

#[derive(DynamicEnum, Debug, Clone)]
#[call(pub fn get_type(&self) -> Type)]
pub enum ResolvedExpr {
	String(LiteralString),
	FmtString(FmtString),
	Number(LiteralNumber),
	Ident(ResolvedVarExpr),
	Call(ResolvedCall),
}

#[derive(Debug, Clone)]
pub struct ResolvedVarExpr {
	pub name: String,
	pub typ: Type,
}

impl ResolvedVarExpr {
	fn get_type(&self) -> Type {
		return self.typ;
	}
}

#[derive(Debug, Clone)]
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
