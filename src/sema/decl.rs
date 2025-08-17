use super::ResolvedExpr;
use super::Type;
use ezcord_derive::DynamicEnum;

#[derive(DynamicEnum, Debug, Clone)]
#[call(pub fn get_name(&self) -> &str)]
pub enum ResolvedDecl {
	Var(ResolvedVarDecl),
}

#[derive(Debug, Clone)]
pub struct ResolvedArgDecl {
	pub name: String,
	pub typ: Type,
}

impl ResolvedArgDecl {
	#[allow(unused)]
	fn get_name(&self) -> &str {
		return &self.name;
	}
}

#[derive(Debug, Clone)]
pub struct ResolvedVarDecl {
	pub name: String,
	pub typ: Type,
	pub init: Option<ResolvedExpr>,
}

impl ResolvedVarDecl {
	fn get_name(&self) -> &str {
		return &self.name;
	}
}
