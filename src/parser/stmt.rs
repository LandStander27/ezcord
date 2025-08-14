use super::Expr;
// use crate::sema::types::Type;

#[derive(Debug)]
pub enum Stmt {
	Expr(Expr),
	Decl(Decl),
}

#[derive(Debug)]
pub enum Decl {
	VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct VarDecl {
	pub ident: String,
	// pub typ: Type,
	pub init: Expr,
}
