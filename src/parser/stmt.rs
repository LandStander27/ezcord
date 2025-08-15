use super::Expr;
// use crate::sema::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Expr(Expr),
	Decl(Decl),
	If(IfStmt),
	VarSet(VarSetStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarSetStmt {
	pub ident: String,
	pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
	pub cond: Expr,
	pub block: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
	VarDecl(VarDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
	pub ident: String,
	// pub typ: Type,
	pub init: Expr,
}
