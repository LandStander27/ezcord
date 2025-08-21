use super::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Expr(Expr),
	Decl(Decl),
	If(IfStmt),
	For(ForStmt),
	While(WhileStmt),
	VarSet(VarSetStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarSetStmt {
	pub ident: String,
	pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
	pub var_ident: String,
	pub iterator: Expr,
	pub block: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
	pub cond: Expr,
	pub block: Vec<Stmt>,
	pub else_block: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
	pub cond: Expr,
	pub block: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
	Var(VarDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
	pub ident: String,
	// pub typ: Type,
	pub init: Expr,
}
