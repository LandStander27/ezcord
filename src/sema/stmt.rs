use super::ResolvedDecl;
use super::ResolvedExpr;

#[derive(Debug, Clone)]
pub enum ResolvedStmt {
	Expr(ResolvedExpr),
	Decl(ResolvedDecl),
	If(ResolvedIfStmt),
	While(ResolvedWhileStmt),
	For(ResolvedForStmt),
	VarSet(ResolvedVarSet),
}

#[derive(Debug, Clone)]
pub struct ResolvedVarSet {
	pub name: String,
	pub expr: ResolvedExpr,
}

#[derive(Debug, Clone)]
pub struct ResolvedIfStmt {
	pub cond: ResolvedExpr,
	pub block: Vec<ResolvedStmt>,
	pub else_block: Option<Vec<ResolvedStmt>>,
}

#[derive(Debug, Clone)]
pub struct ResolvedForStmt {
	pub var_ident: String,
	pub iterator: ResolvedExpr,
	pub block: Vec<ResolvedStmt>,
}

#[derive(Debug, Clone)]
pub struct ResolvedWhileStmt {
	pub cond: ResolvedExpr,
	pub block: Vec<ResolvedStmt>,
}
