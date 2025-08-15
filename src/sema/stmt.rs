use super::ResolvedDecl;
use super::ResolvedExpr;

#[derive(Debug, Clone)]
pub enum ResolvedStmt {
	Expr(ResolvedExpr),
	Decl(ResolvedDecl),
	If(ResolvedIfStmt),
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
}
