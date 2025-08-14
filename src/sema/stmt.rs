use super::ResolvedDecl;
use super::ResolvedExpr;

#[derive(Debug, Clone)]
pub enum ResolvedStmt {
	Expr(ResolvedExpr),
	Decl(ResolvedDecl),
}
