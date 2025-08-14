// use super::strings;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	String(LitOrFmtString),
	Number(i64),
	Ident(String),
	Call(Call),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
	pub name: String,
	pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitOrFmtString {
	Lit(String),
	Fmt(Vec<Expr>),
}
