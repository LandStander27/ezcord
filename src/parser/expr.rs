// use super::strings;

#[derive(Debug)]
pub enum Expr {
	String(String),
	FmtString(FmtString),
	Number(i64),
	Ident(String),
	Call(Call),
}

#[derive(Debug)]
pub struct Call {
	pub name: String,
	pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct FmtString {
	pub segments: Vec<Expr>,
}
