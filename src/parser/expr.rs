// use super::strings;

#[derive(Debug)]
pub enum Expr {
	String(String),
	Number(i64),
	Ident(String),
	Call(Call),
}

#[derive(Debug)]
pub struct Call {
	pub name: String,
	pub args: Vec<Expr>,
}
