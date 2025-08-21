#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
	Binary(BinOperation),
	Unary(UnaryOperation),
}

impl Operation {
	pub fn prec(&self) -> i64 {
		match self {
			Operation::Binary(bin) => match bin {
				BinOperation::Index => 9,
				BinOperation::Div | BinOperation::Mul => 7,
				BinOperation::Add | BinOperation::Sub => 6,

				BinOperation::GreaterOrEqualThan | BinOperation::GreaterThan | BinOperation::LessOrEqualThan | BinOperation::LessThan => 5,
				BinOperation::NotEquals | BinOperation::Equals => 4,

				BinOperation::And => 3,
				BinOperation::Or => 2,
				BinOperation::Range | BinOperation::RangeInclusive => 1,
			},
			Operation::Unary(unary) => match unary {
				UnaryOperation::Not | UnaryOperation::Neg | UnaryOperation::UnwrapOption | UnaryOperation::OptionIsSome => 8,
			},
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperation {
	Not,
	Neg,

	UnwrapOption,
	OptionIsSome,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOperation {
	Add,
	Sub,
	Div,
	Mul,

	Equals,
	NotEquals,

	Index,

	And,
	Or,

	GreaterThan,
	LessThan,

	GreaterOrEqualThan,
	LessOrEqualThan,

	Range,
	RangeInclusive,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	String(LitOrFmtString),
	Number(f64),
	Bool(bool),
	Array(Array),
	BinOp(BinOp),
	UnaryOp(UnaryOp),
	Group(Group),
	Ident(String),
	Call(Call),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
	pub elements: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
	pub expr: Box<Expr>,
}

impl Group {
	pub fn new(expr: Box<Expr>) -> Self {
		return Self { expr };
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
	pub expr: Box<Expr>,
	pub op: Operation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
	pub left: Box<Expr>,
	pub right: Box<Expr>,
	pub op: Operation,
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
