use nom::{
	IResult, Parser,
	branch::alt,
	bytes::complete::{tag, take_till},
	character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, one_of},
	combinator::{cut, map, opt, recognize, value},
	error::{ParseError, context},
	multi::{many0, many1, separated_list0},
	sequence::{delimited, pair, preceded, terminated},
};
use nom_language::error::{VerboseError, VerboseErrorKind};

use anyhow::anyhow;

mod error;
mod strings;
mod tests;

pub mod expr;
use expr::*;

pub mod stmt;
use stmt::*;

type ParseResult<'a, I, O> = IResult<I, O, VerboseError<&'a str>>;

#[derive(Debug, Clone, PartialEq)]
enum Token {
	Stmt(Stmt),
	RBracket,
	Comment,
}

fn parse_ident(input: &str) -> ParseResult<'_, &str, String> {
	let res = recognize(pair(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_")))))).parse(input)?;

	return Ok((res.0, res.1.into()));
}

fn parse_number(input: &str) -> ParseResult<'_, &str, f64> {
	let res = recognize(preceded(opt(char('-')), many1(terminated(one_of("0123456789."), many0(char('_')))))).parse(input)?;
	let num: f64 = match res.1.parse() {
		Ok(o) => o,
		Err(_) => {
			let mut err = VerboseError::from_error_kind(res.1, nom::error::ErrorKind::Fail);
			err.errors
				.push((res.1, VerboseErrorKind::Context("expected <= 1 decimals in number")));
			return Err(nom::Err::Error(err));
		}
	};

	return Ok((res.0, num));
}

fn parse_bool(input: &str) -> ParseResult<'_, &str, bool> {
	return alt((value(false, tag("false")), value(true, tag("true")))).parse(input);
}

fn parse_group(input: &str) -> ParseResult<'_, &str, Group> {
	return delimited(
		terminated(char('('), multispace0),
		map(map(parse_expr, Box::new), Group::new),
		terminated(char(')'), multispace0),
	)
	.parse(input);
}

fn parse_array(input: &str) -> ParseResult<'_, &str, Array> {
	let res = delimited(
		terminated(char('['), multispace0),
		cut(context("invalid array", separated_list0(delimited(multispace0, char(','), multispace0), parse_expr))),
		delimited(multispace0, cut(context("expected ']'", char(']'))), multispace0),
	)
	.parse(input)?;

	return Ok((res.0, Array { elements: res.1 }));
}

fn parse_expr(input: &str) -> ParseResult<'_, &str, Expr> {
	let left = parse_expr_single(input)?;
	return parse_expr_rhs(left.0, left.1, 0);
}

fn parse_postfix(input: &str) -> ParseResult<'_, &str, Expr> {
	return delimited(
		multispace0,
		alt((
			map(parse_array, Expr::Array),
			map(parse_group, Expr::Group),
			map(strings::parse_string, Expr::String),
			map(parse_number, Expr::Number),
			map(parse_bool, Expr::Bool),
			map(func, Expr::Call),
			map(parse_ident, Expr::Ident),
		)),
		multispace0,
	)
	.parse(input);
}

fn parse_expr_single(input: &str) -> ParseResult<'_, &str, Expr> {
	if let Ok((input, unary)) = parse_unary_op(input) {
		let (rest, expr) = parse_postfix(input)?;
		return Ok((rest, Expr::UnaryOp(UnaryOp { expr: Box::new(expr), op: unary })));
	}

	return parse_postfix(input);
}

fn parse_unary_op(input: &str) -> ParseResult<'_, &str, Operation> {
	return context(
		"invalid unary op",
		delimited(
			multispace0,
			map(
				alt((
					value(UnaryOperation::Neg, char('-')),
					value(UnaryOperation::Not, char('!')),
					value(UnaryOperation::UnwrapOption, char('*')),
					value(UnaryOperation::OptionIsSome, char('?')),
				)),
				Operation::Unary,
			),
			multispace0,
		),
	)
	.parse(input);
}

fn parse_binary_op(input: &str) -> ParseResult<'_, &str, Operation> {
	return context(
		"invalid binop",
		delimited(
			multispace0,
			map(
				alt((
					value(BinOperation::Add, char('+')),
					value(BinOperation::Sub, char('-')),
					value(BinOperation::Div, char('/')),
					value(BinOperation::Mul, char('*')),
					value(BinOperation::Equals, tag("==")),
					value(BinOperation::NotEquals, tag("!=")),
					value(BinOperation::And, tag("&&")),
					value(BinOperation::Or, tag("||")),
					value(BinOperation::GreaterThan, char('>')),
					value(BinOperation::LessThan, char('<')),
					value(BinOperation::GreaterOrEqualThan, tag(">=")),
					value(BinOperation::LessOrEqualThan, tag("<=")),
					value(BinOperation::Range, tag("..")),
					value(BinOperation::RangeInclusive, tag("..=")),
					value(BinOperation::Index, char('[')),
				)),
				Operation::Binary,
			),
			multispace0,
		),
	)
	.parse(input);
}

fn parse_expr_rhs(mut input: &str, mut lhs: Expr, prec: i64) -> ParseResult<'_, &str, Expr> {
	loop {
		let (rest, op) = match parse_binary_op(input) {
			Ok(o) => o,
			Err(_e) => return Ok((input, lhs)),
		};

		let cur_op = op.prec();
		if cur_op < prec {
			return Ok((input, lhs));
		}
		input = rest;

		let (rest, mut right) = if op == Operation::Binary(BinOperation::Index) {
			delimited(multispace0, terminated(parse_expr, multispace0), terminated(char(']'), multispace0)).parse(input)?
		} else {
			parse_expr_single(input)?
		};
		input = rest;

		match parse_binary_op(input) {
			Ok(o) => {
				if cur_op < o.1.prec() {
					let (rest, rhs_expr) = parse_expr_rhs(input, right, cur_op + 1)?;
					right = rhs_expr;
					input = rest;
				}
			}
			Err(_e) => {}
		};

		lhs = Expr::BinOp(BinOp {
			left: Box::new(lhs),
			right: Box::new(right),
			op,
		});
	}
}

// fn parse_type(input: &str) -> ParseResult<&str, Type> {
// 	return alt((
// 		value(Type::Bool, tag("bool")),
// 		value(Type::String, tag("string")),
// 		value(Type::Number, tag("number")),
// 		value(Type::Void, tag("void")),
// 		map(
// 			map(preceded(tag("[]"), cut(context("expected element type for array after '[]'", parse_type))), Box::new),
// 			Type::Array,
// 		),
// 	))
// 	.parse(input);
// }

fn parse_decl(input: &str) -> ParseResult<'_, &str, Decl> {
	return delimited(multispace0, alt((map(parse_var_decl, Decl::Var),)), multispace0).parse(input);
}

fn func(input: &str) -> ParseResult<'_, &str, Call> {
	let res = terminated(parse_ident, multispace0).parse(input)?;
	let ident = res.1;
	let res = delimited(
		context("expected (", char('(')),
		cut(separated_list0(char(','), parse_expr)),
		cut(context("expected ')'", char(')'))),
	)
	.parse(res.0)?;

	return Ok((res.0, Call { name: ident, args: res.1 }));
}

fn parse_var_set(input: &str) -> ParseResult<'_, &str, VarSetStmt> {
	let res = parse_ident(input)?;

	let ident = res.1;
	let input = delimited(multispace0, tag("="), multispace0)
		.parse(res.0)?
		.0;
	let res = cut(context("invalid expression", parse_expr)).parse(input)?;
	let expr = res.1;

	return Ok((res.0, VarSetStmt { ident, expr }));
}

fn parse_var_decl(input: &str) -> ParseResult<'_, &str, VarDecl> {
	let res = parse_ident(input)?;

	let ident = res.1;
	let input = delimited(multispace0, tag(":="), multispace0)
		.parse(res.0)?
		.0;
	let res = cut(context("invalid expression", parse_expr)).parse(input)?;
	let init = res.1;

	return Ok((res.0, VarDecl { ident, init }));
}

fn parse_if_stmt(input: &str) -> ParseResult<'_, &str, IfStmt> {
	let input = terminated(tag("if"), multispace1).parse(input)?.0;

	let res = cut(context("invalid condition", parse_expr)).parse(input)?;
	let cond = res.1;

	let mut res = cut(context(
		"invalid block",
		delimited(
			delimited(multispace0, char('{'), multispace0),
			context("invalid statement", parse_multi_stmt),
			delimited(multispace0, char('}'), multispace0),
		),
	))
	.parse(res.0)?;

	let block = res.1;
	let mut else_block = None;
	if let Ok((rest, _)) = delimited(multispace0::<&str, VerboseError<&str>>, tag("else"), multispace0).parse(res.0) {
		res = cut(context(
			"invalid block",
			delimited(
				delimited(multispace0, char('{'), multispace0),
				context("invalid statement", parse_multi_stmt),
				delimited(multispace0, char('}'), multispace0),
			),
		))
		.parse(rest)?;

		else_block = Some(res.1);
	}

	return Ok((res.0, IfStmt { cond, block, else_block }));
}

fn parse_while_stmt(input: &str) -> ParseResult<'_, &str, WhileStmt> {
	let input = terminated(tag("while"), multispace1).parse(input)?.0;

	let res = cut(context("invalid condition", parse_expr)).parse(input)?;
	let cond = res.1;

	let res = cut(context(
		"invalid block",
		delimited(
			delimited(multispace0, char('{'), multispace0),
			context("invalid statement", parse_multi_stmt),
			delimited(multispace0, char('}'), multispace0),
		),
	))
	.parse(res.0)?;
	return Ok((res.0, WhileStmt { cond, block: res.1 }));
}

fn parse_comment(input: &str) -> ParseResult<'_, &str, ()> {
	let rest = preceded(char('#'), take_till(|x| x == '\n'))
		.parse(input)?
		.0;
	return Ok((rest, ()));
}

fn parse_for_stmt(input: &str) -> ParseResult<'_, &str, ForStmt> {
	let (input, _) = terminated(tag("for"), multispace1).parse(input)?;
	let (input, var_ident) = cut(context("expected identifier", parse_ident)).parse(input)?;
	let (input, _) = cut(context("expected 'in'", delimited(multispace1, tag("in"), multispace1))).parse(input)?;
	let (input, iterator) = cut(parse_expr).parse(input)?;
	let (input, block) = cut(context(
		"invalid block",
		delimited(
			delimited(multispace0, char('{'), multispace0),
			context("invalid statement", parse_multi_stmt),
			delimited(multispace0, char('}'), multispace0),
		),
	))
	.parse(input)?;

	return Ok((input, ForStmt { block, var_ident, iterator }));
}

fn parse_stmt(input: &str) -> ParseResult<'_, &str, Token> {
	return delimited(
		multispace0,
		alt((
			value(Token::Comment, parse_comment),
			map(map(parse_for_stmt, Stmt::For), Token::Stmt),
			map(map(parse_if_stmt, Stmt::If), Token::Stmt),
			map(map(parse_while_stmt, Stmt::While), Token::Stmt),
			map(map(parse_decl, Stmt::Decl), Token::Stmt),
			map(map(parse_var_set, Stmt::VarSet), Token::Stmt),
			map(map(parse_expr, Stmt::Expr), Token::Stmt),
			value(Token::RBracket, char('}')),
		)),
		multispace0,
	)
	.parse(input);
}

fn parse_multi_stmt(input: &str) -> ParseResult<'_, &str, Vec<Stmt>> {
	let mut rest: &str = input;
	let mut actions: Vec<Stmt> = Vec::new();

	loop {
		let res = parse_stmt(rest)?;
		if res.1 == Token::RBracket {
			break;
		}
		rest = res.0;

		#[cfg(feature = "parse_debug")]
		dbg!(&res.1);

		match res.1 {
			Token::Stmt(stmt) => actions.push(stmt),
			Token::Comment => {}
			_ => unreachable!(),
		}

		if rest.is_empty() || rest.chars().all(|c| c.is_whitespace()) {
			break;
		}
	}

	return Ok((rest, actions));
}

pub(crate) fn parse(input: String) -> anyhow::Result<Vec<Stmt>> {
	if input.chars().all(|c| c.is_whitespace()) {
		return Ok(Vec::new());
	}

	let res = parse_multi_stmt(input.as_str()).map_err(|e| {
		match e {
			nom::Err::Error(ref e) | nom::Err::Failure(ref e) => {
				let s = error::convert_error(&input, e.clone());
				return anyhow!("{s}");
			}
			_ => {}
		}

		// error!(?e);
		return anyhow!("{e}").context("parse error");
	})?;

	return Ok(res.1);
}
