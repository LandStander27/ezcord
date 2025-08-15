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
	// Unknown(&'a str),
}

fn parse_ident(input: &str) -> ParseResult<&str, String> {
	let res = recognize(pair(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_")))))).parse(input)?;

	return Ok((res.0, res.1.into()));
}

fn parse_number(input: &str) -> ParseResult<&str, i64> {
	let res = recognize(preceded(opt(char('-')), many1(terminated(one_of("0123456789"), many0(char('_')))))).parse(input)?;

	let num: i64 = match res.1.parse() {
		Ok(o) => o,
		Err(_) => {
			let mut err = VerboseError::from_error_kind(res.1, nom::error::ErrorKind::Fail);
			err.errors
				.push((res.1, VerboseErrorKind::Context("invalid i64")));
			return Err(nom::Err::Error(err));
		}
	};

	return Ok((res.0, num));
}

fn parse_bool(input: &str) -> ParseResult<&str, bool> {
	return alt((value(false, tag("false")), value(true, tag("true")))).parse(input);
}

fn parse_expr(input: &str) -> ParseResult<&str, Expr> {
	return delimited(
		multispace0,
		alt((
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

fn parse_decl(input: &str) -> ParseResult<&str, Decl> {
	return delimited(multispace0, alt((map(parse_var_decl, Decl::VarDecl),)), multispace0).parse(input);
}

fn func(input: &str) -> ParseResult<&str, Call> {
	let res = recognize(pair(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_")))))).parse(input)?;

	let ident = res.1;
	let res = delimited(context("expected (", char('(')), cut(separated_list0(char(','), parse_expr)), cut(context("expected ')'", char(')')))).parse(res.0)?;

	return Ok((res.0, Call { name: ident.into(), args: res.1 }));
}

fn parse_var_set(input: &str) -> ParseResult<&str, VarSetStmt> {
	let res = parse_ident(input)?;

	let ident = res.1;
	let input = delimited(multispace0, tag("="), multispace0)
		.parse(res.0)?
		.0;
	let res = cut(context("invalid expression", parse_expr)).parse(input)?;
	let expr = res.1;

	return Ok((res.0, VarSetStmt { ident, expr }));
}

fn parse_var_decl(input: &str) -> ParseResult<&str, VarDecl> {
	let res = parse_ident(input)?;

	let ident = res.1;
	let input = delimited(multispace0, tag(":="), multispace0)
		.parse(res.0)?
		.0;
	let res = cut(context("invalid expression", parse_expr)).parse(input)?;
	let init = res.1;

	return Ok((res.0, VarDecl { ident, init }));
}

fn parse_if_stmt(input: &str) -> ParseResult<&str, IfStmt> {
	let input = terminated(tag("if"), multispace1).parse(input)?.0;

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
	return Ok((res.0, IfStmt { cond, block: res.1 }));
}

fn parse_while_stmt(input: &str) -> ParseResult<&str, WhileStmt> {
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

fn parse_comment(input: &str) -> ParseResult<&str, ()> {
	let rest = preceded(char('#'), take_till(|x| x == '\n'))
		.parse(input)?
		.0;
	return Ok((rest, ()));
}

fn parse_stmt(input: &str) -> ParseResult<&str, Token> {
	return delimited(
		multispace0,
		alt((
			value(Token::Comment, parse_comment),
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

fn parse_multi_stmt(input: &str) -> ParseResult<&str, Vec<Stmt>> {
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
