use nom::{
	IResult, Parser,
	branch::alt,
	bytes::complete::{tag, take_while1},
	character::complete::{alpha1, alphanumeric1, char, multispace0, one_of},
	combinator::{cut, map, opt, recognize},
	error::{ParseError, context},
	multi::{many0, many1, separated_list0},
	sequence::{delimited, pair, preceded, terminated},
};
use nom_language::error::{VerboseError, VerboseErrorKind};

use anyhow::anyhow;
use tracing::{error, info};

mod error;
mod strings;
mod tests;

pub mod expr;
use expr::*;

pub mod stmt;
use stmt::*;

type ParseResult<'a, I, O> = IResult<I, O, VerboseError<&'a str>>;

#[derive(Debug)]
enum Token<'a> {
	Stmt(Stmt),
	Unknown(&'a str),
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

fn parse_expr(input: &str) -> ParseResult<&str, Expr> {
	return delimited(
		multispace0,
		alt((
			map(strings::parse_string, Expr::String),
			map(parse_number, Expr::Number),
			map(func, Expr::Call),
			map(parse_ident, Expr::Ident),
		)),
		multispace0,
	)
	.parse(input);
}

fn func(input: &str) -> ParseResult<&str, Call> {
	let res = recognize(pair(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_")))))).parse(input)?;

	let ident = res.1;
	let res = delimited(context("expected (", char('(')), cut(separated_list0(char(','), parse_expr)), cut(context("expected ')'", char(')')))).parse(res.0)?;

	return Ok((res.0, Call { name: ident.into(), args: res.1 }));
}

fn parse_stmt(input: &str) -> ParseResult<&str, Token> {
	return preceded(
		multispace0,
		alt((
			// map(strings::parse_string, Token::Sequence),
			map(map(parse_expr, Stmt::Expr), Token::Stmt),
			// map(key, Token::Key),
			map(take_while1(|c: char| !c.is_whitespace()), Token::Unknown),
		)),
	)
	.parse(input);
}

pub(crate) fn parse(input: String) -> anyhow::Result<Vec<Stmt>> {
	if input.chars().all(|c| c.is_whitespace()) {
		return Ok(Vec::new());
	}

	let start = std::time::Instant::now();

	let mut rest: &str = input.as_str();
	let mut actions: Vec<Stmt> = Vec::new();

	loop {
		let res = parse_stmt(rest).map_err(|e| {
			match e {
				nom::Err::Error(ref e) | nom::Err::Failure(ref e) => {
					let s = error::convert_error(&input, e.clone());
					error!(s);
					return anyhow!("{s}");
				}
				_ => {}
			}

			error!(?e);
			return anyhow!("{e}").context("parse error");
		})?;

		rest = res.0;

		#[cfg(debug_assertions)]
		dbg!(&res.1);

		match res.1 {
			Token::Unknown(token) => {
				error!("unknown token: {token}");
				return Err(anyhow!("unknown token: {token}"));
			}
			Token::Stmt(stmt) => {
				actions.push(stmt);
			}
		}

		if rest.is_empty() || rest.chars().all(|c| c.is_whitespace()) {
			break;
		}
	}

	info!("parsing done; took {}ms", start.elapsed().as_millis());
	return Ok(actions);
}
