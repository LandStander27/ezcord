use nom::{
	Parser,
	branch::alt,
	bytes::complete::{is_not, take_while_m_n},
	character::complete::{char, multispace1},
	combinator::{cut, map, map_opt, map_res, value, verify},
	error::context,
	multi::fold,
	sequence::{delimited, preceded},
};

use super::*;

#[derive(Clone)]
enum StringFragment<'a> {
	Literal(&'a str),
	FmtFrag(Expr),
	EscapedChar(char),
	EscapedWS,
}

fn parse_unicode(input: &str) -> ParseResult<'_, &str, char> {
	let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
	let parse_delimited_hex = preceded(char('u'), delimited(char('{'), parse_hex, char('}')));

	let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));
	return map_opt(parse_u32, std::char::from_u32).parse(input);
}

fn parse_escaped_char(input: &str) -> ParseResult<'_, &str, char> {
	return preceded(
		char('\\'),
		alt((
			parse_unicode,
			value('\n', char('n')),
			value('\r', char('r')),
			value('\t', char('t')),
			value('\\', char('\\')),
			value('{', char('{')),
			value('"', char('"')),
		)),
	)
	.parse(input);
}

fn parse_escaped_whitespace(input: &str) -> ParseResult<'_, &str, &str> {
	return preceded(char('\\'), cut(context("invalid escaped char", multispace1))).parse(input);
}

fn parse_fmt(input: &str) -> ParseResult<'_, &str, Expr> {
	return delimited(char('{'), super::parse_expr, char('}')).parse(input);
}

fn parse_literal(input: &str) -> ParseResult<'_, &str, &str> {
	let not_quote_slash = is_not("\"\\{");
	return verify(not_quote_slash, |s: &str| !s.is_empty()).parse(input);
}

fn parse_fragment(input: &str) -> ParseResult<'_, &str, StringFragment<'_>> {
	return alt((
		map(parse_literal, StringFragment::Literal),
		map(parse_escaped_char, StringFragment::EscapedChar),
		map(parse_fmt, StringFragment::FmtFrag),
		value(StringFragment::EscapedWS, parse_escaped_whitespace),
	))
	.parse(input);
}

pub fn parse_string(input: &str) -> ParseResult<'_, &str, LitOrFmtString> {
	let build = fold(0.., parse_fragment, Vec::new, |mut s, frag| {
		s.push(frag);
		// match frag {
		// 	StringFragment::Literal(lit) => s.push(lit),
		// 	StringFragment::EscapedChar(c) => s.push(&c.to_string()),
		// 	StringFragment::EscapedWS => {}
		// }
		return s;
	});

	let res = delimited(char('"'), cut(build), cut(context("mismatched quotes", char('"')))).parse(input)?;

	if res
		.1
		.iter()
		.any(|x| matches!(x, StringFragment::FmtFrag(_)))
	{
		let mut v = Vec::new();
		let mut current_str = String::new();
		for frag in res.1.into_iter() {
			match frag {
				StringFragment::EscapedWS => {}
				StringFragment::Literal(lit) => current_str.push_str(lit),
				StringFragment::EscapedChar(c) => current_str.push(c),
				StringFragment::FmtFrag(fmt) => {
					v.push(Expr::String(LitOrFmtString::Lit(std::mem::take(&mut current_str))));
					current_str.clear();
					v.push(fmt);
				}
			}
		}

		if !current_str.is_empty() {
			v.push(Expr::String(LitOrFmtString::Lit(current_str)));
		}

		return Ok((res.0, LitOrFmtString::Fmt(v)));
	} else {
		return Ok((
			res.0,
			LitOrFmtString::Lit(
				res.1
					.iter()
					.filter_map(|x| match x {
						StringFragment::Literal(lit) => Some(lit.to_string()),
						StringFragment::EscapedChar(c) => Some(c.to_string()),
						StringFragment::EscapedWS => None,
						StringFragment::FmtFrag(_) => unreachable!(),
					})
					.collect(),
			),
		));
	}
}
