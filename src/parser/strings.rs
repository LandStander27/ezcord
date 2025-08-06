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

use super::ParseResult;

#[derive(Clone)]
enum StringFragment<'a> {
	Literal(&'a str),
	EscapedChar(char),
	EscapedWS,
}

fn parse_unicode(input: &str) -> ParseResult<&str, char> {
	let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
	let parse_delimited_hex = preceded(char('u'), delimited(char('{'), parse_hex, char('}')));

	let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));
	return map_opt(parse_u32, std::char::from_u32).parse(input);
}

fn parse_escaped_char(input: &str) -> ParseResult<&str, char> {
	return preceded(
		char('\\'),
		alt((
			parse_unicode,
			value('\n', char('n')),
			value('\r', char('r')),
			value('\t', char('t')),
			value('\\', char('\\')),
			value('"', char('"')),
		)),
	)
	.parse(input);
}

fn parse_escaped_whitespace(input: &str) -> ParseResult<&str, &str> {
	return preceded(char('\\'), cut(context("invalid escaped char", multispace1))).parse(input);
}

fn parse_literal(input: &str) -> ParseResult<&str, &str> {
	let not_quote_slash = is_not("\"\\");
	return verify(not_quote_slash, |s: &str| !s.is_empty()).parse(input);
}

fn parse_fragment(input: &str) -> ParseResult<&str, StringFragment> {
	return alt((
		map(parse_literal, StringFragment::Literal),
		map(parse_escaped_char, StringFragment::EscapedChar),
		value(StringFragment::EscapedWS, parse_escaped_whitespace),
	))
	.parse(input);
}

pub fn parse_string(input: &str) -> ParseResult<&str, String> {
	let build = fold(0.., parse_fragment, String::new, |mut s, frag| {
		match frag {
			StringFragment::Literal(lit) => s.push_str(lit),
			StringFragment::EscapedChar(c) => s.push(c),
			StringFragment::EscapedWS => {}
		}
		return s;
	});

	return delimited(char('"'), cut(build), cut(context("mismatched quotes", char('"')))).parse(input);
}
