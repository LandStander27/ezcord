#[cfg(test)]
use super::*;

#[test]
fn test_string_parsing() {
	let data = "\"tab:\\tafter tab, newline:\\nnew line, quote: \\\", emoji: \\u{1F602}, newline:\\nescaped whitespace: \\    abc\"";
	let result = strings::parse_string(data);
	assert_eq!(
		result,
		Ok((
			"",
			LitOrFmtString::Lit("tab:\tafter tab, newline:\nnew line, quote: \", emoji: 😂, newline:\nescaped whitespace: abc".into())
		))
	);

	assert!(strings::parse_string("\"this is another test\"").is_ok());
	assert!(strings::parse_string("\"this is another test").is_err());
	assert!(strings::parse_string("this is another test\"").is_err());
	assert!(strings::parse_string("\"this \\h is another test\"").is_err());
}

#[test]
fn test_func_parsing() {
	assert!(func("delay(1, 2)").is_ok());
	assert!(parse("delay(1, 2)".into()).is_err());
	assert!(func("delay(1)").is_ok());
	assert!(func("exit()").is_ok());
	assert!(parse("not_a_func(\"haha\")".into()).is_err());
}
