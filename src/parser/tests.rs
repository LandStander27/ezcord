#[cfg(test)]
use super::*;

#[test]
fn test_unary_op_parsing() {
	assert_eq!(
		parse_expr("!2"),
		Ok((
			"",
			Expr::UnaryOp(UnaryOp {
				expr: Box::new(Expr::Number(2.0)),
				op: Operation::Unary(UnaryOperation::Not),
			})
		))
	);

	assert_eq!(
		parse_expr("-exit()"),
		Ok((
			"",
			Expr::UnaryOp(UnaryOp {
				expr: Box::new(Expr::Call(Call {
					name: "exit".into(),
					args: vec![]
				})),
				op: Operation::Unary(UnaryOperation::Neg),
			})
		))
	);
}

#[test]
fn test_binary_op_parsing() {
	assert_eq!(
		parse_expr("2 + 6 + 7 / 8"),
		Ok((
			"",
			Expr::BinOp(BinOp {
				left: Box::new(Expr::BinOp(BinOp {
					left: Box::new(Expr::Number(2.0)),
					op: Operation::Binary(BinOperation::Add),
					right: Box::new(Expr::Number(6.0))
				})),
				op: Operation::Binary(BinOperation::Add),
				right: Box::new(Expr::BinOp(BinOp {
					left: Box::new(Expr::Number(7.0)),
					op: Operation::Binary(BinOperation::Div),
					right: Box::new(Expr::Number(8.0))
				}))
			})
		))
	);

	assert_eq!(
		parse_expr("2 + (6 + 7) / 8"),
		Ok((
			"",
			Expr::BinOp(BinOp {
				left: Box::new(Expr::Number(2.0)),
				op: Operation::Binary(BinOperation::Add),
				right: Box::new(Expr::BinOp(BinOp {
					left: Box::new(Expr::Group(Group {
						expr: Box::new(Expr::BinOp(BinOp {
							left: Box::new(Expr::Number(6.0)),
							op: Operation::Binary(BinOperation::Add),
							right: Box::new(Expr::Number(7.0))
						}))
					})),
					op: Operation::Binary(BinOperation::Div),
					right: Box::new(Expr::Number(8.0))
				}))
			})
		))
	);
}

#[test]
fn test_array_parsing() {
	assert_eq!(
		parse_array("[5, 7, 5, 1]"),
		Ok((
			"",
			Array {
				elements: vec![Expr::Number(5.0), Expr::Number(7.0), Expr::Number(5.0), Expr::Number(1.0)]
			}
		))
	);
}

#[test]
fn test_var_set_parsing() {
	assert_eq!(
		parse_var_set("test_variable = 6"),
		Ok((
			"",
			VarSetStmt {
				ident: "test_variable".into(),
				expr: Expr::Number(6.0)
			}
		))
	);
}

#[test]
fn test_var_decl_parsing() {
	assert_eq!(
		parse_var_decl("test_variable := 6"),
		Ok((
			"",
			VarDecl {
				ident: "test_variable".into(),
				init: Expr::Number(6.0)
			}
		))
	);
}

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
	assert_eq!(
		func("delay(1, 2)"),
		Ok((
			"",
			Call {
				name: "delay".into(),
				args: vec![Expr::Number(1.0), Expr::Number(2.0)]
			}
		))
	);

	assert_eq!(
		func("delay(1)"),
		Ok((
			"",
			Call {
				name: "delay".into(),
				args: vec![Expr::Number(1.0)]
			}
		))
	);

	assert_eq!(
		func("exit()"),
		Ok((
			"",
			Call {
				name: "exit".into(),
				args: vec![]
			}
		))
	);
}

#[test]
fn test_bool_parsing() {
	assert_eq!(parse_bool("false"), Ok(("", false)));
	assert_eq!(parse_bool("true"), Ok(("", true)));
}

#[test]
fn test_number_parsing() {
	assert_eq!(parse_number("5"), Ok(("", 5.0)));
	assert_eq!(parse_number("5.5"), Ok(("", 5.5)));
	assert_eq!(parse_number("5.0"), Ok(("", 5.0)));
	assert_eq!(parse_number("-34532.55"), Ok(("", -34532.55)));
}

#[test]
fn test_ident_parsing() {
	assert_eq!(parse_ident("test"), Ok(("", "test".into())));
	assert_eq!(parse_ident("test_wadad"), Ok(("", "test_wadad".into())));
	assert_eq!(parse_ident("awdwaafds"), Ok(("", "awdwaafds".into())));

	assert_eq!(parse_ident("awdwaafds 5gg"), Ok((" 5gg", "awdwaafds".into())));
}
