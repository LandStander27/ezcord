use nom::Offset;
use nom_language::error::{VerboseError, VerboseErrorKind};

use std::fmt::Write;

pub(super) fn convert_error(input: &String, err: VerboseError<&str>) -> String {
	let mut result = String::new();

	for (substring, kind) in err.errors.iter() {
		let offset = input.offset(substring);

		if input.is_empty() {
			match kind {
				VerboseErrorKind::Char(c) => write!(&mut result, "error: expected '{c}', got empty input\n\n"),
				VerboseErrorKind::Context(ctx) => write!(&mut result, "error: in {ctx}, got empty input\n\n"),
				VerboseErrorKind::Nom(e) => write!(&mut result, "error: in {e:?}, got empty input\n\n"),
			}
			.unwrap();

			continue;
		}

		let prefix = &input.as_bytes()[..offset];

		let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;
		let line_begin = prefix
			.iter()
			.rev()
			.position(|&b| b == b'\n')
			.map(|pos| offset - pos)
			.unwrap_or(0);

		let line = input[line_begin..]
			.lines()
			.next()
			.unwrap_or(&input[line_begin..])
			.trim_end();

		let column_number = line.offset(substring) + 1;

		match kind {
			VerboseErrorKind::Char(c) => {
				if let Some(actual) = substring.chars().next() {
					write!(
						&mut result,
						"error: expected '{c}', found {actual}\n   --> line {line_number}\n     {line}\n{caret:>column$}\n\n",
						caret = "^",
						column = column_number + 5
					)
				} else {
					write!(
						&mut result,
						"error: expected '{c}', got end of input\n   --> line {line_number}\n     {line}\n{caret:>column$}\n\n",
						caret = "^",
						column = column_number + 5
					)
				}
			}
			VerboseErrorKind::Context(ctx) => {
				write!(
					&mut result,
					"error: {ctx}\n   --> line {line_number}\n     {line}\n{caret:>column$}\n\n",
					caret = "^",
					column = column_number + 5
				)
			}
			VerboseErrorKind::Nom(err) => {
				write!(
					&mut result,
					"error: {err:?}\n   --> line {line_number}\n     {line}\n{caret:>column$}\n\n",
					caret = "^",
					column = column_number + 5
				)
			}
		}
		.unwrap();
	}

	return result;
}
