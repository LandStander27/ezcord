use serde::Deserialize;

#[derive(Debug, Copy, Clone, PartialEq, Deserialize)]
pub enum Type {
	String,
	Number,
	Bool,
	Void,
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		return f.write_str(match self {
			Self::Void => "void",
			Self::Number => "Number",
			Self::String => "String",
			Self::Bool => "Boolean",
		});
	}
}
