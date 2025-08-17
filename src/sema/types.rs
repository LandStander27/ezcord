use serde::Deserialize;

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub enum Type {
	String,
	Number,
	Bool,
	Array(Box<Type>),
	Void,
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		return f.write_str(&match self {
			Self::Void => "void".into(),
			Self::Number => "Number".into(),
			Self::String => "String".into(),
			Self::Bool => "Boolean".into(),
			Self::Array(inner) => format!("Array[{inner}]"),
		});
	}
}
