use serde::Deserialize;

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub enum Type {
	String,
	Number,
	Bool,
	Array(Box<Type>),
	Optional(Box<Type>),
	Any,
	Void,
}

impl Type {
	pub fn is_same(&self, other: &Type) -> bool {
		if self == &Type::Any || other == &Type::Any {
			return true;
		} else if let Type::Array(inner) = self {
			if let Type::Array(other_inner) = other {
				return inner.is_same(other_inner);
			} else {
				return false;
			}
		} else if let Type::Optional(inner) = self {
			if let Type::Optional(other_inner) = other {
				return inner.is_same(other_inner);
			} else {
				return false;
			}
		} else {
			return self == other;
		}
	}
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		return f.write_str(&match self {
			Self::Void => "void".into(),
			Self::Number => "Number".into(),
			Self::String => "String".into(),
			Self::Bool => "Boolean".into(),
			Self::Array(inner) => format!("Array[{inner}]"),
			Self::Optional(inner) => format!("Option<{inner}>"),
			Self::Any => "Any".into(),
		});
	}
}
