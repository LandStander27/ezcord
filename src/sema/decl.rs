use super::Type;
// use ezcord_derive::DynamicEnum;

// #[derive(DynamicEnum, Debug, Clone)]
// #[call(fn get_name(&self) -> &str)]
// #[call(fn get_type(&self) -> &Type)]
// pub enum ResolvedDecl {
// 	Param(ResolvedParamDecl),
// 	Var(ResolvedVarDecl),
// }

#[derive(Debug, Clone)]
pub struct ResolvedParamDecl {
	pub name: String,
	pub typ: Type,
}

// impl ResolvedParamDecl {
// 	fn get_name(&self) -> &str {
// 		return &self.name;
// 	}

// 	fn get_type(&self) -> &Type {
// 		return &self.typ;
// 	}
// }

#[derive(Debug, Clone)]
pub struct ResolvedVarDecl {
	pub name: String,
	pub typ: Type,
}

// impl ResolvedVarDecl {
// 	fn get_name(&self) -> &str {
// 		return &self.name;
// 	}

// 	fn get_type(&self) -> &Type {
// 		return &self.typ;
// 	}
// }
