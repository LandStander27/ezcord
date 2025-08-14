use anyhow::anyhow;

use crate::actions::Function;
use crate::parser::expr::*;
use crate::parser::stmt::*;

pub mod expr;
use expr::*;

pub mod stmt;
use stmt::*;

pub mod decl;
use decl::*;

pub mod types;
use types::*;

pub struct Sema<'a> {
	functions: &'a Vec<Function>,
	vars: Vec<Vec<ResolvedVarDecl>>,
}

impl<'a> Sema<'a> {
	pub fn new(functions: &'a Vec<Function>, vars: Vec<ResolvedVarDecl>) -> Self {
		return Self {
			functions,
			vars: vec![vars, Vec::new()],
		};
	}

	fn get_function(&self, ident: &str) -> anyhow::Result<&Function> {
		for func in self.functions {
			if func.name() == ident {
				return Ok(func);
			}
		}

		return Err(anyhow!("invalid function"));
	}

	fn resolve_call(&self, call: Call) -> anyhow::Result<ResolvedCall> {
		let mut args = Vec::new();

		let func = self.get_function(&call.name)?;
		if func.args().len() != call.args.len() {
			return Err(anyhow!("invalid number of arguments; function signature: {}", func.signature()));
		}

		for (i, arg) in call.args.into_iter().enumerate() {
			let resolved = self.resolve_expr(arg)?;
			if resolved.get_type() != func.args()[i].typ {
				return Err(anyhow!("invalid argument type; function signature: {}; got: {}", func.signature(), resolved.get_type()));
			}
			args.push(resolved);
		}

		return Ok(ResolvedCall {
			name: call.name,
			args,
			ret_type: func.get_type(),
		});
	}

	fn resolve_str(&self, s: LitOrFmtString) -> anyhow::Result<ResolvedExpr> {
		match s {
			LitOrFmtString::Lit(s) => Ok(ResolvedExpr::String(LiteralString { s })),
			LitOrFmtString::Fmt(fmt) => {
				let mut v = Vec::new();
				for frag in fmt.into_iter() {
					v.push(self.resolve_expr(frag)?);
				}
				Ok(ResolvedExpr::FmtString(FmtString { fragments: v }))
			}
		}
	}

	fn resolve_expr(&self, expr: Expr) -> anyhow::Result<ResolvedExpr> {
		return Ok(match expr {
			Expr::Call(call) => ResolvedExpr::Call(self.resolve_call(call)?),
			Expr::Number(number) => ResolvedExpr::Number(LiteralNumber { number }),
			Expr::String(s) => self.resolve_str(s)?,
			Expr::Ident(ident) => {
				for scope in &self.vars {
					for var in scope {
						if var.name == ident {
							return Ok(ResolvedExpr::Ident(ResolvedVarExpr { name: var.name.clone(), typ: var.typ }));
						}
					}
				}

				return Err(anyhow!("invalid variable"));
			}
		});
	}

	fn resolve_decl(&self, decl: Decl) -> anyhow::Result<ResolvedDecl> {
		return Ok(match decl {
			Decl::VarDecl(var) => {
				let expr = self.resolve_expr(var.init)?;

				ResolvedDecl::Var(ResolvedVarDecl {
					name: var.ident,
					typ: expr.get_type(),
					init: Some(expr),
				})
			}
		});
	}

	pub fn resolve(&mut self, input: Vec<Stmt>) -> anyhow::Result<Vec<ResolvedStmt>> {
		let mut resolved_stmts = Vec::new();

		for stmt in input {
			match stmt {
				Stmt::Expr(expr) => resolved_stmts.push(ResolvedStmt::Expr(self.resolve_expr(expr)?)),
				Stmt::Decl(decl) => {
					let decl = self.resolve_decl(decl)?;
					let ResolvedDecl::Var(ref var) = decl;
					self.vars.last_mut().unwrap().push(var.clone());
					resolved_stmts.push(ResolvedStmt::Decl(decl));
				}
			}

			#[cfg(feature = "parse_debug")]
			dbg!(resolved_stmts.last().unwrap());
		}

		return Ok(resolved_stmts);
	}
}
