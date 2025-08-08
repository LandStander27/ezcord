use anyhow::anyhow;
use tracing::info;

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
	vars: Vec<ResolvedVarDecl>,
}

impl<'a> Sema<'a> {
	pub fn new(functions: &'a Vec<Function>, vars: Vec<ResolvedVarDecl>) -> Self {
		return Self { functions, vars };
	}

	fn get_function(&self, ident: &str) -> anyhow::Result<&Function> {
		for func in self.functions {
			if func.name() == ident {
				return Ok(func);
			}
		}

		return Err(anyhow!("unknown function"));
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
				return Err(anyhow!("invalid argument type; function signature: {}", func.signature()));
			}
			args.push(resolved);
		}

		return Ok(ResolvedCall { name: call.name, args });
	}

	fn resolve_expr(&self, expr: Expr) -> anyhow::Result<ResolvedExpr> {
		return Ok(match expr {
			Expr::Call(call) => ResolvedExpr::Call(self.resolve_call(call)?),
			Expr::Number(number) => ResolvedExpr::Number(LiteralNumber { number }),
			Expr::String(s) => ResolvedExpr::String(LiteralString { s }),
			Expr::Ident(ident) => {
				for var in &self.vars {
					if var.name == ident {
						return Ok(ResolvedExpr::Ident(ResolvedVarExpr { name: var.name.clone(), typ: var.typ }));
					}
				}

				return Err(anyhow!("invalid variable"));
			}
		});
	}

	pub fn resolve(&self, input: Vec<Stmt>) -> anyhow::Result<Vec<ResolvedStmt>> {
		let start = std::time::Instant::now();
		let mut resolved_stmts = Vec::new();

		for stmt in input {
			match stmt {
				Stmt::Expr(expr) => resolved_stmts.push(ResolvedStmt::Expr(self.resolve_expr(expr)?)),
			}
		}

		info!("resolving done; took {}ms", start.elapsed().as_millis());
		return Ok(resolved_stmts);
	}
}
