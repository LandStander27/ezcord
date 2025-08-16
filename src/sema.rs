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
		return Self { functions, vars: vec![vars] };
	}

	fn get_function(&self, ident: &str) -> anyhow::Result<&Function> {
		for func in self.functions {
			if func.name() == ident {
				return Ok(func);
			}
		}

		return Err(anyhow!("invalid function"));
	}

	fn resolve_binary_operation(&self, binary: BinOp) -> anyhow::Result<ResolvedBinaryOp> {
		let left = self.resolve_expr(*binary.left)?;
		let right = self.resolve_expr(*binary.right)?;

		if left.get_type() != right.get_type() {
			return Err(anyhow!("cannot do operation on '{}' and '{}'", left.get_type(), right.get_type()));
		}

		return Ok(ResolvedBinaryOp {
			left: Box::new(left),
			right: Box::new(right),
			op: binary.op,
		});
	}

	fn resolve_unary_operation(&self, unary: UnaryOp) -> anyhow::Result<ResolvedUnaryOp> {
		let expr = self.resolve_expr(*unary.expr)?;

		let needed_type = match unary.op {
			Operation::Unary(ref unary) => match unary {
				UnaryOperation::Neg => Type::Number,
				UnaryOperation::Not => Type::Bool,
			},
			_ => unreachable!(),
		};

		if expr.get_type() != needed_type {
			return Err(anyhow!("cannot do operation on '{}', expected '{}'", expr.get_type(), needed_type));
		}

		return Ok(ResolvedUnaryOp {
			expr: Box::new(expr),
			op: unary.op,
		});
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
			Expr::Bool(value) => ResolvedExpr::Bool(LiteralBool { value }),
			Expr::String(s) => self.resolve_str(s)?,
			Expr::Ident(ident) => {
				for scope in self.vars.iter().rev() {
					for var in scope {
						if var.name == ident {
							return Ok(ResolvedExpr::Ident(ResolvedVarExpr {
								name: var.name.clone(),
								typ: var.typ,
							}));
						}
					}
				}

				return Err(anyhow!("invalid variable"));
			}
			Expr::BinOp(binary) => ResolvedExpr::BinaryOp(self.resolve_binary_operation(binary)?),
			Expr::UnaryOp(unary) => ResolvedExpr::UnaryOp(self.resolve_unary_operation(unary)?),
			Expr::Group(group) => ResolvedExpr::Group(ResolvedGroup {
				expr: Box::new(self.resolve_expr(*group.expr)?),
			}),
		});
	}

	fn resolve_decl(&mut self, decl: Decl) -> anyhow::Result<ResolvedDecl> {
		return Ok(match decl {
			Decl::VarDecl(var) => {
				let i = self
					.vars
					.last()
					.unwrap()
					.iter()
					.position(|x| x.name == var.ident);
				if let Some(i) = i {
					self.vars.swap_remove(i);
				}

				let expr = self.resolve_expr(var.init)?;

				ResolvedDecl::Var(ResolvedVarDecl {
					name: var.ident,
					typ: expr.get_type(),
					init: Some(expr),
				})
			}
		});
	}

	fn resolve_block(&mut self, block: Vec<Stmt>) -> anyhow::Result<Vec<ResolvedStmt>> {
		self.vars.push(Vec::new());
		let mut resolved_stmts = Vec::new();

		for stmt in block {
			match stmt {
				Stmt::Expr(expr) => resolved_stmts.push(ResolvedStmt::Expr(self.resolve_expr(expr)?)),
				Stmt::Decl(decl) => {
					let decl = self.resolve_decl(decl)?;
					let ResolvedDecl::Var(ref var) = decl;
					self.vars.last_mut().unwrap().push(var.clone());
					resolved_stmts.push(ResolvedStmt::Decl(decl));
				}
				Stmt::If(if_stmt) => resolved_stmts.push(ResolvedStmt::If(self.resolve_if_stmt(if_stmt)?)),
				Stmt::While(while_stmt) => resolved_stmts.push(ResolvedStmt::While(self.resolve_while_stmt(while_stmt)?)),
				Stmt::VarSet(var) => {
					let mut selected_var = None;
					for scope in self.vars.iter().rev() {
						for i in scope {
							if i.name == var.ident {
								selected_var = Some(i);
							}
						}
					}

					if let Some(selected_var) = selected_var {
						let expr = self.resolve_expr(var.expr)?;
						if expr.get_type() != selected_var.typ {
							return Err(anyhow!("variable assignment type mismatch"));
						}
						resolved_stmts.push(ResolvedStmt::VarSet(ResolvedVarSet { name: var.ident, expr }));
					} else {
						return Err(anyhow!("variable does not exist"));
					}
				}
			}

			#[cfg(feature = "parse_debug")]
			dbg!(resolved_stmts.last().unwrap());
		}

		self.vars.pop();
		return Ok(resolved_stmts);
	}

	fn resolve_if_stmt(&mut self, stmt: IfStmt) -> anyhow::Result<ResolvedIfStmt> {
		let cond = self.resolve_expr(stmt.cond)?;
		if cond.get_type() != Type::Bool {
			return Err(anyhow!("if condition must return a boolean; got: {}", cond.get_type()));
		}

		let block = self.resolve_block(stmt.block)?;
		let else_block = if let Some(s) = stmt.else_block {
			Some(self.resolve_block(s)?)
		} else {
			None
		};

		return Ok(ResolvedIfStmt { cond, block, else_block });
	}

	fn resolve_while_stmt(&mut self, stmt: WhileStmt) -> anyhow::Result<ResolvedWhileStmt> {
		let cond = self.resolve_expr(stmt.cond)?;
		if cond.get_type() != Type::Bool {
			return Err(anyhow!("if condition must return a boolean; got: {}", cond.get_type()));
		}

		let block = self.resolve_block(stmt.block)?;
		return Ok(ResolvedWhileStmt { cond, block });
	}

	pub fn resolve(&mut self, input: Vec<Stmt>) -> anyhow::Result<Vec<ResolvedStmt>> {
		return self.resolve_block(input);
	}
}
