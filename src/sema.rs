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
	decls: Vec<Vec<ResolvedDecl>>,
}

macro_rules! get_decl {
	($self:expr, $ident:expr, $type:tt) => {
		'body: {
			for scope in $self.decls.iter().rev() {
				for var in scope {
					if var.get_name() == $ident {
						#[allow(irrefutable_let_patterns)]
						if let ResolvedDecl::$type(inner) = var {
							break 'body Some(inner);
						}
					}
				}
			}

			break 'body None;
		}
	};
}

impl<'a> Sema<'a> {
	pub fn new(functions: &'a Vec<Function>, vars: Vec<ResolvedVarDecl>) -> Self {
		return Self {
			functions,
			decls: vec![vars.into_iter().map(ResolvedDecl::Var).collect()],
		};
	}

	fn get_builtin_function(&self, ident: &str) -> anyhow::Result<&Function> {
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

		match binary.op {
			Operation::Binary(ref binary) => match binary {
				BinOperation::Index => {
					if !matches!(left.get_type(), Type::Array(_)) {
						return Err(anyhow!("cannot index into non-Array, got '{}'", left.get_type()));
					}

					if !right.get_type().is_same(&Type::Number) {
						return Err(anyhow!("expected index to be '{}', got '{}'", Type::Number, right.get_type()));
					}
				}
				_ => {
					if !left.get_type().is_same(&right.get_type()) {
						return Err(anyhow!("cannot do operation on '{}' and '{}'", left.get_type(), right.get_type()));
					}
				}
			},
			_ => unreachable!(),
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
				UnaryOperation::UnwrapOption | UnaryOperation::OptionIsSome => Type::Optional(Box::new(Type::Any)),
			},
			_ => unreachable!(),
		};

		if !expr.get_type().is_same(&needed_type) {
			return Err(anyhow!("cannot do operation on '{}', expected '{}'", expr.get_type(), needed_type));
		}

		return Ok(ResolvedUnaryOp {
			expr: Box::new(expr),
			op: unary.op,
		});
	}

	fn resolve_call(&self, call: Call) -> anyhow::Result<ResolvedCall> {
		let mut args = Vec::new();

		let func = self.get_builtin_function(&call.name)?;
		if func.args().len() != call.args.len() {
			return Err(anyhow!("invalid number of arguments; function signature: {}", func.signature()));
		}

		for (i, arg) in call.args.into_iter().enumerate() {
			let resolved = self.resolve_expr(arg)?;
			if !resolved.get_type().is_same(&func.args()[i].typ) {
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
				if let Some(var) = get_decl!(self, ident, Var) {
					return Ok(ResolvedExpr::Ident(ResolvedVarExpr {
						name: var.name.clone(),
						typ: var.typ.clone(),
					}));
				}

				return Err(anyhow!("invalid variable"));
			}
			Expr::BinOp(binary) => ResolvedExpr::BinaryOp(self.resolve_binary_operation(binary)?),
			Expr::UnaryOp(unary) => ResolvedExpr::UnaryOp(self.resolve_unary_operation(unary)?),
			Expr::Group(group) => ResolvedExpr::Group(ResolvedGroup {
				expr: Box::new(self.resolve_expr(*group.expr)?),
			}),
			Expr::Array(array) => {
				let mut elements = Vec::new();
				let mut element_type: Option<Type> = None;
				for i in array.elements {
					let element = self.resolve_expr(i)?;
					if let Some(ref element_type) = element_type {
						if !element_type.is_same(&element.get_type()) {
							return Err(anyhow!(
								"all elements of array must be of same type; expected '{}', got '{}'",
								element_type,
								element.get_type()
							));
						}
					} else {
						element_type = Some(element.get_type());
					}
					elements.push(element);
				}
				ResolvedExpr::Array(ResolvedArray { elements })
			}
		});
	}

	fn resolve_decl(&mut self, decl: Decl) -> anyhow::Result<ResolvedDecl> {
		return Ok(match decl {
			Decl::Var(var) => {
				let i = self
					.decls
					.last()
					.unwrap()
					.iter()
					.position(|x| x.get_name() == var.ident);
				if let Some(i) = i {
					self.decls.swap_remove(i);
				}

				let expr = self.resolve_expr(var.init)?;

				ResolvedDecl::Var(ResolvedVarDecl {
					name: var.ident,
					typ: expr.get_type(),
					init: Some(expr),
				})
			} // Decl::Fn(func) => {
			  // 	let i = self
			  // 		.decls
			  // 		.last()
			  // 		.unwrap()
			  // 		.iter()
			  // 		.position(|x| x.get_name() == func.ident);
			  // 	if let Some(i) = i {
			  // 		self.decls.swap_remove(i);
			  // 	}

			  // 	let args = func
			  // 		.args
			  // 		.into_iter()
			  // 		.map(|x| ResolvedArgDecl { name: x.ident, typ: x.typ })
			  // 		.collect();
			  // 	let body = self.resolve_block(func.body)?;

			  // 	ResolvedDecl::Fn(ResolvedFnDecl {
			  // 		ident: func.ident,
			  // 		ret_type: func.ret_type,
			  // 		args,
			  // 		body,
			  // 	})
			  // }
		});
	}

	fn resolve_block(&mut self, block: Vec<Stmt>) -> anyhow::Result<Vec<ResolvedStmt>> {
		self.decls.push(Vec::new());
		let mut resolved_stmts = Vec::new();

		for stmt in block {
			match stmt {
				Stmt::Expr(expr) => resolved_stmts.push(ResolvedStmt::Expr(self.resolve_expr(expr)?)),
				Stmt::Decl(decl) => {
					let decl = self.resolve_decl(decl)?;
					self.decls.last_mut().unwrap().push(decl.clone());
					resolved_stmts.push(ResolvedStmt::Decl(decl));
				}
				Stmt::If(if_stmt) => resolved_stmts.push(ResolvedStmt::If(self.resolve_if_stmt(if_stmt)?)),
				Stmt::While(while_stmt) => resolved_stmts.push(ResolvedStmt::While(self.resolve_while_stmt(while_stmt)?)),
				Stmt::VarSet(var) => {
					let selected_var = get_decl!(self, var.ident, Var);
					if let Some(selected_var) = selected_var {
						let expr = self.resolve_expr(var.expr)?;
						if !expr.get_type().is_same(&selected_var.typ) {
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

		self.decls.pop();
		return Ok(resolved_stmts);
	}

	fn resolve_if_stmt(&mut self, stmt: IfStmt) -> anyhow::Result<ResolvedIfStmt> {
		let cond = self.resolve_expr(stmt.cond)?;
		if !cond.get_type().is_same(&Type::Bool) {
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
		if !cond.get_type().is_same(&Type::Bool) {
			return Err(anyhow!("if condition must return a boolean; got: {}", cond.get_type()));
		}

		let block = self.resolve_block(stmt.block)?;
		return Ok(ResolvedWhileStmt { cond, block });
	}

	pub fn resolve(&mut self, input: Vec<Stmt>) -> anyhow::Result<Vec<ResolvedStmt>> {
		return self.resolve_block(input);
	}
}
