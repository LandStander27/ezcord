use anyhow::{Result, anyhow};
use chrono::prelude::*;
use ezcord_derive::DynamicEnum;
use serenity::all::*;
use std::sync::Arc;
use tracing::error;

use crate::sema::{decl::*, expr::*, stmt::*, types::Type};

pub struct RuntimeVar {
	pub name: String,
	pub value: ResolvedExpr,
}

pub struct RunnerContext<'a> {
	vars: Vec<Vec<RuntimeVar>>,
	functions: &'a Vec<Function>,
	interaction: &'a CommandInteraction,
	ctx: &'a Context,
	shard_manager: Arc<ShardManager>,
}

impl<'a> RunnerContext<'a> {
	pub fn new(input_vars: Vec<RuntimeVar>, functions: &'a Vec<Function>, interaction: &'a CommandInteraction, ctx: &'a Context, shard_manager: Arc<ShardManager>) -> Self {
		return Self {
			vars: vec![input_vars, Vec::new()],
			functions,
			interaction,
			ctx,
			shard_manager,
		};
	}

	async fn execute_expr(&self, expr: &ResolvedExpr) -> Result<Option<ResolvedExpr>> {
		match expr {
			ResolvedExpr::Call(func) => {
				for builtin in self.functions.iter() {
					if builtin.name() == func.name.as_str() {
						return self.run(builtin, &func.args).await;
					}
				}

				return Err(anyhow!("function does not exist"));
			}
			ResolvedExpr::Ident(var) => {
				for scope in self.vars.iter().rev() {
					for i in scope {
						if i.name == var.name {
							return Ok(Some(i.value.clone()));
						}
					}
				}

				return Err(anyhow!("var does not exist"));
			}
			ResolvedExpr::FmtString(fmt) => {
				let mut s = String::new();
				for frag in &fmt.fragments {
					let expr = Box::pin(self.execute_expr(frag)).await?;
					if let Some(expr) = expr {
						match expr {
							ResolvedExpr::String(str) => s.push_str(&str.s),
							ResolvedExpr::Number(num) => s.push_str(&num.number.to_string()),
							ResolvedExpr::Bool(value) => s.push_str(if value.value { "true" } else { "false" }),
							_ => todo!(),
						}
					}
				}
				return Ok(Some(ResolvedExpr::String(LiteralString { s })));
			}
			_ => return Ok(Some(expr.clone())),
		}
	}

	async fn execute_decl(&mut self, decl: &ResolvedDecl) -> Result<()> {
		match decl {
			ResolvedDecl::Var(var) => {
				let runtime = RuntimeVar {
					name: var.name.clone(),
					value: self
						.execute_expr(var.init.as_ref().unwrap())
						.await?
						.unwrap(),
				};

				self.vars.last_mut().unwrap().push(runtime);
			}
		}
		return Ok(());
	}

	pub async fn execute_stmt(&mut self, stmt: &'a ResolvedStmt) -> Result<()> {
		match stmt {
			ResolvedStmt::Decl(decl) => {
				self.execute_decl(decl).await?;
			}
			ResolvedStmt::Expr(expr) => {
				self.execute_expr(expr).await?;
			}
			ResolvedStmt::If(if_stmt) => {
				let cond = self.execute_expr(&if_stmt.cond).await?.unwrap();
				if let ResolvedExpr::Bool(cond) = cond {
					if cond.value {
						self.vars.push(Vec::new());
						for stmt in &if_stmt.block {
							Box::pin(self.execute_stmt(stmt)).await?;
						}
						self.vars.pop();
					}
				}
			}
			ResolvedStmt::VarSet(var) => {
				let expr = self.execute_expr(&var.expr).await?.unwrap();

				for scope in self.vars.iter_mut().rev() {
					for i in scope {
						if i.name == var.name {
							i.value = expr;
							return Ok(());
						}
					}
				}
			}
		};

		return Ok(());
	}

	async fn run(&self, function: &Function, args: &[ResolvedExpr]) -> Result<Option<ResolvedExpr>> {
		let mut converted_args = Vec::with_capacity(args.len());
		for arg in args {
			match Box::pin(self.execute_expr(arg)).await? {
				Some(s) => converted_args.push(s),
				None => return Err(anyhow!("value is void")),
			}
		}

		return function
			.run(converted_args.as_slice(), self.interaction, self.ctx, &self.shard_manager)
			.await;
	}
}

#[derive(DynamicEnum)]
#[call(pub fn get_type(&self) -> Type)]
#[call(pub fn args(&self) -> &Vec<ResolvedParamDecl>)]
#[call(pub fn name(&self) -> &str)]
#[call(async fn run(&self, args: &[ResolvedExpr], command: &CommandInteraction, ctx: &Context, shard_manager: &Arc<ShardManager>) -> Result<Option<ResolvedExpr>>)]
pub enum Function {
	Delay(Delay),
	Respond(Respond),
	Exit(Exit),
	Time(Time),
}

impl Function {
	#[inline]
	pub fn signature(&self) -> String {
		return format!(
			"{}({})",
			self.name(),
			self.args()
				.iter()
				.map(|a| format!("{}: {}", a.name, a.typ))
				.collect::<Vec<String>>()
				.join(", ")
		);
	}
}

pub struct Delay {
	args: Vec<ResolvedParamDecl>,
}

impl Default for Delay {
	fn default() -> Self {
		return Self {
			args: vec![ResolvedParamDecl { name: "ms".into(), typ: Type::Number }],
		};
	}
}

impl Delay {
	fn name(&self) -> &str {
		return "delay";
	}

	fn get_type(&self) -> Type {
		return Type::Void;
	}

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, args: &[ResolvedExpr], _command: &CommandInteraction, _ctx: &Context, _shard_manager: &Arc<ShardManager>) -> Result<Option<ResolvedExpr>> {
		if let ResolvedExpr::Number(LiteralNumber { number }) = args[0] {
			tokio::time::sleep(std::time::Duration::from_millis(number as u64)).await;
		}

		return Ok(None);
	}
}

pub struct Respond {
	args: Vec<ResolvedParamDecl>,
}

impl Default for Respond {
	fn default() -> Self {
		return Self {
			args: vec![ResolvedParamDecl {
				name: "message".into(),
				typ: Type::String,
			}],
		};
	}
}

impl Respond {
	fn name(&self) -> &str {
		return "respond";
	}

	fn get_type(&self) -> Type {
		return Type::Void;
	}

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, args: &[ResolvedExpr], command: &CommandInteraction, ctx: &Context, _shard_manager: &Arc<ShardManager>) -> Result<Option<ResolvedExpr>> {
		if let ResolvedExpr::String(LiteralString { s }) = &args[0] {
			let data = CreateInteractionResponseMessage::new().content(s);
			let builder = CreateInteractionResponse::Message(data);
			if let Err(why) = command.create_response(&ctx.http, builder).await {
				error!("{why}");
			}
		}

		return Ok(None);
	}
}

pub struct Exit {
	args: Vec<ResolvedParamDecl>,
}

impl Default for Exit {
	fn default() -> Self {
		return Self { args: vec![] };
	}
}

impl Exit {
	fn name(&self) -> &str {
		return "exit";
	}

	fn get_type(&self) -> Type {
		return Type::Void;
	}

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, _args: &[ResolvedExpr], _command: &CommandInteraction, _ctx: &Context, shard_manager: &Arc<ShardManager>) -> Result<Option<ResolvedExpr>> {
		shard_manager.shutdown_all().await;
		return Ok(None);
	}
}

pub struct Time {
	args: Vec<ResolvedParamDecl>,
}

impl Default for Time {
	fn default() -> Self {
		return Self { args: vec![] };
	}
}

impl Time {
	fn name(&self) -> &str {
		return "time";
	}

	fn get_type(&self) -> Type {
		return Type::String;
	}

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, _args: &[ResolvedExpr], _command: &CommandInteraction, _ctx: &Context, _shard_manager: &Arc<ShardManager>) -> Result<Option<ResolvedExpr>> {
		let now: DateTime<Local> = Local::now();

		return Ok(Some(ResolvedExpr::String(LiteralString {
			s: now.format("%Y-%m-%d %H:%M:%S").to_string(),
		})));
	}
}
