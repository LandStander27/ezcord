use anyhow::Result;
use ezcord_derive::DynamicEnum;
use serenity::all::*;
use std::sync::Arc;
use tracing::error;

use crate::sema::{decl::*, expr::*, types::Type};

pub struct RuntimeVar {
	pub name: String,
	pub value: ResolvedExpr,
}

pub struct RunnerContext {
	vars: Vec<RuntimeVar>,
}

impl RunnerContext {
	pub fn new(vars: Vec<RuntimeVar>) -> Self {
		return Self { vars };
	}

	pub async fn run(&self, function: &Function, args: &[ResolvedExpr], command: &CommandInteraction, ctx: &Context, shard_manager: &Arc<ShardManager>) -> Result<()> {
		let mut converted_args = Vec::with_capacity(args.len());
		for arg in args {
			if let ResolvedExpr::Ident(var) = arg {
				for i in &self.vars {
					if i.name == var.name {
						converted_args.push(&i.value);
					}
				}
			} else {
				converted_args.push(arg);
			}
		}

		function
			.run(converted_args.as_slice(), command, ctx, shard_manager)
			.await?;

		return Ok(());
	}
}

#[derive(DynamicEnum)]
#[call(pub fn args(&self) -> &Vec<ResolvedParamDecl>)]
#[call(pub fn name(&self) -> &str)]
#[call(async fn run(&self, args: &[&ResolvedExpr], command: &CommandInteraction, ctx: &Context, shard_manager: &Arc<ShardManager>) -> Result<()>)]
pub enum Function {
	Delay(Delay),
	Respond(Respond),
	Exit(Exit),
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

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, args: &[&ResolvedExpr], _command: &CommandInteraction, _ctx: &Context, _shard_manager: &Arc<ShardManager>) -> Result<()> {
		if let ResolvedExpr::Number(LiteralNumber { number }) = args[0] {
			tokio::time::sleep(std::time::Duration::from_millis(*number as u64)).await;
		}

		return Ok(());
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

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, args: &[&ResolvedExpr], command: &CommandInteraction, ctx: &Context, _shard_manager: &Arc<ShardManager>) -> Result<()> {
		if let ResolvedExpr::String(LiteralString { s }) = args[0] {
			let data = CreateInteractionResponseMessage::new().content(s);
			let builder = CreateInteractionResponse::Message(data);
			if let Err(why) = command.create_response(&ctx.http, builder).await {
				error!("{why}");
			}
		}

		return Ok(());
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

	fn args(&self) -> &Vec<ResolvedParamDecl> {
		return &self.args;
	}

	async fn run(&self, _args: &[&ResolvedExpr], _command: &CommandInteraction, _ctx: &Context, shard_manager: &Arc<ShardManager>) -> Result<()> {
		// let data = ctx.data.write().await;
		// let manager = data.get::<ShardManagerContainer>().unwrap();
		shard_manager.shutdown_all().await;

		return Ok(());
	}
}
