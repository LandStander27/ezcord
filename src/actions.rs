use anyhow::{Result, anyhow};
use chrono::prelude::*;
use ezcord_derive::DynamicEnum;
use serenity::all::*;
use std::sync::Arc;
use tracing::error;

use crate::parser::expr::*;
use crate::sema::{decl::*, expr::*, stmt::*, types::Type};

pub struct RuntimeVar {
	pub name: String,
	pub value: ResolvedExpr,
}

pub struct RunnerContext<'a> {
	vars: Vec<Vec<RuntimeVar>>,
	builtin_functions: &'a Vec<Function>,
	interaction: Option<&'a CommandInteraction>,
	ctx: &'a Context,
	shard_manager: Arc<ShardManager>,
}

#[macro_export]
macro_rules! force_downcast {
	($upper:expr, $pattern:tt) => {{
		if let ResolvedExpr::$pattern(inner) = $upper {
			inner
		} else {
			return Err(anyhow!("invalid type; expected '{}'; got '{}'", stringify!($pattern), $upper.get_type()));
		}
	}};
}

impl<'a> RunnerContext<'a> {
	pub fn new(
		input_vars: Vec<RuntimeVar>,
		functions: &'a Vec<Function>,
		interaction: Option<&'a CommandInteraction>,
		ctx: &'a Context,
		shard_manager: Arc<ShardManager>,
	) -> Self {
		return Self {
			vars: vec![input_vars, Vec::new()],
			builtin_functions: functions,
			interaction,
			ctx,
			shard_manager,
		};
	}

	async fn calculate_unary_operation(&mut self, unary: &ResolvedUnaryOp) -> Result<ResolvedExpr> {
		return Ok(match unary.op {
			Operation::Binary(_) => unreachable!(),
			Operation::Unary(ref op) => {
				let expr = self.execute_expr(unary.expr.as_ref()).await?.unwrap();
				match op {
					UnaryOperation::Neg => ResolvedExpr::Number(LiteralNumber {
						number: -force_downcast!(expr, Number).number,
					}),
					UnaryOperation::Not => ResolvedExpr::Bool(LiteralBool {
						value: !force_downcast!(expr, Bool).value,
					}),
					UnaryOperation::UnwrapOption => {
						if let Some(value) = force_downcast!(expr, Option).value {
							*value
						} else {
							return Err(anyhow!("value is none"));
						}
					}
					UnaryOperation::OptionIsSome => ResolvedExpr::Bool(LiteralBool {
						value: force_downcast!(expr, Option).value.is_some(),
					}),
				}
			}
		});
	}

	async fn calculate_binary_operation(&mut self, binary: &ResolvedBinaryOp) -> Result<ResolvedExpr> {
		return Ok(match binary.op {
			Operation::Binary(ref op) => {
				let left = self.execute_expr(binary.left.as_ref()).await?.unwrap();
				let right = self.execute_expr(binary.right.as_ref()).await?.unwrap();
				match op {
					BinOperation::Add => ResolvedExpr::Number(LiteralNumber {
						number: force_downcast!(left, Number).number + force_downcast!(right, Number).number,
					}),
					BinOperation::Sub => ResolvedExpr::Number(LiteralNumber {
						number: force_downcast!(left, Number).number - force_downcast!(right, Number).number,
					}),
					BinOperation::Mul => ResolvedExpr::Number(LiteralNumber {
						number: force_downcast!(left, Number).number * force_downcast!(right, Number).number,
					}),
					BinOperation::Div => ResolvedExpr::Number(LiteralNumber {
						number: force_downcast!(left, Number).number / force_downcast!(right, Number).number,
					}),

					BinOperation::Equals => ResolvedExpr::Bool(LiteralBool { value: left == right }),
					BinOperation::NotEquals => ResolvedExpr::Bool(LiteralBool { value: left != right }),

					BinOperation::Index => {
						let arr = force_downcast!(left, Array);
						let mut index = force_downcast!(right, Number).number.floor() as i64;
						if index < 0 {
							index += arr.elements.len() as i64;
						}

						if index >= arr.elements.len() as i64 || index < 0 {
							return Err(anyhow!("index out of bounds; index = {}; len = {}", index as usize, arr.elements.len()));
						}

						arr.elements[index as usize].clone()
					}

					BinOperation::And => {
						let left = force_downcast!(left, Bool).value;
						let right = force_downcast!(right, Bool).value;

						ResolvedExpr::Bool(LiteralBool { value: left && right })
					}
					BinOperation::Or => {
						let left = force_downcast!(left, Bool).value;
						let right = force_downcast!(right, Bool).value;

						ResolvedExpr::Bool(LiteralBool { value: left || right })
					}
					BinOperation::GreaterOrEqualThan => {
						let left = force_downcast!(left, Number).number;
						let right = force_downcast!(right, Number).number;

						ResolvedExpr::Bool(LiteralBool { value: left >= right })
					}
					BinOperation::GreaterThan => {
						let left = force_downcast!(left, Number).number;
						let right = force_downcast!(right, Number).number;

						ResolvedExpr::Bool(LiteralBool { value: left > right })
					}
					BinOperation::LessOrEqualThan => {
						let left = force_downcast!(left, Number).number;
						let right = force_downcast!(right, Number).number;

						ResolvedExpr::Bool(LiteralBool { value: left <= right })
					}
					BinOperation::LessThan => {
						let left = force_downcast!(left, Number).number;
						let right = force_downcast!(right, Number).number;

						ResolvedExpr::Bool(LiteralBool { value: left < right })
					}
				}
			}
			Operation::Unary(_) => unreachable!(),
		});
	}

	async fn execute_expr(&mut self, expr: &ResolvedExpr) -> Result<Option<ResolvedExpr>> {
		match expr {
			ResolvedExpr::Call(func) => {
				for builtin in self.builtin_functions.iter() {
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
			ResolvedExpr::BinaryOp(binary) => Ok(Some(Box::pin(self.calculate_binary_operation(binary)).await?)),
			ResolvedExpr::UnaryOp(unary) => Ok(Some(Box::pin(self.calculate_unary_operation(unary)).await?)),
			ResolvedExpr::Group(group) => Box::pin(self.execute_expr(&group.expr)).await,
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
					} else if let Some(ref else_block) = if_stmt.else_block {
						self.vars.push(Vec::new());
						for stmt in else_block {
							Box::pin(self.execute_stmt(stmt)).await?;
						}
						self.vars.pop();
					}
				}
			}
			ResolvedStmt::While(while_stmt) => loop {
				let cond = self.execute_expr(&while_stmt.cond).await?.unwrap();
				if let ResolvedExpr::Bool(cond) = cond {
					if cond.value {
						self.vars.push(Vec::new());
						for stmt in &while_stmt.block {
							Box::pin(self.execute_stmt(stmt)).await?;
						}
						self.vars.pop();
					} else {
						break;
					}
				}
			},
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

	async fn run(&mut self, function: &Function, args: &[ResolvedExpr]) -> Result<Option<ResolvedExpr>> {
		let mut converted_args = Vec::with_capacity(args.len());
		for arg in args {
			match Box::pin(self.execute_expr(arg)).await? {
				Some(s) => converted_args.push(s),
				None => return Err(anyhow!("value is void")),
			}
		}

		return function
			.run(converted_args.as_slice(), &self.interaction, self.ctx, &self.shard_manager)
			.await;
	}
}

#[derive(DynamicEnum)]
#[call(pub fn get_type(&self) -> Type)]
#[call(pub fn args(&self) -> &Vec<ResolvedArgDecl>)]
#[call(pub fn name(&self) -> &str)]
#[call(async fn run(&self, args: &[ResolvedExpr], command: &Option<&CommandInteraction>, ctx: &Context, shard_manager: &Arc<ShardManager>) -> Result<Option<ResolvedExpr>>)]
pub enum Function {
	Delay(Delay),
	Respond(Respond),
	Exit(Exit),
	Time(Time),
	SendMessage(SendMessage),
	ReplyToMessage(ReplyToMessage),
	GetMessageContent(GetMessageContent),
	GetRandNumber(GetRandNumber),
	GetRandDecimal(GetRandDecimal),
	GetArrayLength(GetArrayLength),
	PrintDebug(PrintDebug),
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

#[macro_export]
macro_rules! create_function {
	($struct_name:ident: $func_name:ident($($arg_name:ident: $arg_type:expr),*) -> $ret_type:tt ($($arg_name2:ident: $arg_type2:ty),*)$block:block) => {
		#[allow(unused)]
		pub struct $struct_name {
			args: Vec<ResolvedArgDecl>,
		}

		impl Default for $struct_name {
			fn default() -> Self {
				return Self { args: vec![
					$(
						ResolvedArgDecl {
							name: stringify!($arg_name).into(),
							typ: $arg_type
						},
					)*
				] };
			}
		}

		impl $struct_name {
			#[allow(unused)]
			fn name(&self) -> &str {
				return stringify!($func_name);
			}

			#[allow(unused)]
			fn get_type(&self) -> Type {
				return Type::$ret_type;
			}

			#[allow(unused)]
			fn args(&self) -> &Vec<ResolvedArgDecl> {
				return &self.args;
			}

			async fn run(&self, $($arg_name2: $arg_type2),*) -> Result<Option<ResolvedExpr>> {
				$block
			}
		}
	};
}

create_function!(PrintDebug: print_debug(string: Type::String) -> Void (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let s = &force_downcast!(&args[0], String).s;
	tracing::debug!("{s}");

	return Ok(None);
});

create_function!(GetArrayLength: array_len(arr: Type::Array(Box::new(Type::Any))) -> Number (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let arr = &force_downcast!(&args[0], Array).elements;

	return Ok(Some(ResolvedExpr::Number(LiteralNumber { number: arr.len() as f64 })));
});

create_function!(GetRandDecimal: random_decimal(lower: Type::Number, higher: Type::Number) -> Number (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let lower = &force_downcast!(&args[0], Number).number;
	let higher = &force_downcast!(&args[1], Number).number;

	let number = lower + fastrand::f64() * (higher - lower);

	return Ok(Some(ResolvedExpr::Number(LiteralNumber { number })));
});

create_function!(GetRandNumber: random_number(lower: Type::Number, higher: Type::Number) -> Number (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let lower = &force_downcast!(&args[0], Number).number;
	let higher = &force_downcast!(&args[1], Number).number;

	let number = fastrand::i64(*lower as i64..*higher as i64);

	return Ok(Some(ResolvedExpr::Number(LiteralNumber { number: number as f64 })));
});

create_function!(GetMessageContent: message_content(message_id: Type::String, channel_id: Type::String) -> String (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let message_id = &force_downcast!(&args[0], String).s;
	let channel_id = &force_downcast!(&args[1], String).s;

	let msg = ctx
		.http
		.get_message(ChannelId::new(channel_id.parse()?), MessageId::new(message_id.parse()?))
		.await?;

	return Ok(Some(ResolvedExpr::String(LiteralString { s: msg.content })));
});

create_function!(ReplyToMessage: reply_to_message(message: Type::String, message_id: Type::String, channel_id: Type::String) -> Void (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let message = &force_downcast!(&args[0], String).s;
	let message_id = &force_downcast!(&args[1], String).s;
	let channel_id = &force_downcast!(&args[2], String).s;

	let channel_id = ChannelId::new(channel_id.parse()?);
	let message_id = MessageId::new(message_id.parse()?);
	let msg = ctx
		.http
		.get_message(channel_id, message_id)
		.await?;

	msg.reply(&ctx.http, message).await?;

	return Ok(None);
});

create_function!(SendMessage: send_message(message: Type::String, channel_id: Type::String) -> String (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let message = &force_downcast!(&args[0], String).s;
	let channel_id = &force_downcast!(&args[1], String).s;

	let id = ChannelId::new(channel_id.parse()?);
	let new_message = CreateMessage::new().content(message);
	id.send_message(&ctx.http, new_message).await?;

	return Ok(None);
});

create_function!(Delay: delay(ms: Type::Number) -> Void (args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	if let ResolvedExpr::Number(LiteralNumber { number }) = args[0] {
		tokio::time::sleep(std::time::Duration::from_millis(number as u64)).await;
	}

	return Ok(None);
});

create_function!(Respond: respond(message: Type::String) -> Void (args: &[ResolvedExpr], command: &Option<&CommandInteraction>, ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	if command.is_none() {
		return Err(anyhow!("respond can only be used in a slash-command"));
	}

	if let ResolvedExpr::String(LiteralString { s }) = &args[0] {
		let data = CreateInteractionResponseMessage::new().content(s);
		let builder = CreateInteractionResponse::Message(data);
		if let Err(why) = command.unwrap().create_response(&ctx.http, builder).await {
			error!("{why}");
		}
	}

	return Ok(None);
});

create_function!(Exit: exit() -> Void (_args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, shard_manager: &Arc<ShardManager>) {
	shard_manager.shutdown_all().await;
	return Ok(None);
});

create_function!(Time: time() -> String (_args: &[ResolvedExpr], _command: &Option<&CommandInteraction>, _ctx: &Context, _shard_manager: &Arc<ShardManager>) {
	let now: DateTime<Local> = Local::now();

	return Ok(Some(ResolvedExpr::String(LiteralString {
		s: now.format("%Y-%m-%d %H:%M:%S").to_string(),
	})));
});
