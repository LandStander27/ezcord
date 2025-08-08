use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use clap::Parser;
use sema::decl::ResolvedVarDecl;
use sema::expr::{LiteralString, ResolvedExpr};
use sema::stmt::ResolvedStmt;
use tokio::signal::unix::{SignalKind, signal};
use tracing_subscriber::field::MakeExt;

use serenity::all::*;
use serenity::async_trait;
use serenity::builder::{CreateCommand, CreateCommandOption};
use serenity::model::channel::Message;
use serenity::prelude::*;

#[allow(unused)]
use tracing::{debug, error, info, trace, warn};

mod config;
use config::*;

mod actions;
use actions::*;

mod parser;
mod sema;

#[derive(Parser, Debug)]
#[command(name = "ezcord", version = version::version)]
#[command(about = "Run Discord bots easily!", long_about = None)]
struct Args {
	#[arg(short, long, help = "increase verbosity")]
	verbose: bool,

	#[arg(required = true, help = "path to the bot config")]
	bot: String,
}

macro_rules! one_or_other {
	($cond:expr, $yes:expr, $no:expr) => {{ if $cond { $yes } else { $no } }};
}

pub struct ShardManagerContainer;
impl TypeMapKey for ShardManagerContainer {
	type Value = Arc<ShardManager>;
}

pub struct ConfigContainer;
impl TypeMapKey for ConfigContainer {
	type Value = Arc<Config>;
}

pub struct CommandsContainer;
impl TypeMapKey for CommandsContainer {
	type Value = Arc<RwLock<HashMap<String, Vec<ResolvedStmt>>>>;
}

pub struct BuiltinFuncsContainer;
impl TypeMapKey for BuiltinFuncsContainer {
	type Value = Arc<RwLock<Vec<Function>>>;
}

struct Handler;
#[async_trait]
impl EventHandler for Handler {
	async fn interaction_create(&self, ctx: Context, interaction: Interaction) {
		if let Interaction::Command(command) = interaction {
			let mut resolved_vars = Vec::new();
			for v in command.data.options() {
				let value = match v.value {
					ResolvedValue::String(s) => ResolvedExpr::String(LiteralString { s: s.to_string() }),
					_ => unimplemented!(),
				};

				resolved_vars.push(RuntimeVar { name: v.name.to_string(), value });
			}

			let global_data = ctx.data.read().await;
			let commands = global_data.get::<CommandsContainer>().unwrap().read().await;
			let shard_manager = global_data.get::<ShardManagerContainer>().unwrap();
			let funcs = global_data
				.get::<BuiltinFuncsContainer>()
				.unwrap()
				.read()
				.await;

			let runner = RunnerContext::new(resolved_vars);
			for stmt in &commands[&command.data.name] {
				if let ResolvedStmt::Expr(ResolvedExpr::Call(func)) = stmt {
					for builtin in funcs.iter() {
						if builtin.name() == func.name.as_str() {
							if let Err(e) = runner
								.run(builtin, &func.args, &command, &ctx, shard_manager)
								.await
							{
								error!("{e}");
								let data = CreateInteractionResponseMessage::new().content(format!("Error in command: {e}"));
								let builder = CreateInteractionResponse::Message(data);
								if let Err(why) = command.create_response(&ctx.http, builder).await {
									error!(?why);
								}
							}
						}
					}
				}
			}
		}
	}

	async fn ready(&self, ctx: Context, ready: Ready) {
		info!("{} is connected!", ready.user.name);

		let mut builtin_functions = vec![Function::Delay(Delay::default()), Function::Respond(Respond::default()), Function::Exit(Exit::default())];
		debug!("registering slash commands");
		let mut commands = Vec::new();
		{
			let data = ctx.data.read().await;
			let config = data.get::<ConfigContainer>().unwrap();
			let mut parsed_commands = data.get::<CommandsContainer>().unwrap().write().await;

			for cmd in config.command.as_ref().unwrap_or(&vec![]).iter() {
				let actions = parser::parse(cmd.action.clone()).unwrap();
				let resolved_args = cmd
					.args
					.as_ref()
					.unwrap_or(&vec![])
					.iter()
					.map(|a| ResolvedVarDecl { name: a.name.clone(), typ: a.typ })
					.collect();

				let sema = sema::Sema::new(&builtin_functions, resolved_args);
				let resolved = sema.resolve(actions).unwrap();
				parsed_commands.insert(cmd.name.clone(), resolved);

				let mut slash_command = CreateCommand::new(&cmd.name).description(&cmd.desc);
				for arg in cmd.args.as_ref().unwrap_or(&vec![]).iter() {
					slash_command = slash_command.add_option(CreateCommandOption::new(arg.typ.into(), arg.name.clone(), arg.desc.clone()));
				}
				commands.push(slash_command);
			}

			let mut builtin = data.get::<BuiltinFuncsContainer>().unwrap().write().await;
			builtin.append(&mut builtin_functions);
		}
		let guild = GuildId::new(1065054413493911652);
		guild.set_commands(&ctx.http, commands).await.unwrap();
	}

	async fn message(&self, _ctx: Context, _msg: Message) {
		// if msg.content == "stop bot" {
		// 	let data = ctx.data.read().await;
		// 	let shard_manager = data.get::<ShardManagerContainer>().unwrap();
		// 	shard_manager.shutdown_all().await;
		// }
	}
}

#[tokio::main]
async fn main() -> Result<()> {
	let args = Args::parse();

	let filter = tracing_subscriber::EnvFilter::builder().parse(format!("serenity=warn,ezcord={}", one_or_other!(args.verbose, "trace", "debug")))?;
	let subscriber = tracing_subscriber::fmt()
		.compact()
		.with_file(args.verbose)
		.with_line_number(args.verbose)
		.with_thread_ids(args.verbose)
		.with_target(args.verbose)
		.map_fmt_fields(|f| f.debug_alt())
		.with_env_filter(filter)
		.finish();
	tracing::subscriber::set_global_default(subscriber)?;

	trace!("registered logger");

	let config = load_config(&args.bot)?;
	trace!(loaded_config = ?config);

	let mut client = Client::builder(&config.token, GatewayIntents::all())
		.event_handler(Handler)
		.await?;

	{
		let mut data = client.data.write().await;
		data.insert::<ShardManagerContainer>(client.shard_manager.clone());
		data.insert::<ConfigContainer>(Arc::new(config));
		data.insert::<CommandsContainer>(Arc::new(RwLock::new(HashMap::new())));
		data.insert::<BuiltinFuncsContainer>(Arc::new(RwLock::new(Vec::new())));
	}

	let shard_manager = client.shard_manager.clone();
	tokio::spawn(async move {
		trace!("registering signal hooks");
		let mut int = signal(SignalKind::interrupt()).unwrap();
		let mut term = signal(SignalKind::terminate()).unwrap();

		debug!("waiting for shutdown signal..");
		loop {
			let int = int.recv();
			let term = term.recv();
			tokio::select! {
				_ = int => {
					println!();
					info!("gracefully shutting down");
					break;
				}
				_ = term => {
					info!("gracefully shutting down");
					break;
				}
				_ = tokio::time::sleep(std::time::Duration::from_millis(250)) => {}
			}
		}

		shard_manager.shutdown_all().await;
	});

	client.start().await?;

	return Ok(());
}
