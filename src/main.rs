use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use clap::Parser;
use regex::Regex;
use sema::decl::ResolvedVarDecl;
use sema::expr::{LiteralString, ResolvedExpr};
use sema::stmt::ResolvedStmt;
use sema::types::Type;
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
use config::Event as ConfigEvent;
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

pub struct ResolvedCommand {
	statements: Vec<ResolvedStmt>,
	log: bool,
	restrict_to_user: Option<u64>,
}

pub struct ResolvedEvent {
	statements: Vec<ResolvedStmt>,
	event: ConfigEvent,
	filter: Option<Regex>,
	restrict_to_channel: Option<u64>,
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
	type Value = Arc<RwLock<HashMap<String, ResolvedCommand>>>;
}

pub struct EventsContainer;
impl TypeMapKey for EventsContainer {
	type Value = Arc<RwLock<Vec<ResolvedEvent>>>;
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

			if let Some(id) = commands[&command.data.name].restrict_to_user {
				if command.user.id.get() != id {
					error!("unauthorized user");
					let data = CreateInteractionResponseMessage::new().content("unauthorized user");
					let builder = CreateInteractionResponse::Message(data);
					if let Err(why) = command.create_response(&ctx.http, builder).await {
						error!(?why);
					}

					return;
				}
			}

			let mut runner = RunnerContext::new(resolved_vars, &funcs, Some(&command), &ctx, shard_manager.clone());

			let mut start = None;
			if commands[&command.data.name].log {
				info!("'{}' command running", command.data.name);
				start = Some(std::time::Instant::now());
			}

			for stmt in &commands[&command.data.name].statements {
				if let Err(e) = runner.execute_stmt(stmt).await {
					error!("{e}");
					let data = CreateInteractionResponseMessage::new().content(format!("Error in command: {e}"));
					let builder = CreateInteractionResponse::Message(data);
					if let Err(why) = command.create_response(&ctx.http, builder).await {
						error!(?why);
					}

					break;
				}
			}

			if let Some(time) = start {
				info!("'{}' command finished in {}ms", command.data.name, time.elapsed().as_millis());
			}
		}
	}

	async fn ready(&self, ctx: Context, ready: Ready) {
		info!("{} is connected!", ready.user.name);

		let mut builtin_functions = vec![
			Function::Delay(Delay::default()),
			Function::Respond(Respond::default()),
			Function::Exit(Exit::default()),
			Function::Time(Time::default()),
			Function::SendMessage(SendMessage::default()),
			Function::ReplyToMessage(ReplyToMessage::default()),
			Function::GetMessageContent(GetMessageContent::default()),
			Function::GetRandNumber(GetRandNumber::default()),
			Function::GetRandDecimal(GetRandDecimal::default()),
			Function::GetArrayLength(GetArrayLength::default()),
			Function::PrintDebug(PrintDebug::default()),
		];
		debug!("registering slash commands");
		let mut commands = Vec::new();
		{
			let data = ctx.data.read().await;
			let config = data.get::<ConfigContainer>().unwrap();
			let mut parsed_commands = data.get::<CommandsContainer>().unwrap().write().await;
			let mut parsed_events = data.get::<EventsContainer>().unwrap().write().await;

			for cmd in config.command.as_ref().unwrap_or(&vec![]).iter() {
				debug!("creating '{}' command", cmd.name);

				let start = std::time::Instant::now();
				let actions = match parser::parse(cmd.action.clone()) {
					Ok(o) => o,
					Err(e) => {
						error!(
							"{}{e}",
							if e.to_string().contains('\n') {
								"\n"
							} else {
								""
							}
						);
						return;
					}
				};
				trace!("parsing took {}ms", start.elapsed().as_millis());

				let resolved_args = cmd
					.args
					.as_ref()
					.unwrap_or(&vec![])
					.iter()
					.map(|a| ResolvedVarDecl {
						name: a.name.clone(),
						typ: a.typ.clone(),
						init: None,
					})
					.collect();

				let start = std::time::Instant::now();
				let mut sema = sema::Sema::new(&builtin_functions, resolved_args);
				let resolved = match sema.resolve(actions) {
					Ok(o) => o,
					Err(e) => {
						error!(
							"{}{e}",
							if e.to_string().contains('\n') {
								"\n"
							} else {
								""
							}
						);
						return;
					}
				};
				trace!("resolving took {}ms", start.elapsed().as_millis());

				parsed_commands.insert(
					cmd.name.clone(),
					ResolvedCommand {
						statements: resolved,
						log: cmd.log.unwrap_or_default(),
						restrict_to_user: cmd.restrict_to_user,
					},
				);

				let mut slash_command = CreateCommand::new(&cmd.name).description(&cmd.desc);
				for arg in cmd.args.as_ref().unwrap_or(&vec![]).iter() {
					slash_command = slash_command
						.add_option(CreateCommandOption::new(arg.typ.clone().into(), arg.name.clone(), arg.desc.clone()).required(!arg.optional.unwrap_or_default()));
				}
				commands.push(slash_command);
			}

			debug!("creating events");
			for event in config.on_event.as_ref().unwrap_or(&vec![]).iter() {
				let regex = if let Some(ref r) = event.filter {
					let r = match Regex::new(r) {
						Ok(o) => o,
						Err(e) => {
							error!("{e}");
							return;
						}
					};
					Some(r)
				} else {
					None
				};

				let actions = match parser::parse(event.action.clone()) {
					Ok(o) => o,
					Err(e) => {
						error!(
							"{}{e}",
							if e.to_string().contains('\n') {
								"\n"
							} else {
								""
							}
						);
						return;
					}
				};

				let mut sema = sema::Sema::new(
					&builtin_functions,
					vec![
						ResolvedVarDecl {
							name: "message".into(),
							typ: Type::String,
							init: None,
						},
						ResolvedVarDecl {
							name: "channel".into(),
							typ: Type::String,
							init: None,
						},
					],
				);
				let resolved = match sema.resolve(actions) {
					Ok(o) => o,
					Err(e) => {
						error!(
							"{}{e}",
							if e.to_string().contains('\n') {
								"\n"
							} else {
								""
							}
						);
						return;
					}
				};

				parsed_events.push(ResolvedEvent {
					statements: resolved,
					event: event.event,
					filter: regex,
					restrict_to_channel: event.restrict_to_channel,
				});
			}
			debug!("created all events");

			let mut builtin = data.get::<BuiltinFuncsContainer>().unwrap().write().await;
			builtin.append(&mut builtin_functions);

			let guild = GuildId::new(config.guild);
			guild.set_commands(&ctx.http, commands).await.unwrap();
		}

		info!("done setting up");
	}

	async fn message_update(&self, ctx: Context, _old_msg: Option<Message>, _new: Option<Message>, msg: MessageUpdateEvent) {
		if msg.author.is_none() || msg.content.is_none() {
			return;
		}

		if msg.author.unwrap().id == ctx.cache.current_user().id {
			return;
		}

		let global_data = ctx.data.read().await;
		let events = global_data.get::<EventsContainer>().unwrap().read().await;
		let shard_manager = global_data.get::<ShardManagerContainer>().unwrap();
		let funcs = global_data
			.get::<BuiltinFuncsContainer>()
			.unwrap()
			.read()
			.await;

		for event in events
			.iter()
			.filter(|x| x.event == ConfigEvent::MessageEdit || x.event == ConfigEvent::MessageEditOrCreate)
		{
			if let Some(ref id) = event.restrict_to_channel {
				if *id != msg.channel_id.get() {
					continue;
				}
			}

			if let Some(ref filter) = event.filter {
				if !filter.is_match(msg.content.as_ref().unwrap()) {
					continue;
				}
			}

			let mut runner = RunnerContext::new(
				vec![
					RuntimeVar {
						name: "message".into(),
						value: ResolvedExpr::String(LiteralString { s: msg.id.get().to_string() }),
					},
					RuntimeVar {
						name: "channel".into(),
						value: ResolvedExpr::String(LiteralString {
							s: msg.channel_id.get().to_string(),
						}),
					},
				],
				&funcs,
				None,
				&ctx,
				shard_manager.clone(),
			);

			for stmt in event.statements.iter() {
				if let Err(e) = runner.execute_stmt(stmt).await {
					error!("{e}");
					break;
				}
			}
		}
	}

	async fn message(&self, ctx: Context, msg: Message) {
		if msg.author.id == ctx.cache.current_user().id {
			return;
		}

		let global_data = ctx.data.read().await;
		let events = global_data.get::<EventsContainer>().unwrap().read().await;
		let shard_manager = global_data.get::<ShardManagerContainer>().unwrap();
		let funcs = global_data
			.get::<BuiltinFuncsContainer>()
			.unwrap()
			.read()
			.await;

		for event in events
			.iter()
			.filter(|x| x.event == ConfigEvent::MessageCreate || x.event == ConfigEvent::MessageEditOrCreate)
		{
			if let Some(ref id) = event.restrict_to_channel {
				if *id != msg.channel_id.get() {
					continue;
				}
			}

			if let Some(ref filter) = event.filter {
				if !filter.is_match(&msg.content) {
					continue;
				}
			}

			let mut runner = RunnerContext::new(
				vec![
					RuntimeVar {
						name: "message".into(),
						value: ResolvedExpr::String(LiteralString { s: msg.id.get().to_string() }),
					},
					RuntimeVar {
						name: "channel".into(),
						value: ResolvedExpr::String(LiteralString {
							s: msg.channel_id.get().to_string(),
						}),
					},
				],
				&funcs,
				None,
				&ctx,
				shard_manager.clone(),
			);

			for stmt in event.statements.iter() {
				if let Err(e) = runner.execute_stmt(stmt).await {
					error!("{e}");
					break;
				}
			}
		}
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
		data.insert::<EventsContainer>(Arc::new(RwLock::new(Vec::new())));
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
