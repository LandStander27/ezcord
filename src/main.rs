use std::sync::Arc;

use anyhow::{Context as AnyhowContext, Result, anyhow};
use clap::Parser;
use tokio::signal::unix::{SignalKind, signal};
use tracing::Level;

use serenity::all::{Ready, ShardManager};
use serenity::async_trait;
use serenity::model::channel::Message;
use serenity::prelude::*;

#[allow(unused)]
use tracing::{debug, error, info, trace, warn};

mod config;
use config::*;

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

struct Handler;

#[async_trait]
impl EventHandler for Handler {
	async fn ready(&self, _ctx: Context, ready: Ready) {
		info!("{} is connected!", ready.user.name);
	}

	async fn message(&self, ctx: Context, msg: Message) {
		if msg.content == "stop bot" {
			let data = ctx.data.read().await;
			let shard_manager = data.get::<ShardManagerContainer>().unwrap();
			shard_manager.shutdown_all().await;
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
		.with_env_filter(filter)
		.finish();
	tracing::subscriber::set_global_default(subscriber)?;

	trace!("registered logger");

	let config = load_config(&args.bot)?;
	let mut client = Client::builder(config.token, GatewayIntents::all())
		.event_handler(Handler)
		.await?;

	{
		let mut data = client.data.write().await;
		data.insert::<ShardManagerContainer>(client.shard_manager.clone());
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
