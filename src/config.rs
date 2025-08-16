use crate::sema::types::Type as ArgType;
use serde::Deserialize;
use serenity::model::application::CommandOptionType;

impl From<ArgType> for CommandOptionType {
	fn from(val: ArgType) -> CommandOptionType {
		return match val {
			ArgType::Number => CommandOptionType::Integer,
			ArgType::String => CommandOptionType::String,
			ArgType::Bool => CommandOptionType::Boolean,
			ArgType::Void => panic!("argument type cannot be 'void'"),
		};
	}
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq)]
pub enum Event {
	Message,
}

#[derive(Deserialize, Debug)]
pub struct Argument {
	pub name: String,
	pub desc: String,
	pub optional: Option<bool>,

	#[serde(rename(deserialize = "type"))]
	pub typ: ArgType,
}

#[derive(Deserialize, Debug)]
pub struct Command {
	pub name: String,
	pub action: String,
	pub desc: String,
	pub args: Option<Vec<Argument>>,
	pub log: Option<bool>,
}

#[derive(Deserialize, Debug)]
pub struct OnEvent {
	// pub name: String,
	pub event: Event,
	pub action: String,
	// pub desc: String,
	// pub args: Option<Vec<Argument>>,
	// pub log: Option<bool>,
}

#[derive(Deserialize, Debug)]
pub struct Config {
	pub token: String,
	pub command: Option<Vec<Command>>,
	pub on_event: Option<Vec<OnEvent>>,
}

pub fn load_config(s: &str) -> anyhow::Result<Config> {
	let config = toml::from_str(&std::fs::read_to_string(s)?)?;
	return Ok(config);
}
