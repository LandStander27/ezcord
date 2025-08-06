use serde::Deserialize;

#[derive(Deserialize)]
pub struct Command {
	pub name: String,
}

#[derive(Deserialize)]
pub struct Config {
	pub token: String,
	pub command: Option<Vec<Command>>,
}

pub fn load_config(s: &str) -> anyhow::Result<Config> {
	let config = toml::from_str(&std::fs::read_to_string(s)?)?;
	return Ok(config);
	// let token = config
	// 	.get("token")
	// 	.context("'token' must be specified")?
	// 	.as_str()
	// 	.context("'token' must be a string")?;
}
