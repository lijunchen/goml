pub mod anf;
pub mod builtins;
pub mod common;
pub mod compile_match;
pub mod core;
pub mod derive;
pub mod env;
pub mod go;
pub mod lift;
pub mod mangle;
pub mod mono;
pub mod pipeline;
pub mod pprint;
pub mod query;
pub mod rename;
pub mod tast;
pub mod typer;

#[cfg(test)]
mod tests;
