pub mod anf;
pub mod compile_match;
pub mod core;
pub mod env;
pub mod go;
pub mod mangle;
pub mod mono;
pub mod pipeline;
pub mod pprint;
pub mod query;
pub mod rename;
pub mod tast;
pub mod type_encoding;
pub mod typer;

#[cfg(test)]
mod tests;
