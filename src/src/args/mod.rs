use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long, default_value_t = String::from("host=localhost port=5432"))]
    pub connection_string: String,
    #[arg(long)]
    pub dont_follow: Option<String>,
    #[arg(short, long)]
    pub out: Option<PathBuf>,
    #[arg(short, long)]
    pub edge_labels: bool,
    #[arg(value_enum, short, long, default_value_t=Format::Dot)]
    pub format: Format,
    pub query: String,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Format {
    Dot,
    Pdf,
    Svg,
}
