use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// PostgreSQL connection string
    #[arg(short, long, default_value_t = String::from("host=localhost port=5432"))]
    pub connection_string: String,
    /// Query for tables to be excluded from bfs
    #[arg(long)]
    pub dont_follow: Option<String>,
    /// A file path to write output (defaults to stdout)
    #[arg(short, long)]
    pub out: Option<PathBuf>,
    /// Label edges with "on update"/"on delete" actions
    #[arg(short, long)]
    pub edge_labels: bool,
    /// Output format
    #[arg(value_enum, short, long, default_value_t=Format::Dot)]
    pub format: Format,
    /// The query string, or "-" to accept the query on stdin
    pub query: String,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Format {
    Dot,
    Pdf,
    Svg,
}
