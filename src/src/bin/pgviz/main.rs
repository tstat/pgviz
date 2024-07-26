use std::{
    fs::File,
    io::{stdin, stdout, BufWriter, LineWriter, Read, Stdout, Write},
    process::{Child, ChildStdin, Command, Stdio},
};

use clap::Parser;
use pgviz::args::{Args, Format};

#[tokio::main]
async fn main() -> Result<(), tokio_postgres::Error> {
    let args = Args::parse();
    let input_str = match args.query.as_str() {
        "-" => {
            let mut s = String::new();
            stdin().read_to_string(&mut s).unwrap();
            s
        }
        _ => args.query,
    };
    let dont_follow_filter = args
        .dont_follow
        .as_ref()
        .map(|x| pgviz::filter::parser::parse(x));
    let filter = pgviz::filter::parser::parse(&input_str);
    // Take the format as given, or from the output file extension, or default
    // to Dot
    let format = args.format.unwrap_or_else(|| {
        match args
            .out
            .as_ref()
            .and_then(|o| o.extension())
            .and_then(|o| o.to_str())
        {
            Some("pdf") => Format::Pdf,
            Some("svg") => Format::Svg,
            _ => Format::Dot,
        }
    });
    let mut write_handle: WriteHandle = match format {
        Format::Dot => match args.out {
            None => WriteHandle::Stdout(LineWriter::new(stdout())),
            Some(out_arg) => WriteHandle::File(BufWriter::new(File::create(out_arg).unwrap())),
        },
        _ => {
            let format_arg = match format {
                Format::Svg => "svg",
                Format::Pdf => "pdf",
                Format::Dot => "dot",
            };
            let mut cmd = Command::new("dot");
            cmd.arg(format!("-T{format_arg}"));
            if let Some(out_arg) = args.out {
                cmd.arg(format!("-o{}", out_arg.display()));
            }
            cmd.stdin(Stdio::piped());
            let mut child = cmd.spawn().unwrap();
            let stdin = child.stdin.take().unwrap();
            WriteHandle::Child(child, stdin)
        }
    };
    pgviz::write_graph(
        &args.connection_string,
        dont_follow_filter,
        args.edge_labels,
        filter,
        &mut write_handle,
    )
    .await?;

    if let WriteHandle::Child(mut child, child_stdin) = write_handle {
        child.stdin = Some(child_stdin);
        let _ = child.wait().unwrap();
    }
    Ok(())
}

enum WriteHandle {
    Stdout(LineWriter<Stdout>),
    File(BufWriter<File>),
    Child(Child, ChildStdin),
}

impl Write for WriteHandle {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            WriteHandle::Stdout(x) => x.write(buf),
            WriteHandle::File(x) => x.write(buf),
            WriteHandle::Child(_, x) => x.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            WriteHandle::Stdout(x) => x.flush(),
            WriteHandle::File(x) => x.flush(),
            WriteHandle::Child(_, x) => x.flush(),
        }
    }
}
