use std::path::PathBuf;

use app::{App, Dir};
use clap::{Args, Parser};
use miette::IntoDiagnostic;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

mod app;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    target: Option<PathBuf>,

    #[command(flatten)]
    logging: LoggingCli,
}

#[derive(Clone, Args)]
struct LoggingCli {
    #[arg(long)]
    quiet: bool,
    #[arg(long)]
    debug: bool,
    #[arg(long)]
    trace: bool,
}

impl LoggingCli {
    fn level(&self) -> &'static str {
        self.trace
            .then_some("trace")
            .or_else(|| self.debug.then_some("debug"))
            .or_else(|| self.quiet.then_some("warn"))
            .unwrap_or("info")
    }
}

fn main() -> miette::Result<()> {
    miette::set_panic_hook();

    let Cli { target, logging } = Cli::parse();

    init_logging(logging);

    let target = Dir::new_open(target)?;

    iced::application(App::title, App::update, App::view)
        .theme(App::theme)
        .font(iced_fonts::REQUIRED_FONT_BYTES)
        .font(iced_fonts::NERD_FONT_BYTES)
        .run_with(|| App::new(target))
        .into_diagnostic()?;

    Ok(())
}

fn init_logging(cli: LoggingCli) {
    let level = cli.level();

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(tracing_subscriber::EnvFilter::new(format!("rmv={level}")))
        .init();
}
