mod ast;
mod parser;

use bumpalo::Bump;
use codespan_reporting::{
	files::SimpleFile,
	term::{
		emit,
		termcolor::{ColorChoice, StandardStream},
		Config,
	},
};
use lasso::Rodeo;
use parser::grammar::parse;
use std::{env, fs, io, io::BufRead};

fn aux_parse(code: &str) -> Option<String> {
	let mut rodeo = Rodeo::new();
	let bump = Bump::new();
	let file = SimpleFile::new("stdin", code);
	let func = parse(code, &mut rodeo, &bump);

	match func {
		Ok(v) => Some(format!("{:#?}", v)),
		Err(e) => {
			let stderr = StandardStream::stderr(ColorChoice::Always);
			let config = Config::default();
			let diag = e.into();

			emit(&mut stderr.lock(), &config, &file, &diag).unwrap();
			None
		}
	}
}

fn run_file_list() -> io::Result<()> {
	for name in env::args().skip(1) {
		let code = fs::read_to_string(name.as_str())?;

		if let Some(debug) = aux_parse(&code) {
			fs::write(name + ".out", debug)?;
		}
	}

	Ok(())
}

fn run_stdin() -> io::Result<()> {
	for line in io::stdin().lock().lines().flatten() {
		if let Some(debug) = aux_parse(&line) {
			println!("{}", debug);
		}
	}

	Ok(())
}

fn main() -> io::Result<()> {
	if env::args().len() > 1 {
		run_file_list()
	} else {
		run_stdin()
	}
}
