use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, Criterion};
use lasso::Rodeo;
use lei::parser::grammar::parse;

mod file_help;

fn bench_file(c: &mut Criterion, name: &str, code: &str) {
	let show = format!("ast: {}", name);

	c.bench_function(&show, |b| {
		b.iter(|| {
			let mut rodeo = Rodeo::new();
			let bump = Bump::new();

			parse(code, &mut rodeo, &bump).unwrap();
		});
	});
}

fn sample_config() -> Criterion {
	Criterion::default().sample_size(30)
}

fn run_bench(c: &mut Criterion) {
	file_help::for_each_file(c, bench_file);
}

criterion_group! {
	name = ast_bench;
	config = sample_config();
	targets = run_bench
}

criterion_main!(ast_bench);
