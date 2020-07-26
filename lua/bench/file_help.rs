use criterion::Criterion;
use std::fs;

pub fn for_each_file<F>(c: &mut Criterion, func: F)
where
	F: Fn(&mut Criterion, &str, &str),
{
	for entry in fs::read_dir("lua/test/").unwrap() {
		let entry = entry.unwrap();
		let name = entry.file_name().into_string().unwrap();
		let source = fs::read_to_string(entry.path()).unwrap();

		func(c, name.as_str(), source.as_str());
	}
}
