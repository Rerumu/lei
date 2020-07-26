use super::lexer::Token;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::ops::Range;

#[derive(Debug)]
pub struct SliceExpect {
	pub range: Range<usize>,
	pub expect: &'static str,
	pub found: Token,
}

#[derive(Debug)]
pub struct TokenExpect {
	pub range: Range<usize>,
	pub expect: Token,
	pub found: Token,
}

#[derive(Debug)]
pub struct NotClosed {
	pub has_range: Range<usize>,
	pub found_range: Range<usize>,
	pub open: Token,
	pub closed: Token,
	pub found: Token,
}

#[derive(Debug)]
pub struct StaticError {
	pub range: Range<usize>,
	pub value: &'static str,
}

#[derive(Debug)]
pub enum Error {
	SliceExpect(SliceExpect),
	TokenExpect(TokenExpect),
	NotClosed(NotClosed),
	StaticError(StaticError),
}

impl Error {
	pub fn new_slice_expect(range: Range<usize>, expect: &'static str, found: Token) -> Error {
		Error::SliceExpect(SliceExpect {
			range,
			expect,
			found,
		})
	}

	pub fn new_token_expect(range: Range<usize>, expect: Token, found: Token) -> Error {
		Error::TokenExpect(TokenExpect {
			range,
			expect,
			found,
		})
	}

	pub fn new_not_closed(
		has_range: Range<usize>,
		found_range: Range<usize>,
		open: Token,
		closed: Token,
		found: Token,
	) -> Error {
		Error::NotClosed(NotClosed {
			has_range,
			found_range,
			open,
			closed,
			found,
		})
	}

	pub fn new_static(range: Range<usize>, value: &'static str) -> Error {
		Error::StaticError(StaticError { range, value })
	}
}

impl From<Error> for Diagnostic<()> {
	fn from(err: Error) -> Diagnostic<()> {
		let mut diag = Diagnostic::error();

		match err {
			Error::SliceExpect(e) => {
				let label = Label::primary((), e.range);
				let message = format!("{} expected, but found {}", e.expect, e.found);

				diag = diag.with_labels(vec![label]).with_message(message);
			}
			Error::TokenExpect(e) => {
				// repeated, but without ``
				let label = Label::primary((), e.range);
				let message = format!("{} expected, but found {}", e.expect, e.found);

				diag = diag.with_labels(vec![label]).with_message(message);
			}
			Error::NotClosed(e) => {
				let label_list = vec![
					Label::primary((), e.has_range),
					Label::primary((), e.found_range),
				];
				let message = format!("{} expected, but found {}", e.closed, e.found);
				let note = format!("{} was not closed", e.open);

				diag = diag
					.with_labels(label_list)
					.with_message(message)
					.with_notes(vec![note]);
			}
			Error::StaticError(e) => {
				let label = Label::primary((), e.range);

				diag = diag.with_labels(vec![label]).with_message(e.value);
			}
		}

		diag
	}
}

pub type Res<T> = Result<T, Error>;
pub type Opt<T> = Res<Option<T>>;
