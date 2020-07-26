use super::{error::StaticError, number::num_int_radix};
use logos::Logos;
use std::ops::Range;

#[derive(Logos)]
pub enum StrEscape {
	#[error]
	Error,

	#[token(r"\n")]
	SlashN,

	#[token(r"\r")]
	SlashR,

	#[token(r"\t")]
	SlashT,

	#[token("\\\n")]
	SlashNewLine,

	#[token("\\\"")]
	SlashQuote,

	#[token("\\'")]
	SlashApost,

	#[token(r"\\")]
	SlashSlash,

	#[regex(r"\\\d\d?\d?")]
	SlashNum,

	#[regex(r"\\x[0-9a-fA-F][0-9a-fA-F]")]
	SlashHex,

	#[regex(r"\\u\{[0-9a-fA-F]+\}")]
	SlashUnicode,

	#[regex(r"[^\\]+")]
	Normal,
}

fn as_unicode(value: u32, range: Range<usize>) -> Result<String, StaticError> {
	std::char::from_u32(value)
		.map(|v| v.to_string())
		.ok_or_else(|| StaticError {
			range,
			value: "Not valid unicode",
		})
}

fn eval_str_seq_relative(s: &str) -> Result<String, StaticError> {
	let mut buf = String::new();

	for (token, range) in StrEscape::lexer(s).spanned() {
		let owned;
		let value = match token {
			StrEscape::SlashN | StrEscape::SlashNewLine => "\n",
			StrEscape::SlashR => "\r",
			StrEscape::SlashT => "\t",
			StrEscape::SlashQuote => "\"",
			StrEscape::SlashApost => "'",
			StrEscape::SlashSlash => "\\",
			StrEscape::SlashNum => {
				let range = range.start + 1..range.end;
				let num = num_int_radix(&s[range.clone()], 10);

				if num > 0xFF {
					let err = StaticError {
						range,
						value: "Range of [0, 255] expected",
					};

					return Err(err);
				}

				owned = as_unicode(num, range)?;
				owned.as_str()
			}
			StrEscape::SlashHex => {
				let num = num_int_radix(&s[range.start + 2..range.end], 16);

				owned = as_unicode(num, range)?;
				owned.as_str()
			}
			StrEscape::SlashUnicode => {
				let range = range.start + 3..range.end - 1;
				let num = num_int_radix(&s[range.clone()], 16);

				owned = as_unicode(num, range)?;
				owned.as_str()
			}
			StrEscape::Normal => &s[range],
			StrEscape::Error => {
				let err = StaticError {
					range,
					value: "Invalid escape sequence",
				};

				return Err(err);
			}
		};

		buf.push_str(value);
	}

	Ok(buf)
}

// idea: could introduce this into the main lexer in future to avoid start offset
pub fn eval_str_seq(s: &str, start: usize) -> Result<String, StaticError> {
	eval_str_seq_relative(s).map_err(|mut e| {
		let range = e.range.clone();

		e.range = range.start + start..range.end + start;
		e
	})
}
