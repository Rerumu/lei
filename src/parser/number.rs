use crate::ast::node::{BinOp, LNumber};
use lexical_core::{parse_format_radix, NumberFormat};

fn lua_number_format() -> NumberFormat {
	NumberFormat::compile(
		b'_', false, false, false, false, false, false, false, false, false, true, false, false,
		false, true, true, true, false, true, true, true, true, true, true, true, true, false,
	)
	.unwrap()
}

pub fn num_int_radix(num: &str, radix: u8) -> u32 {
	parse_format_radix(num.as_bytes(), radix, lua_number_format()).unwrap_or(u32::max_value())
}

// ambiguous can be an integer or a float
pub fn num_amb_radix(num: &str, radix: u8) -> LNumber {
	let format = lua_number_format();
	let bytes = num.as_bytes();

	parse_format_radix::<i64>(bytes, radix, format).map_or_else(
		|_| {
			parse_format_radix::<f64>(bytes, radix, format)
				.map(LNumber::Float)
				.unwrap()
		},
		LNumber::Integer,
	)
}

pub const UNARY_PRIORITY: u8 = 14;

pub fn as_binary_priority(op: BinOp) -> (u8, u8) {
	match op {
		BinOp::Or => (1, 2),
		BinOp::And => (3, 4),
		BinOp::LessThan
		| BinOp::GreaterThan
		| BinOp::LessThanEqual
		| BinOp::GreaterThanEqual
		| BinOp::TildeEqual
		| BinOp::TwoEquals => (5, 6),
		BinOp::TwoDots => (8, 7),
		BinOp::Plus | BinOp::Minus => (9, 10),
		BinOp::Star | BinOp::Slash | BinOp::Percent => (11, 12),
		BinOp::Caret => (16, 15),
	}
}
