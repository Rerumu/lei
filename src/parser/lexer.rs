use super::error::{Error, Res};
use crate::ast::node::NodeLine;
use logos::{Lexer, Logos, Span};

pub struct Line(usize);

impl Default for Line {
	fn default() -> Self {
		Self(1)
	}
}

fn line_count(lex: &mut Lexer<Token>) {
	let num = lex.slice().chars().filter(|&x| x == '\n').count();

	lex.extras.0 += num;
}

fn long_lexeme_head(slice: &str) -> Option<usize> {
	// starts with `[`?
	if !slice.starts_with('[') {
		return None;
	}

	// how many `=` after `[`?
	let count = slice.chars().skip(1).take_while(|&v| v == '=').count();

	// ends with `[`?
	if !matches!(slice.chars().nth(count + 1), Some('[')) {
		return None;
	}

	Some(count)
}

fn long_lexeme(lex: &mut Lexer<Token>, skips: usize) {
	let num_eq = match lex.slice().get(skips..).map(long_lexeme_head).flatten() {
		Some(v) => v,
		None => return,
	};

	let mut search = false;
	let mut num = 0;

	for (i, v) in lex.remainder().char_indices() {
		match (search, v) {
			(true, '=') => num += 1,
			(true, ']') if num_eq == num => {
				lex.bump(i + 1);
				break;
			}
			(false, ']') => {
				search = true;
				num = 0;
			}
			_ => search = false,
		}
	}

	line_count(lex);
}

fn line_and_skip(lex: &mut Lexer<Token>) -> logos::Skip {
	line_count(lex);
	logos::skip(lex)
}

fn long_and_skip(lex: &mut Lexer<Token>) -> logos::Skip {
	long_lexeme(lex, 2);
	logos::skip(lex)
}

#[logos(extras = Line)]
#[derive(Logos, PartialEq, Clone, Debug)]
pub enum Token {
	#[error]
	Error,

	Eof,

	// (skip) whitespace
	#[regex(r"\p{White_Space}+", line_and_skip)]
	// (skip) line comment
	#[regex(r"--([^\n(\[=*\[)].*)?", logos::skip)]
	// (skip) long comment
	#[regex(r"--\[=*\[", long_and_skip)]
	// normal grammar
	#[regex(r"[_\p{L}][_\p{L}\p{N}]*")]
	Ident,

	#[regex("\"([^\"]|\\\\.)*\"")]
	QuotedString,

	#[regex(r"'([^']|\\.)*'")]
	ApostString,

	#[regex(r"\[=*\[", |x| long_lexeme(x, 0))]
	LongString,

	#[regex(r"0[xX][0-9a-fA-F_]+([pP][0-9a-fA-F_]+)?(\.[0-9a-fA-F_]*)?")]
	HexNumber,

	#[regex(r"0[bB][01_]+([eE][01_]+)?(\.[01_]*)?")]
	BinNumber,

	#[regex(r"\.[0-9][0-9_]*([eE][\+\-]?[0-9_]+)?")]
	DecNumber,

	#[regex(r"[0-9][0-9_]*(\.[0-9_]*)?([eE][\+\-]?[0-9_]+)?")]
	NormNumber,

	#[token("-")]
	Minus,

	#[token("-=")]
	MinusEqual,

	#[token("->")]
	ThinArrow,

	#[token(",")]
	Comma,

	#[token(";")]
	Semicolon,

	#[token(":")]
	Colon,

	#[token("?")]
	Question,

	#[token(".")]
	Dot,

	#[token("..")]
	TwoDots,

	#[token("...")]
	Ellipse,

	#[token("(")]
	OpenParentheses,

	#[token(")")]
	ClosedParentheses,

	#[token("[")]
	OpenBracket,

	#[token("]")]
	ClosedBracket,

	#[token("{")]
	OpenBrace,

	#[token("}")]
	ClosedBrace,

	#[token("*")]
	Star,

	#[token("*=")]
	StarEqual,

	#[token("/")]
	Slash,

	#[token("/=")]
	SlashEqual,

	#[token("#")]
	Hash,

	#[token("%")]
	Percent,

	#[token("%=")]
	PercentEqual,

	#[token("^")]
	Caret,

	#[token("^=")]
	CaretEqual,

	#[token("+")]
	Plus,

	#[token("+=")]
	PlusEqual,

	#[token("<")]
	LessThan,

	#[token("<=")]
	LessThanEqual,

	#[token("=")]
	Equal,

	#[token("==")]
	TwoEquals,

	#[token(">")]
	GreaterThan,

	#[token(">=")]
	GreaterThanEqual,

	#[token("|")]
	Pipe,

	#[token("~=")]
	TildeEqual,

	#[token("and")]
	And,

	#[token("break")]
	Break,

	#[token("continue")]
	Continue,

	#[token("do")]
	Do,

	#[token("else")]
	Else,

	#[token("elseif")]
	ElseIf,

	#[token("end")]
	End,

	#[token("false")]
	False,

	#[token("for")]
	For,

	#[token("function")]
	Function,

	#[token("if")]
	If,

	#[token("in")]
	In,

	#[token("local")]
	Local,

	#[token("nil")]
	Nil,

	#[token("not")]
	Not,

	#[token("or")]
	Or,

	#[token("repeat")]
	Repeat,

	#[token("return")]
	Return,

	#[token("then")]
	Then,

	#[token("true")]
	True,

	#[token("until")]
	Until,

	#[token("while")]
	While,
}

impl<'a> std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let value = match self {
			Token::Error => "<unknown token>",
			Token::Eof => "<eof>",
			Token::Ident => "<identifier>",
			Token::QuotedString | Token::ApostString | Token::LongString => "<string>",
			Token::HexNumber | Token::BinNumber | Token::DecNumber | Token::NormNumber => {
				"<number>"
			}
			Token::Minus => "`-`",
			Token::MinusEqual => "`-=`",
			Token::ThinArrow => "`->`",
			Token::Comma => "`,`",
			Token::Semicolon => "`;`",
			Token::Colon => "`:`",
			Token::Question => "`?`",
			Token::Dot => "`.`",
			Token::TwoDots => "`..`",
			Token::Ellipse => "`...`",
			Token::OpenParentheses => "`(`",
			Token::ClosedParentheses => "`)`",
			Token::OpenBracket => "`[`",
			Token::ClosedBracket => "`]`",
			Token::OpenBrace => "`{`",
			Token::ClosedBrace => "`}`",
			Token::Star => "`*`",
			Token::StarEqual => "`*=`",
			Token::Slash => "`/`",
			Token::SlashEqual => "`/=`",
			Token::Hash => "`#`",
			Token::Percent => "`%`",
			Token::PercentEqual => "`%=`",
			Token::Caret => "`^`",
			Token::CaretEqual => "`^=`",
			Token::Plus => "`+`",
			Token::PlusEqual => "`+=`",
			Token::LessThan => "`<`",
			Token::LessThanEqual => "`<=`",
			Token::Equal => "`=`",
			Token::TwoEquals => "`==`",
			Token::GreaterThan => "`>`",
			Token::GreaterThanEqual => "`>=`",
			Token::Pipe => "`|`",
			Token::TildeEqual => "`~=`",
			Token::And => "'and'",
			Token::Break => "'break'",
			Token::Continue => "'continue'",
			Token::Do => "'do'",
			Token::Else => "'else'",
			Token::ElseIf => "'elseif'",
			Token::End => "'end'",
			Token::False => "'false'",
			Token::For => "'for'",
			Token::Function => "'function'",
			Token::If => "'if'",
			Token::In => "'in'",
			Token::Local => "'local'",
			Token::Nil => "'nil'",
			Token::Not => "'not'",
			Token::Or => "'or'",
			Token::Repeat => "'repeat'",
			Token::Return => "'return'",
			Token::Then => "'then'",
			Token::True => "'true'",
			Token::Until => "'until'",
			Token::While => "'while'",
		};

		write!(f, "{}", value)
	}
}

pub struct LexState<'a> {
	pub lex: Lexer<'a, Token>,
	pub token: Token,
}

impl<'a> LexState<'a> {
	pub fn node_line<T>(&self, node: T) -> NodeLine<T> {
		NodeLine {
			line: self.lex.extras.0,
			node,
		}
	}

	pub fn next(&mut self) {
		self.token = self.lex.next().unwrap_or(Token::Eof);
	}

	pub fn test_next(&mut self, exp: Token) -> Res<Span> {
		if self.token == exp {
			let span = self.lex.span();

			self.next();
			Ok(span)
		} else {
			Err(self.error_exp_token(exp))
		}
	}

	pub fn test_next_slice(&mut self, slice: &'static str) -> Res<()> {
		if self.token == Token::Ident && self.lex.slice() == slice {
			self.next();
			Ok(())
		} else {
			Err(self.error_exp_slice(slice))
		}
	}

	pub fn test_close(&mut self, exp: Token, open: Token, span: Span) -> Res<()> {
		if self.token == exp {
			self.next();
			Ok(())
		} else {
			let err = Error::new_not_closed(span, self.lex.span(), open, exp, self.token.clone());

			Err(err)
		}
	}

	pub fn error_exp_token(&self, expect: Token) -> Error {
		Error::new_token_expect(self.lex.span(), expect, self.token.clone())
	}

	pub fn error_exp_slice(&self, expect: &'static str) -> Error {
		Error::new_slice_expect(self.lex.span(), expect, self.token.clone())
	}
}
