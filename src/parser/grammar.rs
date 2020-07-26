use super::{
	error::{Error, Opt, Res},
	lexer::{LexState, Token},
	number::{as_binary_priority, num_amb_radix, UNARY_PRIORITY},
	string::eval_str_seq,
};
use crate::ast::{
	node::*,
	types::{TypeDeclaration, TypeField, TypeInfo},
};
use bumpalo::{collections::Vec, vec, Bump};
use lasso::{Rodeo, Spur};
use logos::{Logos, Span};
use std::convert::TryInto;

fn as_name_of_expr(exp: &Expression) -> Option<Spur> {
	// now that's a retro game reference
	if let Expression::Value {
		value: Value::Var(var),
		..
	} = exp
	{
		if var.var.suffixes.is_empty() {
			if let Prefix::Name(name) = &var.var.prefix {
				return Some(name.node);
			}
		}
	}

	None
}

fn is_suffix_start(token: &Token) -> bool {
	matches!(
		token,
		Token::Dot
			| Token::OpenBracket
			| Token::Colon
			| Token::OpenParentheses
			| Token::OpenBrace
			| Token::QuotedString
			| Token::ApostString
			| Token::LongString
	)
}

fn is_suffixed_call(suff: &Suffixed) -> bool {
	matches!(suff.suffixes.last(), Some(Suffix::Call(_)))
}

fn is_expr_bound(token: &Token) -> bool {
	matches!(token, Token::ClosedParentheses | Token::Equal) || is_stat_bound(token)
}

fn is_stat_bound(token: &Token) -> bool {
	matches!(
		token,
		Token::Eof
			| Token::Break
			| Token::Continue
			| Token::Else
			| Token::ElseIf
			| Token::End | Token::Return
			| Token::Until
	)
}

pub struct State<'a, 'b> {
	pub lex: LexState<'a>,
	pub rodeo: &'a mut Rodeo,
	pub bump: &'b Bump,
}

impl<'a, 'b> State<'a, 'b> {
	fn parse_list_nonempty<T, S, F>(&mut self, sep: S, func: F) -> Res<Vec<'b, T>>
	where
		T: 'b,
		S: Fn(&mut State) -> bool,
		F: Fn(&mut State<'a, 'b>) -> Res<T>,
	{
		let mut list = vec![in self.bump; func(self)?];

		while sep(self) {
			list.push(func(self)?);
		}

		Ok(list)
	}

	fn parse_list<T, G, S, F>(&mut self, guard: G, sep: S, func: F) -> Res<Vec<'b, T>>
	where
		T: 'b,
		G: Fn(&State) -> bool,
		S: Fn(&mut State) -> bool,
		F: Fn(&mut State<'a, 'b>) -> Res<T>,
	{
		if guard(self) {
			return Ok(Vec::new_in(self.bump));
		}

		self.parse_list_nonempty(sep, func)
	}

	fn skip_table_sep(&mut self) -> bool {
		self.lex
			.test_next(Token::Comma)
			.or_else(|_| self.lex.test_next(Token::Semicolon))
			.is_ok()
	}

	fn parse_type_list(&mut self) -> Res<Vec<'b, TypeInfo<'b>>> {
		// this func is also used for <T>, but that's not optional
		// so the ClosedParentheses check should be fine
		self.parse_list(
			|st| st.lex.token == Token::ClosedParentheses,
			|st| st.lex.test_next(Token::Comma).is_ok(),
			Self::parse_type_info,
		)
	}

	fn aux_table_parser<T, F>(&mut self, func: F) -> Res<Vec<'b, T>>
	where
		T: 'b,
		F: Fn(&mut State<'a, 'b>) -> Res<T>,
	{
		let sm_brace = self.lex.test_next(Token::OpenBrace)?;
		let fields = self.parse_list(
			|st| st.lex.token == Token::ClosedBrace,
			|st| st.skip_table_sep() && st.lex.token != Token::ClosedBrace,
			func,
		)?;

		if !fields.is_empty() {
			self.skip_table_sep();
		}

		self.lex
			.test_close(Token::ClosedBrace, Token::OpenBrace, sm_brace)?;
		Ok(fields)
	}

	fn parse_func_type(&mut self) -> Res<TypeInfo<'b>> {
		let sm_paren = self.lex.test_next(Token::OpenParentheses)?;
		let arguments = self.parse_type_list()?;

		self.lex
			.test_close(Token::ClosedParentheses, Token::OpenParentheses, sm_paren)?;
		self.lex.test_next(Token::ThinArrow)?;

		// fixme: doesn't really support chained functions
		let ret = match self.lex.token {
			Token::OpenParentheses => {
				let sm_paren = self.lex.test_next(Token::OpenParentheses)?;
				let types = self.parse_type_list()?;

				self.lex
					.test_close(Token::ClosedParentheses, Token::OpenParentheses, sm_paren)?;
				Ok(TypeInfo::Tuple { types })
			}
			_ => self.parse_type_info(),
		}?;

		Ok(TypeInfo::Callback {
			arguments,
			return_type: self.bump.alloc(ret),
		})
	}

	fn parse_field_type(&mut self) -> Res<TypeField<'b>> {
		match self.lex.token {
			Token::OpenBracket => {
				let sm_brack = self.lex.test_next(Token::OpenBracket)?;
				let key = self.parse_type_info()?;

				self.lex
					.test_close(Token::ClosedBracket, Token::OpenBracket, sm_brack)?;

				let value = self.parse_type_info()?;

				Ok(TypeField::ExpressionKey { key, value })
			}
			Token::Ident => {
				let key = self.rodeo.get_or_intern(self.lex.lex.slice());

				self.lex.next();
				self.lex.test_next(Token::Equal)?;

				let value = self.parse_type_info()?;

				Ok(TypeField::NameKey { key, value })
			}
			_ => {
				let err = self.lex.error_exp_slice("<type field>");

				Err(err)
			}
		}
	}

	fn parse_table_type(&mut self) -> Res<TypeInfo<'b>> {
		let fields = self.aux_table_parser(Self::parse_field_type)?;

		Ok(TypeInfo::Table { fields })
	}

	fn parse_typeof(&mut self) -> Res<TypeInfo<'b>> {
		self.lex.test_next_slice("typeof")?;

		let expr = self.parse_surrounded(Token::OpenParentheses, Token::ClosedParentheses)?;
		let inner = self.bump.alloc(expr);

		Ok(TypeInfo::Typeof { inner })
	}

	fn parse_basic_type(&mut self) -> Res<TypeInfo<'b>> {
		let base = self.rodeo.get_or_intern(self.lex.lex.slice());

		self.lex.test_next(Token::Ident)?;

		let value = match self.lex.test_next(Token::LessThan) {
			Ok(sm_less) => {
				let generics = self.parse_type_list()?;

				self.lex
					.test_close(Token::GreaterThan, Token::LessThan, sm_less)?;
				TypeInfo::Generic { base, generics }
			}
			Err(_) => TypeInfo::Basic(base),
		};

		Ok(value)
	}

	fn parse_sub_type(&mut self) -> Res<TypeInfo<'b>> {
		let unboxed = match self.lex.token {
			Token::OpenParentheses => self.parse_func_type(),
			Token::OpenBrace => self.parse_table_type(),
			Token::Ident if self.lex.lex.slice() == "typeof" => self.parse_typeof(),
			Token::Ident => self.parse_basic_type(),
			_ => {
				let err = self.lex.error_exp_slice("<type>");

				return Err(err);
			}
		}?;
		let value = match self.lex.test_next(Token::Question) {
			Ok(_) => TypeInfo::Optional {
				base: self.bump.alloc(unboxed),
			},
			Err(_) => unboxed,
		};

		Ok(value)
	}

	fn parse_type_info(&mut self) -> Res<TypeInfo<'b>> {
		let list = self.parse_list_nonempty(
			|st| st.lex.test_next(Token::Pipe).is_ok(),
			Self::parse_sub_type,
		)?;
		let value = if list.len() == 1 {
			list.into_iter().next().unwrap()
		} else {
			TypeInfo::Union { types: list }
		};

		Ok(value)
	}

	fn parse_ident(&mut self) -> Res<NodeLine<Spur>> {
		let node = self.rodeo.get_or_intern(self.lex.lex.slice());
		let ident = self.lex.node_line(node);

		self.lex.test_next(Token::Ident)?;
		Ok(ident)
	}

	fn parse_var_type(&mut self) -> Opt<TypeInfo<'b>> {
		self.lex
			.test_next(Token::Colon)
			.map_or(Ok(None), |_| self.parse_type_info().map(Some))
	}

	fn parse_var_name(&mut self) -> Res<VarName<'b>> {
		let name = self.parse_ident()?.node;
		let type_info = self.parse_var_type()?;

		Ok(VarName { name, type_info })
	}

	fn parse_var_name_list(&mut self, maybe: bool) -> Res<Vec<'b, VarName<'b>>> {
		self.parse_list(
			|st| maybe && st.lex.token != Token::Ident,
			|st| st.lex.test_next(Token::Comma).is_ok(),
			Self::parse_var_name,
		)
	}

	fn parse_surrounded(&mut self, pre: Token, post: Token) -> Res<Expression<'b>> {
		let sm = self.lex.test_next(pre.clone())?;
		let expr = self.parse_expression()?;

		self.lex.test_close(post, pre, sm)?;
		Ok(expr)
	}

	fn parse_field(&mut self) -> Res<Field<'b>> {
		match self.lex.token {
			Token::OpenBracket => {
				let key = self.parse_surrounded(Token::OpenBracket, Token::ClosedBracket)?;

				self.lex.test_next(Token::Equal)?;

				let value = self.parse_expression()?;

				Ok(Field::ExpressionKey { key, value })
			}
			Token::Ident => {
				let value = self.parse_expression()?; // value form
				let field = match as_name_of_expr(&value) {
					Some(key) if self.lex.test_next(Token::Equal).is_ok() => {
						let value = self.parse_expression()?;

						Field::NameKey { key, value }
					}
					_ => Field::NoKey { value },
				};

				Ok(field)
			}
			_ => {
				let value = self.parse_expression()?;

				Ok(Field::NoKey { value })
			}
		}
	}

	fn aux_table_constructor(&mut self) -> Res<TableConstructor<'b>> {
		let fields = self.aux_table_parser(Self::parse_field)?;

		Ok(TableConstructor { fields })
	}

	// a bit redundant but helps wrapping
	fn parse_table_value(&mut self) -> Res<Value<'b>> {
		self.aux_table_constructor().map(Value::TableConstructor)
	}

	fn parse_func_value(&mut self) -> Res<Value<'b>> {
		let kw_func = self.lex.test_next(Token::Function)?;
		let body = self.parse_func_body()?;

		self.lex.test_close(Token::End, Token::Function, kw_func)?;
		Ok(Value::Function(body))
	}

	fn as_symbol_value(&mut self) -> Res<Value<'b>> {
		let node = match self.lex.token {
			Token::True => LSymbol::True,
			Token::False => LSymbol::False,
			Token::Nil => LSymbol::Nil,
			Token::Ellipse => LSymbol::Ellipse,
			_ => {
				let err = self.lex.error_exp_slice("<value>");

				return Err(err);
			}
		};
		let value = self.lex.node_line(node);

		self.lex.next();
		Ok(Value::Symbol(value))
	}

	fn parse_number_value(&mut self, radix: u8, off: usize) -> Res<Value<'b>> {
		let slice = &self.lex.lex.slice()[off..];
		let number = num_amb_radix(slice, radix);

		self.lex.next();
		Ok(Value::Number(number))
	}

	fn aux_string(&mut self) -> Res<Spur> {
		let base = self.lex.lex.span().start + 1;
		let slice = self.lex.lex.slice();
		let value = &slice[1..slice.len() - 1];

		self.lex.next();
		eval_str_seq(value, base)
			.map(|v| self.rodeo.get_or_intern(v))
			.map_err(Error::StaticError)
	}

	fn parse_string_value(&mut self) -> Res<Value<'b>> {
		self.aux_string().map(Value::String)
	}

	fn aux_long_string(&mut self) -> Spur {
		let slice = self.lex.lex.slice();
		let eq = slice.chars().skip(1).take_while(|&c| c == '=').count();
		let range = eq + 2..slice.len() - eq - 2;

		self.lex.next();
		self.rodeo.get_or_intern(&slice[range])
	}

	fn parse_long_string_value(&mut self) -> Res<Value<'b>> {
		let value = self.aux_long_string();

		Ok(Value::String(value))
	}

	fn parse_prefix(&mut self) -> Res<Prefix<'b>> {
		let value = match self.lex.token {
			Token::Ident => {
				let name = self.parse_ident()?;

				Prefix::Name(name)
			}
			Token::OpenParentheses => {
				let expr =
					self.parse_surrounded(Token::OpenParentheses, Token::ClosedParentheses)?;
				let value = self.bump.alloc(Value::Parentheses(expr));

				Prefix::Expression(Expression::Value { value })
			}
			_ => {
				let err = self.lex.error_exp_slice("<prefix>");

				return Err(err);
			}
		};

		Ok(value)
	}

	fn parse_suffix_index(&mut self) -> Res<Suffix<'b>> {
		let value = match self.lex.token {
			Token::Dot => {
				self.lex.next();

				let name = self.parse_ident()?;

				Index::Dot(name)
			}
			Token::OpenBracket => {
				let expr = self.parse_surrounded(Token::OpenBracket, Token::ClosedBracket)?;

				Index::Brackets(expr)
			}
			_ => {
				let err = self.lex.error_exp_slice("<index>");

				return Err(err);
			}
		};

		Ok(Suffix::Index(value))
	}

	fn parse_func_args(&mut self) -> Res<FunctionArgs<'b>> {
		let value = match self.lex.token {
			Token::OpenParentheses => {
				let sm_paren = self.lex.test_next(Token::OpenParentheses)?;
				let expr = self.parse_expr_list(true)?;

				self.lex
					.test_close(Token::ClosedParentheses, Token::OpenParentheses, sm_paren)?;
				FunctionArgs::Parentheses(expr)
			}
			Token::OpenBrace => {
				let table = self.aux_table_constructor()?;

				FunctionArgs::TableConstructor(table)
			}
			Token::QuotedString | Token::ApostString => {
				let value = self.aux_string()?;

				FunctionArgs::String(value)
			}
			Token::LongString => {
				let value = self.aux_long_string();

				FunctionArgs::String(value)
			}
			_ => {
				let err = self.lex.error_exp_slice("<arguments>");

				return Err(err);
			}
		};

		Ok(value)
	}

	fn parse_suffix_method(&mut self) -> Res<Suffix<'b>> {
		self.lex.test_next(Token::Colon)?;

		let name = self.parse_ident()?;
		let args = self.parse_func_args()?;
		let call = Call::MethodCall(MethodCall { name, args });

		Ok(Suffix::Call(call))
	}

	fn parse_suffix_call(&mut self) -> Res<Suffix<'b>> {
		let args = self.parse_func_args()?;
		let call = Call::AnonymousCall(args);

		Ok(Suffix::Call(call))
	}

	fn parse_suffix(&mut self) -> Res<Suffix<'b>> {
		match self.lex.token {
			Token::Dot | Token::OpenBracket => self.parse_suffix_index(),
			Token::Colon => self.parse_suffix_method(),
			Token::OpenParentheses
			| Token::OpenBrace
			| Token::QuotedString
			| Token::ApostString
			| Token::LongString => self.parse_suffix_call(),
			_ => {
				let err = self.lex.error_exp_slice("<suffix>");

				Err(err)
			}
		}
	}

	fn parse_suffixed_pair(&mut self) -> Res<Suffixed<'b>> {
		let prefix = self.parse_prefix()?;
		let suffixes = self.parse_list(
			|st| !is_suffix_start(&st.lex.token),
			|st| is_suffix_start(&st.lex.token),
			Self::parse_suffix,
		)?;

		Ok(Suffixed { prefix, suffixes })
	}

	fn parse_suffixed_value(&mut self) -> Res<Value<'b>> {
		let suff = self.parse_suffixed_pair()?;
		let value = if is_suffixed_call(&suff) {
			Value::FunctionCall(FunctionCall { call: suff })
		} else {
			Value::Var(Var { var: suff })
		};

		Ok(value)
	}

	fn parse_simple_expr(&mut self) -> Res<Expression<'b>> {
		let unboxed = match self.lex.token {
			Token::OpenBrace => self.parse_table_value(),
			Token::Function => self.parse_func_value(),
			Token::True | Token::False | Token::Nil | Token::Ellipse => self.as_symbol_value(),
			Token::BinNumber => self.parse_number_value(2, 2),
			Token::HexNumber => self.parse_number_value(16, 2),
			Token::DecNumber | Token::NormNumber => self.parse_number_value(10, 0),
			Token::QuotedString | Token::ApostString => self.parse_string_value(),
			Token::LongString => self.parse_long_string_value(),
			Token::Ident | Token::OpenParentheses => self.parse_suffixed_value(),
			_ => {
				let err = self.lex.error_exp_slice("<value>");

				return Err(err);
			}
		}?;
		let value = self.bump.alloc(unboxed);

		Ok(Expression::Value { value })
	}

	fn parse_unop_expr(&mut self, node: UnOp) -> Res<Expression<'b>> {
		let un_op = self.lex.node_line(node);

		self.lex.next();

		let unboxed = self.parse_sub_expr(UNARY_PRIORITY)?;
		let rhs = self.bump.alloc(unboxed);

		Ok(Expression::UnaryOp { rhs, un_op })
	}

	fn parse_sub_expr(&mut self, min_prec: u8) -> Res<Expression<'b>> {
		let mut lhs = match self.lex.token.clone().try_into() {
			Ok(node) => self.parse_unop_expr(node),
			Err(_) => self.parse_simple_expr(),
		}?;

		while let Ok(node) = self.lex.token.clone().try_into() {
			let (lhs_prec, rhs_prec) = as_binary_priority(node);
			let bin_op = self.lex.node_line(node);

			if lhs_prec < min_prec {
				break;
			}

			self.lex.next();

			let rhs = self.parse_sub_expr(rhs_prec)?;

			lhs = Expression::BinaryOp {
				lhs: self.bump.alloc(lhs),
				rhs: self.bump.alloc(rhs),
				bin_op,
			};
		}

		Ok(lhs)
	}

	fn parse_expression(&mut self) -> Res<Expression<'b>> {
		self.parse_sub_expr(0)
	}

	fn parse_expr_list(&mut self, maybe: bool) -> Res<Vec<'b, Expression<'b>>> {
		self.parse_list(
			|st| maybe && is_expr_bound(&st.lex.token),
			|st| st.lex.test_next(Token::Comma).is_ok(),
			Self::parse_expression,
		)
	}

	fn parse_do_block(&mut self) -> Res<Stmt<'b>> {
		let kw_do = self.lex.test_next(Token::Do)?;
		let block = self.parse_block()?;

		self.lex.test_close(Token::End, Token::Do, kw_do)?;
		Ok(Stmt::Do(Do { block }))
	}

	fn parse_param_list(&mut self) -> Res<Vec<'b, Parameter<'b>>> {
		let mut list = Vec::new_in(self.bump);

		while self.lex.token != Token::ClosedParentheses {
			let (variadic, param) = match self.lex.test_next(Token::Ellipse) {
				Ok(_) => (true, Parameter::Ellipse),
				Err(_) => (false, Parameter::Name(self.parse_var_name()?)),
			};

			list.push(param);

			if variadic || self.lex.test_next(Token::Comma).is_err() {
				break;
			}
		}

		Ok(list)
	}

	fn parse_func_ret_type(&mut self) -> Opt<TypeInfo<'b>> {
		self.lex
			.test_next(Token::Colon)
			.map_or(Ok(None), |_| self.parse_type_info().map(Some))
	}

	fn parse_func_body(&mut self) -> Res<FunctionBody<'b>> {
		let sm_paren = self.lex.test_next(Token::OpenParentheses)?;
		let parameters = self.parse_param_list()?;

		self.lex
			.test_close(Token::ClosedParentheses, Token::OpenParentheses, sm_paren)?;

		let return_type = self.parse_func_ret_type()?;
		let block = self.parse_block()?;

		Ok(FunctionBody {
			parameters,
			return_type,
			block,
		})
	}

	fn parse_func_decl(&mut self) -> Res<Stmt<'b>> {
		let kw_func = self.lex.test_next(Token::Function)?;
		let name =
			self.parse_list_nonempty(|st| st.lex.test_next(Token::Dot).is_ok(), Self::parse_ident)?;
		let body = self.parse_func_body()?;

		self.lex.test_close(Token::End, Token::Function, kw_func)?;

		let func = self.bump.alloc(FunctionDeclaration { name, body });

		Ok(Stmt::FunctionDeclaration(func))
	}

	fn aux_for_numeric_loop(&mut self, var: VarName<'b>) -> Res<Stmt<'b>> {
		self.lex.test_next(Token::Equal)?;

		let start = self.parse_expression()?;

		self.lex.test_next(Token::Comma)?;

		let end = self.parse_expression()?;
		let step = match self.lex.test_next(Token::Comma) {
			Ok(_) => Some(self.parse_expression()?),
			Err(_) => None,
		};

		self.lex.test_next(Token::Do)?;

		let block = self.parse_block()?;
		let numeric = self.bump.alloc(NumericFor {
			var,
			start,
			end,
			step,
			block,
		});

		Ok(Stmt::NumericFor(numeric))
	}

	fn aux_for_generic_loop(&mut self, name_list: Vec<'b, VarName<'b>>) -> Res<Stmt<'b>> {
		self.lex.test_next(Token::In)?;

		let expr_list = self.parse_expr_list(false)?;

		self.lex.test_next(Token::Do)?;

		let block = self.parse_block()?;
		let generic = self.bump.alloc(GenericFor {
			name_list,
			expr_list,
			block,
		});

		Ok(Stmt::GenericFor(generic))
	}

	fn parse_for_loop(&mut self) -> Res<Stmt<'b>> {
		let kw_for = self.lex.test_next(Token::For)?;
		let name_list = self.parse_var_name_list(true)?;
		let stat = match (&self.lex.token, name_list) {
			// found no idents
			(Token::Equal, mut v) if v.len() == 1 => {
				let last = v.pop().unwrap();

				self.aux_for_numeric_loop(last)
			}
			(Token::In, v) if !v.is_empty() => self.aux_for_generic_loop(v),
			(_, v) => {
				let err = if v.is_empty() {
					self.lex.error_exp_token(Token::Ident)
				} else {
					self.lex.error_exp_slice("'in' or `=`")
				};

				return Err(err);
			}
		};

		self.lex.test_close(Token::End, Token::For, kw_for)?;
		stat
	}

	fn parse_if_sub(&mut self) -> Res<IfStat<'b>> {
		let condition = self.parse_expression()?;

		self.lex.test_next(Token::Then)?;

		let block = self.parse_block()?;

		Ok(IfStat { condition, block })
	}

	fn parse_if_sub_list(&mut self) -> Res<Vec<'b, IfStat<'b>>> {
		self.parse_list_nonempty(
			|st| st.lex.test_next(Token::ElseIf).is_ok(),
			Self::parse_if_sub,
		)
	}

	fn parse_if_else(&mut self) -> Opt<Block<'b>> {
		match self.lex.test_next(Token::Else) {
			Ok(_) => self.parse_block().map(Some),
			Err(_) => Ok(None),
		}
	}

	fn parse_if_stat(&mut self) -> Res<Stmt<'b>> {
		let kw_if = self.lex.test_next(Token::If)?;
		let stat_list = self.parse_if_sub_list()?;
		let else_opt = self.parse_if_else()?;

		self.lex.test_close(Token::End, Token::If, kw_if)?;
		Ok(Stmt::If(If {
			stat_list,
			else_opt,
		}))
	}

	fn parse_local_stat_func(&mut self, kw_func: Span) -> Res<Stmt<'b>> {
		let name = self.parse_ident()?.node;
		let body = self.parse_func_body()?;

		self.lex.test_close(Token::End, Token::Function, kw_func)?;

		let func = self.bump.alloc(LocalFunction { name, body });

		Ok(Stmt::LocalFunction(func))
	}

	fn parse_local_stat_assign(&mut self) -> Res<Stmt<'b>> {
		let name_list = self.parse_var_name_list(false)?;
		let expr_list = match self.lex.test_next(Token::Equal) {
			Ok(_) => self.parse_expr_list(false)?,
			Err(_) => Vec::new_in(self.bump),
		};

		Ok(Stmt::LocalAssignment(LocalAssignment {
			name_list,
			expr_list,
		}))
	}

	fn parse_local_stat(&mut self) -> Res<Stmt<'b>> {
		self.lex.test_next(Token::Local)?;

		match self.lex.test_next(Token::Function) {
			Ok(kw_func) => self.parse_local_stat_func(kw_func),
			Err(_) => self.parse_local_stat_assign(),
		}
	}

	fn parse_repeat_loop(&mut self) -> Res<Stmt<'b>> {
		let kw_repeat = self.lex.test_next(Token::Repeat)?;
		let block = self.parse_block()?;

		self.lex
			.test_close(Token::Until, Token::Repeat, kw_repeat)?;

		let until = self.parse_expression()?;

		Ok(Stmt::Repeat(Repeat { block, until }))
	}

	fn parse_type_decl(&mut self) -> Res<Stmt<'b>> {
		self.lex.test_next_slice("type")?;

		let base = self.parse_ident()?.node;
		let generics = match self.lex.test_next(Token::LessThan) {
			Ok(left) => {
				let list = self.parse_list_nonempty(
					|st| st.lex.test_next(Token::Comma).is_ok(),
					|st| st.parse_ident().map(|v| v.node),
				)?;

				self.lex
					.test_close(Token::LessThan, Token::GreaterThan, left)?;
				list
			}
			Err(_) => Vec::new_in(self.bump),
		};

		self.lex.test_next(Token::Equal)?;

		let declare_as = self.parse_type_info()?;

		Ok(Stmt::TypeDeclaration(TypeDeclaration {
			base,
			generics,
			declare_as,
		}))
	}

	fn parse_while_loop(&mut self) -> Res<Stmt<'b>> {
		let kw_while = self.lex.test_next(Token::While)?;
		let condition = self.parse_expression()?;

		self.lex.test_next(Token::Do)?;

		let block = self.parse_block()?;

		self.lex.test_close(Token::End, Token::While, kw_while)?;
		Ok(Stmt::While(While { condition, block }))
	}

	fn aux_assignment(&mut self, var_list: Vec<'b, Var<'b>>) -> Res<Stmt<'b>> {
		let op = &self.lex.token;

		match (op, op.clone().try_into()) {
			(_, Ok(op)) if var_list.len() == 1 => {
				self.lex.next();

				let var = var_list.into_iter().next().unwrap();
				let expr = self.parse_expression()?;

				Ok(Stmt::CompAssignment(CompAssignment { var, expr, op }))
			}
			(Token::Equal, Err(_)) => {
				self.lex.next();

				let expr_list = self.parse_expr_list(false)?;

				Ok(Stmt::Assignment(Assignment {
					var_list,
					expr_list,
				}))
			}
			_ => {
				let err = Error::new_static(self.lex.lex.span(), "Malformed statement");

				Err(err)
			}
		}
	}

	fn parse_stat_expr(&mut self) -> Res<Stmt<'b>> {
		let lhs = self.parse_list_nonempty(
			|st| st.lex.test_next(Token::Comma).is_ok(),
			Self::parse_suffixed_pair,
		)?;

		if lhs.len() == 1 && is_suffixed_call(&lhs[0]) {
			let call = lhs.into_iter().next().unwrap();

			return Ok(Stmt::FunctionCall(FunctionCall { call }));
		} else if lhs.iter().any(is_suffixed_call) {
			let err = Error::new_static(self.lex.lex.span(), "Malformed call");

			return Err(err);
		}

		let mut var_list = Vec::with_capacity_in(lhs.len(), self.bump);

		var_list.extend(lhs.into_iter().map(|var| Var { var }));
		self.aux_assignment(var_list)
	}

	fn parse_stmt(&mut self) -> Res<Stmt<'b>> {
		match self.lex.token {
			Token::Do => self.parse_do_block(),
			Token::Function => self.parse_func_decl(),
			Token::For => self.parse_for_loop(),
			Token::If => self.parse_if_stat(),
			Token::Local => self.parse_local_stat(),
			Token::Repeat => self.parse_repeat_loop(),
			Token::Ident if self.lex.lex.slice() == "type" => self.parse_type_decl(),
			Token::While => self.parse_while_loop(),
			Token::Ident | Token::OpenParentheses => self.parse_stat_expr(),
			_ => Err(self.lex.error_exp_slice("<statement>")),
		}
	}

	fn parse_last_stmt(&mut self) -> Opt<LastStmt<'b>> {
		let span = self.lex.lex.span();
		let value = match self.lex.token {
			Token::Break => {
				self.lex.next();
				Some(LastStmt::Break(span))
			}
			Token::Continue => {
				self.lex.next();
				Some(LastStmt::Continue(span))
			}
			Token::Return => {
				self.lex.next();

				let ret_list = self.parse_expr_list(true)?;

				Some(LastStmt::Return(ret_list))
			}
			_ => None,
		};

		Ok(value)
	}

	fn skip_semicolons(&mut self) {
		while self.lex.test_next(Token::Semicolon).is_ok() {}
	}

	fn parse_block(&mut self) -> Res<Block<'b>> {
		let mut stmts = Vec::new_in(self.bump);

		while !is_stat_bound(&self.lex.token) {
			stmts.push(self.parse_stmt()?);
			self.skip_semicolons();
		}

		let last_stmt = self.parse_last_stmt()?;

		self.skip_semicolons();

		Ok(Block { stmts, last_stmt })
	}
}

pub fn parse<'b>(code: &str, rodeo: &mut Rodeo, bump: &'b Bump) -> Res<FunctionBody<'b>> {
	let lex = LexState {
		lex: Token::lexer(code),
		token: Token::Error,
	};

	let mut state = State { rodeo, bump, lex };

	state.lex.next();

	let block = state.parse_block()?;

	state.lex.test_next(Token::Eof)?;
	Ok(FunctionBody {
		parameters: vec![in bump; Parameter::Ellipse],
		return_type: None,
		block,
	})
}
