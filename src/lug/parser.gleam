//// Parser for Lua 5.4

import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lug/lexer

type Tokens =
  List(#(lexer.Token, lexer.Position))

pub type Block {
  Block(location: Span, statements: List(Statement))
}

pub type Statement {
  Assignment(
    location: Span,
    names: List(Expression),
    expressions: List(Expression),
  )
  FunctionCall(location: Span, name: Expression, args: List(Expression))
  Label(location: Span, name: String)
  Break(location: Span)
  Goto(location: Span, label: String)
  Do(location: Span, body: Block)
  While(location: Span, condition: Expression, body: Block)
  Repeat(location: Span, condition: Expression, body: Block)
  If(
    location: Span,
    condition: Expression,
    then: Block,
    elseif: List(Elseif),
    else_: option.Option(Block),
  )
  For(
    location: Span,
    identifier: String,
    init: Expression,
    limit: Expression,
    step: option.Option(Expression),
    body: Block,
  )
  ForIn(location: Span, names: List(String), in: List(Expression), do: Block)
  FunctionDeclaration(
    location: Span,
    name: FunctionName,
    parameters: List(Parameter),
    body: Block,
  )
  LocalFunction(
    location: Span,
    name: FunctionName,
    paramaters: List(Parameter),
    body: Block,
  )
  Local(location: Span, names: List(String), expressions: List(Expression))
  Return(location: Span, expressions: List(Expression))
}

pub type Elseif {
  Elseif(location: Span, condition: Expression, block: Block)
}

pub type FunctionName {
  FunctionName(
    root: String,
    subfields: List(String),
    method: option.Option(String),
  )
}

pub type Parameter {
  NamedParameter(location: Span, name: String)
  VariadicParameter(location: Span)
}

pub type Expression {
  BooleanNil(location: Span)
  BooleanTrue(location: Span)
  BooleanFalse(location: Span)
  Numeral(location: Span, value: String)
  LiteralString(location: Span, value: String)
  Variable(location: Span, name: String)
  Vararg(location: Span)
  Function(location: Span, parameters: List(Parameter), body: Block)
  Call(location: Span, function: Expression, args: List(Expression))
  Table(location: Span, fields: List(Field))
  Index(location: Span, table: Expression, key: Expression)
  BinaryOperation(
    location: Span,
    name: BinaryOperator,
    left: Expression,
    right: Expression,
  )
  UnaryOperation(location: Span, name: UnaryOperator, expression: Expression)
}

pub type Field {
  ListField(location: Span, value: Expression)
  RecordField(location: Span, key: Expression, value: Expression)
}

pub type BinaryOperator {
  // arithmetic operators
  Add
  Sub
  Mult
  Div
  FloorDiv
  Pow
  Mod

  // relational operators
  Eq
  NotEq
  Gt
  GtEq
  Lt
  LtEq

  // bitwise operators
  BitwiseAnd
  BitwiseOr
  BitwiseXor
  BitwiseShiftLeft
  BitwiseShiftRight

  // booleans operators
  BooleanAnd
  BooleanOr

  Concat
}

pub type UnaryOperator {
  NumeralNegation
  BooleanNegation
  BitwiseNegation
  Length
}

pub type Span {
  Span(start: lexer.Position, end: lexer.Position)
}

pub type Error {
  UnexpectedToken(token: lexer.Token, position: lexer.Position)
  UnexpectedExpression(expression: Expression)
  UnexpectedEndOfInput
  ExpectedEndOfInput(rest: Tokens)
}

pub fn parse(code: String) -> Result(Block, Error) {
  lexer.new(code)
  |> lexer.discard_comments
  |> lexer.discard_whitespaces
  |> lexer.lex
  |> do_parse
}

pub fn parse_tokens(tokens: Tokens) -> Result(Block, Error) {
  // remove whitespaces and comments
  list.filter(tokens, fn(pair) {
    let #(tok, _pos) = pair
    case tok {
      lexer.Space(_) | lexer.Comment(_) | lexer.LongComment(_) -> False
      _ -> True
    }
  })
  |> do_parse
}

fn do_parse(tokens: Tokens) -> Result(Block, Error) {
  case block(tokens) {
    Ok(#(block, [#(lexer.EndOfFile, _)])) -> Ok(block)
    Ok(#(_block, tokens)) -> Error(ExpectedEndOfInput(tokens))
    Error(e) -> Error(e)
  }
}

fn block(tokens: Tokens) -> Result(#(Block, Tokens), Error) {
  do_block(tokens, [])
}

fn do_block(
  tokens: Tokens,
  acc: List(Statement),
) -> Result(#(Block, Tokens), Error) {
  case tokens {
    [#(lexer.Else, end), ..] as tokens
    | [#(lexer.Elseif, end), ..] as tokens
    | [#(lexer.Until, end), ..] as tokens
    | [#(lexer.End, end), ..] as tokens
    | [#(lexer.EndOfFile, end), ..] as tokens -> {
      let parsed = list.reverse(acc)
      case parsed {
        [] -> Ok(#(Block(Span(end, end), []), tokens))
        [first, ..] -> {
          let span = Span(first.location.start, end)
          Ok(#(Block(span, parsed), tokens))
        }
      }
    }
    [#(lexer.Semicolon, _), ..tokens] -> do_block(tokens, acc)
    tokens -> {
      use #(stmt, tokens) <- result.try(statement(tokens))
      do_block(tokens, [stmt, ..acc])
    }
  }
}

fn statement(tokens: Tokens) -> Result(#(Statement, Tokens), Error) {
  case tokens {
    [#(lexer.ColonColon, start), ..tokens] -> label(tokens, start)
    [#(lexer.Break, start), ..tokens] ->
      Ok(#(Break(token_span(start, lexer.Break)), tokens))
    [#(lexer.Goto, start), ..tokens] -> goto(tokens, start)
    [#(lexer.Do, start), ..tokens] -> do(tokens, start)
    [#(lexer.While, start), ..tokens] -> while(tokens, start)
    [#(lexer.Repeat, start), ..tokens] -> repeat(tokens, start)
    [#(lexer.If, start), ..tokens] -> if_(tokens, start)
    [#(lexer.For, start), ..tokens] -> for(tokens, start)
    [#(lexer.Function, start), ..tokens] -> function_declaration(tokens, start)
    [#(lexer.Local, start), #(lexer.Function, _), ..tokens] ->
      local_function(tokens, start)
    [#(lexer.Local, start), ..tokens] -> local(tokens, start)
    [#(lexer.Return, start), ..tokens] -> return(tokens, start)
    [#(_other, start), ..] -> {
      use #(expr, tokens) <- result.try(expression(tokens))
      case expr {
        Call(span, name, args) -> Ok(#(FunctionCall(span, name, args), tokens))

        // try to parse as an assignment
        variable ->
          case tokens {
            [#(lexer.Equal, _), ..tokens] -> {
              use #(values, end, tokens) <- result.try(
                expression_list(tokens, []),
              )
              let span = Span(start, end)
              Ok(#(Assignment(span, [variable], values), tokens))
            }

            [#(lexer.Comma, _), ..tokens] -> {
              use names, _, tokens <- comma_delimited_until(
                lexer.Equal,
                tokens,
                expression,
              )
              use #(values, end, tokens) <- result.try(
                expression_list(tokens, []),
              )

              let span = Span(start, end)
              Ok(#(Assignment(span, [variable, ..names], values), tokens))
            }

            [#(unexpected, position), ..] ->
              Error(UnexpectedToken(unexpected, position))

            [] -> Error(UnexpectedEndOfInput)
          }
      }
    }
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn label(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(label, _, tokens) <- result.try(identifier(tokens))
  use end, tokens <- expect(lexer.ColonColon, tokens)

  let span = Span(start, end)
  Ok(#(Label(span, label), tokens))
}

fn goto(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(label, _, tokens) <- result.try(identifier(tokens))

  let span = string_span(start, label)
  Ok(#(Goto(span, label), tokens))
}

fn do(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(body, tokens) <- result.try(block(tokens))
  use end, tokens <- expect(lexer.End, tokens)

  let span = Span(start, end)
  Ok(#(Do(span, body), tokens))
}

fn while(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(condition, tokens) <- result.try(expression(tokens))
  use _, tokens <- expect(lexer.Do, tokens)
  use #(body, tokens) <- result.try(block(tokens))
  use end, tokens <- expect(lexer.End, tokens)

  let span = Span(start, end)
  Ok(#(While(span, condition, body), tokens))
}

fn repeat(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(body, tokens) <- result.try(block(tokens))
  use _, tokens <- expect(lexer.Until, tokens)
  use #(condition, tokens) <- result.try(expression(tokens))

  let span = Span(start, condition.location.end)
  Ok(#(Repeat(span, condition, body), tokens))
}

fn if_(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(condition, tokens) <- result.try(expression(tokens))
  use _, tokens <- expect(lexer.Then, tokens)
  use #(then, tokens) <- result.try(block(tokens))
  use #(else_if, else_, tokens) <- result.try(else_if(tokens, []))
  use end, tokens <- expect(lexer.End, tokens)

  let span = Span(start, end)
  Ok(#(If(span, condition, then, else_if, else_), tokens))
}

fn else_if(
  tokens: Tokens,
  acc: List(Elseif),
) -> Result(#(List(Elseif), option.Option(Block), Tokens), Error) {
  case tokens {
    [#(lexer.Elseif, start), ..tokens] -> {
      use #(condition, tokens) <- result.try(expression(tokens))
      use _, tokens <- expect(lexer.Then, tokens)
      use #(block, tokens) <- result.try(block(tokens))

      let span = Span(start, block.location.end)

      else_if(tokens, [Elseif(span, condition, block), ..acc])
    }
    [#(lexer.Else, _), ..tokens] -> {
      use #(else_, tokens) <- result.try(block(tokens))
      Ok(#(list.reverse(acc), option.Some(else_), tokens))
    }
    [#(lexer.End, _), ..] as tokens ->
      Ok(#(list.reverse(acc), option.None, tokens))
    [#(unexpected, position), ..] ->
      Error(UnexpectedToken(unexpected, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn for(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(var, _, rest) <- result.try(identifier(tokens))
  case rest {
    [#(lexer.Equal, _), ..tokens] -> {
      use #(init, tokens) <- result.try(expression(tokens))
      use _, tokens <- expect(lexer.Comma, tokens)
      use #(limit, tokens) <- result.try(expression(tokens))
      use #(step, tokens) <- result.try(case tokens {
        [#(lexer.Comma, _), ..tokens] -> {
          use #(step, tokens) <- result.try(expression(tokens))
          Ok(#(option.Some(step), tokens))
        }

        _ -> Ok(#(option.None, tokens))
      })
      use _, tokens <- expect(lexer.Do, tokens)
      use #(body, tokens) <- result.try(block(tokens))
      use end, tokens <- expect(lexer.End, tokens)

      let span = Span(start, end)
      Ok(#(For(span, var, init, limit, step, body), tokens))
    }
    [#(lexer.Comma, _), ..tokens] -> {
      use #(variables, _, tokens) <- result.try(identifier_list(tokens, []))
      use _, tokens <- expect(lexer.In, tokens)

      use expressions, _, tokens <- comma_delimited_until(
        lexer.Do,
        tokens,
        expression,
      )
      use #(body, tokens) <- result.try(block(tokens))
      use end, tokens <- expect(lexer.End, tokens)

      let span = Span(start, end)
      Ok(#(ForIn(span, [var, ..variables], expressions, body), tokens))
    }
    [#(lexer.In, _), ..tokens] -> {
      use expressions, _, tokens <- comma_delimited_until(
        lexer.Do,
        tokens,
        expression,
      )
      use #(body, tokens) <- result.try(block(tokens))
      use end, tokens <- expect(lexer.End, tokens)

      let span = Span(start, end)
      Ok(#(ForIn(span, [var], expressions, body), tokens))
    }
    [#(unexpected, position), ..] ->
      Error(UnexpectedToken(unexpected, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn function_declaration(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(name, args, body, end, tokens) <- result.try(do_function(tokens))

  let span = Span(start, end)
  Ok(#(FunctionDeclaration(span, name, args, body), tokens))
}

fn local_function(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(name, args, body, end, tokens) <- result.try(do_function(tokens))

  let span = Span(start, end)
  Ok(#(LocalFunction(span, name, args, body), tokens))
}

fn do_function(
  tokens: Tokens,
) -> Result(
  #(FunctionName, List(Parameter), Block, lexer.Position, Tokens),
  Error,
) {
  use #(root, _, tokens) <- result.try(identifier(tokens))
  use #(name, tokens) <- result.try(function_name(root, tokens, []))
  use _, tokens <- expect(lexer.LeftParen, tokens)
  use args, _, tokens <- comma_delimited_until(
    lexer.RightParen,
    tokens,
    parameter,
  )
  use #(body, tokens) <- result.try(block(tokens))
  use end, tokens <- expect(lexer.End, tokens)

  Ok(#(name, args, body, end, tokens))
}

fn function_name(
  root: String,
  tokens: Tokens,
  subfields: List(String),
) -> Result(#(FunctionName, Tokens), Error) {
  case tokens {
    [#(lexer.Dot, _), ..tokens] -> {
      use #(subname, _, tokens) <- result.try(identifier(tokens))
      function_name(root, tokens, [subname, ..subfields])
    }

    [#(lexer.Colon, _), ..tokens] -> {
      use #(method, _, tokens) <- result.try(identifier(tokens))

      Ok(#(
        FunctionName(root, list.reverse(subfields), option.Some(method)),
        tokens,
      ))
    }

    _ -> Ok(#(FunctionName(root, list.reverse(subfields), option.None), tokens))
  }
}

fn local(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  use #(names, end, tokens) <- result.try(identifier_list(tokens, []))
  case tokens {
    [#(lexer.Equal, _), ..tokens] -> {
      use #(values, end, tokens) <- result.try(expression_list(tokens, []))

      let span = Span(start, end)
      Ok(#(Local(span, names, values), tokens))
    }

    _ -> Ok(#(Local(Span(start, end), names, []), tokens))
  }
}

fn return(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Statement, Tokens), Error) {
  case tokens {
    [#(lexer.Else, end), ..] as tokens
    | [#(lexer.Elseif, end), ..] as tokens
    | [#(lexer.Until, end), ..] as tokens
    | [#(lexer.Semicolon, end), ..] as tokens
    | [#(lexer.End, end), ..] as tokens
    | [#(lexer.EndOfFile, end), ..] as tokens -> {
      let span = Span(start, end)
      Ok(#(Return(span, []), tokens))
    }

    _ -> {
      use #(expressions, end, tokens) <- result.try(expression_list(tokens, []))

      let span = Span(start, end)
      Ok(#(Return(span, expressions), tokens))
    }
  }
}

fn expression(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
  do_expression(tokens, 0)
}

fn do_expression(
  tokens: Tokens,
  minimun_binding: Int,
) -> Result(#(Expression, Tokens), Error) {
  use #(prefix, tokens) <- result.try(prefix_expression(tokens))
  bind_expression(prefix, tokens, minimun_binding)
}

fn prefix_expression(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [#(lexer.Nil, start), ..tokens] ->
      Ok(#(BooleanNil(token_span(start, lexer.Nil)), tokens))

    [#(lexer.BTrue, start), ..tokens] ->
      Ok(#(BooleanTrue(token_span(start, lexer.BTrue)), tokens))

    [#(lexer.BFalse, start), ..tokens] ->
      Ok(#(BooleanFalse(token_span(start, lexer.BFalse)), tokens))

    [#(lexer.Int(n), start), ..tokens] | [#(lexer.Float(n), start), ..tokens] ->
      Ok(#(Numeral(string_span(start, n), n), tokens))

    [#(lexer.String(str), start), ..tokens]
    | [#(lexer.LongString(str), start), ..tokens] ->
      Ok(#(LiteralString(string_span(start, str), str), tokens))

    [#(lexer.DotDotDot, start), ..tokens] ->
      Ok(#(Vararg(token_span(start, lexer.DotDotDot)), tokens))

    [#(lexer.Identifier(var), start), ..tokens] ->
      Ok(#(Variable(string_span(start, var), var), tokens))

    [#(lexer.Function, start), ..tokens] -> function_expression(tokens, start)

    [#(lexer.LeftBrace, start), ..tokens] -> table(tokens, start)

    [#(lexer.LeftParen, _), ..tokens] -> {
      use #(prefix, tokens) <- result.try(do_expression(tokens, 0))
      use _, tokens <- expect(lexer.RightParen, tokens)

      Ok(#(prefix, tokens))
    }

    [#(lexer.Minus, start), ..tokens] ->
      unary_operation(NumeralNegation, tokens, start)

    [#(lexer.Hash, start), ..tokens] -> unary_operation(Length, tokens, start)

    [#(lexer.Not, start), ..tokens] ->
      unary_operation(BooleanNegation, tokens, start)

    [#(lexer.Tilde, start), ..tokens] ->
      unary_operation(BitwiseNegation, tokens, start)

    [#(unexpected, position), ..] ->
      Error(UnexpectedToken(unexpected, position))

    [] -> Error(UnexpectedEndOfInput)
  }
}

fn bind_expression(
  prefix: Expression,
  tokens: Tokens,
  minimun_binding: Int,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [#(lexer.Plus, start), ..] ->
      binary_operation(Add, tokens, start, prefix, minimun_binding)

    [#(lexer.Minus, start), ..] ->
      binary_operation(Sub, tokens, start, prefix, minimun_binding)

    [#(lexer.Star, start), ..] ->
      binary_operation(Mult, tokens, start, prefix, minimun_binding)

    [#(lexer.Slash, start), ..] ->
      binary_operation(Div, tokens, start, prefix, minimun_binding)

    [#(lexer.SlashSlash, start), ..] ->
      binary_operation(FloorDiv, tokens, start, prefix, minimun_binding)

    [#(lexer.Circumflex, start), ..] ->
      binary_operation(Pow, tokens, start, prefix, minimun_binding)

    [#(lexer.Percent, start), ..] ->
      binary_operation(Mod, tokens, start, prefix, minimun_binding)

    [#(lexer.EqualEqual, start), ..] ->
      binary_operation(Eq, tokens, start, prefix, minimun_binding)

    [#(lexer.NotEqual, start), ..] ->
      binary_operation(NotEq, tokens, start, prefix, minimun_binding)

    [#(lexer.Greater, start), ..] ->
      binary_operation(Gt, tokens, start, prefix, minimun_binding)

    [#(lexer.GreaterEqual, start), ..] ->
      binary_operation(GtEq, tokens, start, prefix, minimun_binding)

    [#(lexer.Less, start), ..] ->
      binary_operation(Lt, tokens, start, prefix, minimun_binding)

    [#(lexer.LessEqual, start), ..] ->
      binary_operation(LtEq, tokens, start, prefix, minimun_binding)

    [#(lexer.Amper, start), ..] ->
      binary_operation(BitwiseAnd, tokens, start, prefix, minimun_binding)

    [#(lexer.VBar, start), ..] ->
      binary_operation(BitwiseOr, tokens, start, prefix, minimun_binding)

    [#(lexer.Tilde, start), ..] ->
      binary_operation(BitwiseXor, tokens, start, prefix, minimun_binding)

    [#(lexer.GreaterGreater, start), ..] ->
      binary_operation(BitwiseShiftLeft, tokens, start, prefix, minimun_binding)

    [#(lexer.LessLess, start), ..] ->
      binary_operation(
        BitwiseShiftRight,
        tokens,
        start,
        prefix,
        minimun_binding,
      )

    [#(lexer.And, start), ..] ->
      binary_operation(BooleanAnd, tokens, start, prefix, minimun_binding)

    [#(lexer.Or, start), ..] ->
      binary_operation(BooleanOr, tokens, start, prefix, minimun_binding)

    [#(lexer.DotDot, start), ..] ->
      binary_operation(Concat, tokens, start, prefix, minimun_binding)

    [#(lexer.String(str), start), ..tokens]
    | [#(lexer.LongString(str), start), ..tokens] -> {
      let str = LiteralString(string_span(start, str), str)

      let span = Span(prefix.location.start, str.location.end)
      let expr = Call(span, prefix, [str])

      bind_expression(expr, tokens, minimun_binding)
    }

    [#(lexer.LeftBrace, tbl_start), ..tokens] -> {
      use #(tbl, tokens) <- result.try(table(tokens, tbl_start))

      let span = Span(prefix.location.start, tbl.location.end)
      let expr = Call(span, prefix, [tbl])

      bind_expression(expr, tokens, minimun_binding)
    }

    [#(lexer.LeftParen, _), ..tokens] -> {
      use args, end, tokens <- comma_delimited_until(
        lexer.RightParen,
        tokens,
        expression,
      )

      let span = Span(prefix.location.start, end)
      let expr = Call(span, prefix, args)
      bind_expression(expr, tokens, minimun_binding)
    }

    [#(lexer.LeftSquare, _), ..tokens] -> {
      use #(key, tokens) <- result.try(expression(tokens))
      use end, tokens <- expect(lexer.RightSquare, tokens)

      let span = Span(prefix.location.start, end)
      let expr = Index(span, prefix, key)
      bind_expression(expr, tokens, minimun_binding)
    }

    [#(lexer.Dot, start), ..tokens] -> {
      use #(name, _, tokens) <- result.try(identifier(tokens))

      let str = LiteralString(string_span(start, name), name)

      let span = Span(prefix.location.start, str.location.end)
      let expr = Index(span, prefix, str)
      bind_expression(expr, tokens, minimun_binding)
    }

    // method call is desugared when parsing
    [#(lexer.Colon, start), ..tokens] -> {
      use #(method, _, tokens) <- result.try(identifier(tokens))
      let name_span = string_span(start, method)

      let span = Span(prefix.location.start, name_span.end)
      let method = Index(span, prefix, Variable(name_span, method))

      case tokens {
        [#(lexer.LeftParen, _), ..tokens] -> {
          use args, end, tokens <- comma_delimited_until(
            lexer.RightParen,
            tokens,
            expression,
          )

          let span = Span(prefix.location.start, end)
          let expr = Call(span, method, [prefix, ..args])

          bind_expression(expr, tokens, minimun_binding)
        }

        [#(lexer.String(str), start), ..tokens]
        | [#(lexer.LongString(str), start), ..tokens] -> {
          let str = LiteralString(string_span(start, str), str)

          let span = Span(prefix.location.start, str.location.end)
          let expr = Call(span, method, [prefix, str])

          bind_expression(expr, tokens, minimun_binding)
        }

        [#(lexer.LeftBrace, tbl_start), ..tokens] -> {
          use #(tbl, tokens) <- result.try(table(tokens, tbl_start))

          let span = Span(prefix.location.start, tbl.location.end)
          let expr = Call(span, method, [prefix, tbl])

          bind_expression(expr, tokens, minimun_binding)
        }

        [#(unexpected, position), ..] ->
          Error(UnexpectedToken(unexpected, position))
        [] -> Error(UnexpectedEndOfInput)
      }
    }

    // nothing to bind with
    _ -> Ok(#(prefix, tokens))
  }
}

// Lua's lambda
fn function_expression(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Expression, Tokens), Error) {
  use _, tokens <- expect(lexer.LeftParen, tokens)
  use args, _, tokens <- comma_delimited_until(
    lexer.RightParen,
    tokens,
    parameter,
  )
  use #(body, tokens) <- result.try(block(tokens))
  use end, tokens <- expect(lexer.End, tokens)

  let span = Span(start, end)
  Ok(#(Function(span, args, body), tokens))
}

fn table(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Expression, Tokens), Error) {
  use #(fields, end, tokens) <- result.try(do_table(tokens, []))

  let span = Span(start, end)
  Ok(#(Table(span, fields), tokens))
}

fn do_table(
  tokens: Tokens,
  acc: List(Field),
) -> Result(#(List(Field), lexer.Position, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(lexer.RightBrace, end), ..tokens] ->
      Ok(#(list.reverse(acc), end, tokens))
    [#(_, start), ..] -> {
      use #(field, tokens) <- result.try(table_field(tokens, start))
      case tokens {
        // table fields can be separeted by comas or semicolons
        [#(lexer.Comma, _), ..tokens] | [#(lexer.Semicolon, _), ..tokens] ->
          do_table(tokens, [field, ..acc])

        [#(lexer.RightBrace, end), ..tokens] ->
          Ok(#(list.reverse(acc), end, tokens))

        [#(unexpected, position), ..] ->
          Error(UnexpectedToken(unexpected, position))

        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn table_field(
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Field, Tokens), Error) {
  case tokens {
    [#(lexer.LeftSquare, _), ..tokens] -> {
      use #(key, tokens) <- result.try(expression(tokens))
      use _, tokens <- expect(lexer.RightSquare, tokens)
      use _, tokens <- expect(lexer.Equal, tokens)
      use #(value, tokens) <- result.try(expression(tokens))

      let span = Span(start, value.location.end)
      Ok(#(RecordField(span, key, value), tokens))
    }

    [#(lexer.Identifier(name), identifier_start), ..rest] -> {
      case rest {
        [#(lexer.Equal, _), ..tokens] -> {
          use #(value, tokens) <- result.try(expression(tokens))

          let str = LiteralString(string_span(identifier_start, name), name)

          let span = Span(start, value.location.end)
          Ok(#(RecordField(span, str, value), tokens))
        }

        _ -> {
          use #(value, tokens) <- result.try(expression(tokens))

          let span = Span(start, value.location.end)
          Ok(#(ListField(span, value), tokens))
        }
      }
    }

    [] -> Error(UnexpectedEndOfInput)
    _ -> {
      use #(expression, tokens) <- result.try(expression(tokens))

      let span = Span(start, expression.location.end)
      Ok(#(ListField(span, expression), tokens))
    }
  }
}

fn parameter(tokens: Tokens) -> Result(#(Parameter, Tokens), Error) {
  case tokens {
    [#(lexer.Identifier(name), start), ..tokens] ->
      Ok(#(NamedParameter(string_span(start, name), name), tokens))
    [#(lexer.DotDotDot, start), ..tokens] ->
      Ok(#(VariadicParameter(token_span(start, lexer.DotDotDot)), tokens))
    [#(unexpected, position), ..] ->
      Error(UnexpectedToken(unexpected, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn identifier(
  tokens: Tokens,
) -> Result(#(String, lexer.Position, Tokens), Error) {
  case tokens {
    [#(lexer.Identifier(name), position), ..tokens] ->
      Ok(#(name, position, tokens))
    [#(unexpected, position), ..] ->
      Error(UnexpectedToken(unexpected, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn identifier_list(
  tokens: Tokens,
  acc: List(String),
) -> Result(#(List(String), lexer.Position, Tokens), Error) {
  use #(name, start, tokens) <- result.try(identifier(tokens))
  case tokens {
    [#(lexer.Comma, _), ..tokens] -> identifier_list(tokens, [name, ..acc])
    [] -> Error(UnexpectedEndOfInput)
    _ -> Ok(#(list.reverse(acc), string_offset(start, name), tokens))
  }
}

fn unary_operation(
  operator: UnaryOperator,
  tokens: Tokens,
  start: lexer.Position,
) -> Result(#(Expression, Tokens), Error) {
  let bp = unary_operator_binding(operator)
  use #(parsed, tokens) <- result.try(do_expression(tokens, bp))

  let span = Span(start, parsed.location.end)
  Ok(#(UnaryOperation(span, operator, parsed), tokens))
}

fn unary_operator_binding(operator: UnaryOperator) -> Int {
  case operator {
    NumeralNegation -> 21
    BooleanNegation | BitwiseNegation | Length -> 22
  }
}

fn binary_operation(
  operator: BinaryOperator,
  tokens: Tokens,
  start: lexer.Position,
  prefix: Expression,
  minimun_binding: Int,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(lexer.EndOfFile, _), ..] -> Ok(#(prefix, tokens))

    [_operator, ..rest] ->
      case binary_operator_binding(operator) {
        #(l_bp, r_bp) if l_bp >= minimun_binding -> {
          use #(infix, tokens) <- result.try(do_expression(rest, r_bp))

          let span = Span(start, infix.location.end)
          let expr = BinaryOperation(span, operator, prefix, infix)
          bind_expression(expr, tokens, minimun_binding)
        }

        _ -> Ok(#(prefix, tokens))
      }
  }
}

fn binary_operator_binding(operator: BinaryOperator) -> #(Int, Int) {
  case operator {
    BooleanOr -> #(1, 2)
    BooleanAnd -> #(3, 4)
    Lt | Gt | LtEq | GtEq | NotEq | Eq -> #(5, 6)
    BitwiseOr -> #(7, 8)
    BitwiseXor -> #(9, 10)
    BitwiseAnd -> #(11, 12)
    BitwiseShiftLeft | BitwiseShiftRight -> #(13, 14)
    Concat -> #(15, 16)
    Add | Sub -> #(17, 18)
    Mult | Div | FloorDiv | Mod -> #(19, 20)
    Pow -> #(24, 23)
  }
}

fn expect(
  expected: lexer.Token,
  tokens: Tokens,
  next: fn(lexer.Position, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [#(token, start), ..rest] if token == expected -> next(start, rest)
    [#(unexpected, position), ..] ->
      Error(UnexpectedToken(unexpected, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn expression_list(
  tokens: Tokens,
  acc: List(Expression),
) -> Result(#(List(Expression), lexer.Position, Tokens), Error) {
  use #(parsed, tokens) <- result.try(expression(tokens))
  case tokens {
    [#(lexer.Comma, _), ..tokens] -> expression_list(tokens, [parsed, ..acc])
    [] -> Error(UnexpectedEndOfInput)
    _ -> Ok(#(list.reverse([parsed, ..acc]), parsed.location.end, tokens))
  }
}

fn comma_delimited_until(
  end: lexer.Token,
  tokens: Tokens,
  parse: fn(Tokens) -> Result(#(a, Tokens), Error),
  next: fn(List(a), lexer.Position, Tokens) -> Result(b, Error),
) -> Result(b, Error) {
  use #(parsed, position, tokens) <- result.try(
    do_comma_delimited_until(end, tokens, parse, []),
  )
  next(parsed, position, tokens)
}

fn do_comma_delimited_until(
  end: lexer.Token,
  tokens: Tokens,
  parse: fn(Tokens) -> Result(#(t, Tokens), Error),
  acc: List(t),
) -> Result(#(List(t), lexer.Position, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(token, start), ..tokens] if token == end ->
      Ok(#(list.reverse(acc), token_offset(start, token), tokens))
    _ -> {
      use #(parsed, tokens) <- result.try(parse(tokens))
      case tokens {
        [#(lexer.Comma, _), ..tokens] ->
          do_comma_delimited_until(end, tokens, parse, [parsed, ..acc])
        [#(token, start), ..tokens] if token == end ->
          Ok(#(
            list.reverse([parsed, ..acc]),
            token_offset(start, token),
            tokens,
          ))

        [#(unexpected, position), ..] ->
          Error(UnexpectedToken(unexpected, position))
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn token_offset(start: lexer.Position, token: lexer.Token) -> lexer.Position {
  let to_add = lexer.token_to_string(token) |> string.byte_size

  lexer.Position(
    offset: to_add + start.offset,
    column: to_add + start.column,
    line: start.line,
  )
}

fn token_span(start: lexer.Position, token: lexer.Token) -> Span {
  Span(start, token_offset(start, token))
}

fn string_offset(start: lexer.Position, string: String) -> lexer.Position {
  let to_add = string.byte_size(string)

  lexer.Position(
    offset: to_add + start.offset,
    column: to_add + start.column,
    line: start.line,
  )
}

fn string_span(start: lexer.Position, string: String) -> Span {
  Span(start, string_offset(start, string))
}
