//// Compiler for Lua 5.4

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lug/parser

pub opaque type Chunk {
  Chunk(
    name: String,
    source: option.Option(String),
    instructions: List(Instruction),
    prototypes: List(Chunk),
    upvals: List(Upval),
    constant_registers: List(AttributeRegister),
    is_vararg: Bool,
    span: Span,
  )
}

pub type Instruction {
  // move instructions
  Move(destination: Int, source: Int)
  LoadConstant(destination: Int, value: Constant)
  Getupvalue(destination: Int, index: Int)
  Setupvalue(value: Int, index: Int)

  // table instructions
  NewTable(destination: Int, hash_count: Int, array_count: Int)
  GetTable(destination: Int, table: Int, index: Int)
  GetTableUp(destination: Int, upvalue: Int, index: Int)
  SetTable(table: Int, key: Int, value: Int)
  SetTableUp(index: Int, key: Constant, value: Int)
  SetField(table: Int, key: Constant, value: Int)
  SetList(table: Int, from: Int, length: Int)
  SetListMulti(table: Int, from: Int, length: Int)

  // function instructions
  Vararg(start: Int, args: Int)
  Call(base: Int, arg_count: Int, result_count: Int)
  Return(start: Int, result_count: Int)
  ReturnMulti(start: Int, result_count: Int)
  Closure(destination: Int, prototype: Int)

  // branching instructions
  Test(condition: Int, then: List(Instruction), else_: List(Instruction))
  Label(name: String)
  Goto(to: String)

  // looping instructions
  Loop(
    condition: Int,
    condition_instructions: List(Instruction),
    eval_condition_before_block: Bool,
    body: List(Instruction),
  )
  ForLoop(start: Int, identifier: Int, body: List(Instruction))
  TForLoop(start: Int, identifier: Int, count: Int, body: List(Instruction))
  Break

  // arithmetic instructions
  Add(destination: Int, left: Int, right: Int)
  Sub(destination: Int, left: Int, right: Int)
  Mul(destination: Int, left: Int, right: Int)
  Div(destination: Int, left: Int, right: Int)
  FloorDiv(destination: Int, left: Int, right: Int)
  Pow(destination: Int, left: Int, right: Int)
  Mod(destination: Int, left: Int, right: Int)

  // equality instructions
  Eq(destination: Int, left: Int, right: Int)
  NotEq(destination: Int, left: Int, right: Int)
  Gt(destination: Int, left: Int, right: Int)
  GtEq(destination: Int, left: Int, right: Int)
  Lt(destination: Int, left: Int, right: Int)
  LtEq(destination: Int, left: Int, right: Int)

  // bitwise instructions
  BwAnd(destination: Int, left: Int, right: Int)
  BwOr(destination: Int, left: Int, right: Int)
  BwXor(destination: Int, left: Int, right: Int)
  BwNot(destination: Int, source: Int)
  ShiftLeft(destination: Int, left: Int, right: Int)
  ShiftRight(destination: Int, left: Int, right: Int)

  // boolean instructions
  And(destination: Int, left: Int, right: Int)
  Or(destination: Int, left: Int, right: Int)
  Not(destination: Int, source: Int)

  Negate(destination: Int, value: Int)
  Length(destination: Int, value: Int)
  Concat(destination: Int, left: Int, right: Int)
}

pub type Upval {
  Upval(index: Int, name: String)
}

pub type Constant {
  Nil
  Bool(Bool)
  String(String)
  Int(Int)
  Float(Float)
}

pub type Span {
  Span(start: Int, end: Int)
}

pub type Context {
  Context(
    vars: dict.Dict(String, Variable),
    statement_bindings: dict.Dict(#(String, parser.Span), Int),
    constant_registers: List(AttributeRegister),
    next_register: Int,
    next_global: Int,
    prototypes: List(Chunk),
  )
}

pub type Variable {
  Local(Int)
  Upvalue(Int)
  Global(Int)
}

pub type AttributeRegister {
  Const(Int)
  Close(Int)
}

pub fn compile(code: String) -> Result(Chunk, parser.Error) {
  parser.parse(code) |> result.map(compile_ast)
}

pub fn compile_ast(ast: parser.Block) -> Chunk {
  do_compile(ast, init_context(ast), init_chunk(ast))
}

fn do_compile(ast: parser.Block, context: Context, chunk: Chunk) -> Chunk {
  let #(context, instructions) = block(ast, context)
  Chunk(..chunk, instructions:, constant_registers: context.constant_registers)
}

fn init_context(ast: parser.Block) -> Context {
  Context(
    vars: dict.new(),
    statement_bindings: dict.new(),
    constant_registers: [],
    next_register: 1,
    next_global: 0,
    prototypes: [],
  )
  |> scan_block(ast)
}

fn scan_block(context: Context, ast: parser.Block) -> Context {
  list.fold(ast.statements, context, scan_statement)
}

fn scan_statement(context: Context, statement: parser.Statement) -> Context {
  case statement {
    parser.Assignment(_, names, expressions) ->
      list.fold(names, context, scan_expression)
      |> list.fold(expressions, _, scan_expression)
    parser.FunctionCall(_, function, args) ->
      scan_expression(context, function) |> list.fold(args, _, scan_expression)
    parser.Label(_, _) -> context
    parser.Break(_) -> context
    parser.Goto(_, _) -> context
    parser.Do(_, block) -> scan_block(context, block)
    parser.While(_, condition, block) ->
      scan_expression(context, condition) |> scan_block(block)
    parser.Repeat(_, condition, block) ->
      scan_block(context, block) |> scan_expression(condition)
    parser.If(_, condition, then, else_if, else_) -> {
      let context =
        scan_expression(context, condition)
        |> scan_block(then)
        |> list.fold(else_if, _, fn(context, else_if) {
          let parser.Elseif(_, condition, block) = else_if

          scan_expression(context, condition)
          |> scan_block(block)
        })

      case else_ {
        option.None -> context
        option.Some(block) -> scan_block(context, block)
      }
    }
    parser.For(span, identifier, init, limit, step, body) -> {
      let context =
        Context(
          ..context,
          statement_bindings: dict.insert(
            context.statement_bindings,
            #(identifier, span),
            context.next_register,
          ),
          next_register: context.next_register + 1,
        )
      scan_expression(context, init)
      |> scan_expression(limit)

      case step {
        option.None -> context
        option.Some(step) -> scan_expression(context, step)
      }
      |> scan_block(body)
    }
    parser.ForIn(span, names, in, body) -> {
      list.fold(in, context, scan_expression)
      |> list.fold(names, _, fn(context, name) {
        Context(
          ..context,
          statement_bindings: dict.insert(
            context.statement_bindings,
            #(name, span),
            context.next_register,
          ),
          next_register: context.next_register + 1,
        )
      })
      |> scan_block(body)
    }
    parser.FunctionDeclaration(_, name, parameters, body) -> todo
    parser.LocalFunction(_, name, parameters, body) -> todo
    parser.Local(_, names, values) ->
      list.fold(values, context, scan_expression)
      |> list.fold(names, _, fn(context, name) {
        let parser.AttributeName(attribute, name) = name
        let destination = context.next_register
        let context =
          Context(
            ..context,
            next_register: context.next_register + 1,
            vars: dict.insert(context.vars, name, Local(destination)),
          )

        case attribute {
          option.None -> context
          option.Some(parser.Const) ->
            Context(..context, constant_registers: [
              Const(destination),
              ..context.constant_registers
            ])
          option.Some(parser.Close) ->
            Context(..context, constant_registers: [
              Close(destination),
              ..context.constant_registers
            ])
        }
      })
    parser.Return(_, expressions) ->
      list.fold(expressions, context, scan_expression)
  }
}

fn scan_expression(context: Context, expr: parser.Expression) -> Context {
  case expr {
    parser.BooleanNil(_) -> context
    parser.BooleanFalse(_) -> context
    parser.BooleanTrue(_) -> context
    parser.Numeral(_, _) -> context
    parser.LiteralString(_, _) -> context
    parser.Variable(_, name) ->
      case dict.get(context.vars, name) {
        Ok(_) -> context
        Error(_) ->
          Context(
            ..context,
            next_global: context.next_global + 1,
            vars: dict.insert(context.vars, name, Global(context.next_global)),
          )
      }
    parser.Vararg(_) -> context
    parser.Function(_, parameters, body) ->
      scan_function(context, parameters, body)
    parser.Call(_, function, args) ->
      scan_expression(context, function) |> list.fold(args, _, scan_expression)
    parser.Table(_, fields) -> list.fold(fields, context, scan_field)
    parser.Index(_, table, key) ->
      scan_expression(context, table) |> scan_expression(key)
    parser.BinaryOperation(_, _, lhs, rhs) ->
      scan_expression(context, lhs)
      |> scan_expression(rhs)
    parser.UnaryOperation(_, _, expr) -> scan_expression(context, expr)
  }
}

fn scan_function(
  context: Context,
  parameters: List(parser.Parameter),
  body: parser.Block,
) -> Context {
  todo
}

fn scan_field(context: Context, field: parser.Field) -> Context {
  case field {
    parser.ListField(_, expr) -> scan_expression(context, expr)
    parser.RecordField(_, key, value) ->
      scan_expression(context, key) |> scan_expression(value)
  }
}

fn init_chunk(ast: parser.Block) -> Chunk {
  Chunk(
    name: "main",
    source: option.None,
    instructions: [],
    prototypes: [],
    upvals: [],
    constant_registers: [],
    is_vararg: True,
    span: Span(ast.location.start.line, ast.location.end.line),
  )
}

fn block(block: parser.Block, context: Context) {
  do_block(block.statements, context, [])
}

fn do_block(
  statements: List(parser.Statement),
  context: Context,
  acc: List(Instruction),
) -> #(Context, List(Instruction)) {
  case statements {
    [stmt, ..rest] -> {
      let #(context, instructions) = statement(stmt, context)
      do_block(rest, context, list.append(instructions, acc))
    }
    [] -> #(context, list.reverse(acc))
  }
}

fn statement(
  stmt: parser.Statement,
  context: Context,
) -> #(Context, List(Instruction)) {
  case stmt {
    parser.Assignment(_, names, expressions) ->
      assignment(names, expressions, context)
    parser.FunctionCall(_, name, args) -> {
      let #(context, _, instructions) = call(name, args, context)
      #(context, instructions)
    }
    parser.Label(_, name) -> #(context, [Label(name)])
    parser.Break(_) -> #(context, [Break])
    parser.Goto(_, label) -> #(context, [Goto(label)])
    parser.Do(_, do_block) -> block(do_block, context)
    parser.While(_, condition, body) -> while(condition, body, context)
    parser.Repeat(_, condition, body) -> repeat(condition, body, context)
    parser.If(_, condition, then, else_if, else_) ->
      if_(condition, then, else_if, else_, context)
    parser.For(span, identifier, init, limit, step, body) ->
      for(identifier, init, limit, step, body, span, context)
    parser.ForIn(span, names, in, body) ->
      for_in(names, in, body, span, context)
    parser.FunctionDeclaration(_, name, parameters, body) ->
      function_declaration(name, parameters, body, context)
    parser.LocalFunction(_, name, parameters, body) ->
      local_function(name, parameters, body, context)
    parser.Local(_, names, expressions) -> local(names, expressions, context)
    parser.Return(_, values) -> return(values, context)
  }
}

fn assignment(
  names: List(parser.Expression),
  expressions: List(parser.Expression),
  context: Context,
) -> #(Context, List(Instruction)) {
  case names, expressions {
    [name], [value] -> {
      let #(context, value_register, value_instructions) =
        expression(value, context)

      let #(context, name_instructions) =
        do_assignment(name, value_register, context)

      #(context, list.append(name_instructions, value_instructions))
    }

    _, _ -> {
      let #(final, values) = case list.reverse(expressions) {
        [parser.Vararg(_) as final, ..values] -> #(option.Some(final), values)
        [parser.Call(_, _, _) as final, ..values] -> #(
          option.Some(final),
          values,
        )
        _ -> #(option.None, expressions)
      }

      let #(context, values_registers, values_instructions) =
        list.fold(values, #(context, [], []), fn(acc, value) {
          let #(context, registers, generated) = acc
          let #(context, value_register, value_instructions) =
            expression(value, context)
          #(
            context,
            [value_register, ..registers],
            list.append(value_instructions, generated),
          )
        })

      let values_register = list.reverse(values_registers)

      let names_count = list.length(names)
      let values_count = list.length(values_registers)

      let names_and_values = case names_count == values_count {
        True -> list.zip(names, values_registers)
        False -> {
          let diff = names_count - values_count
          let values_registers =
            list.append(values_register, list.repeat(-1, diff))

          list.zip(names, values_registers)
        }
      }

      let #(context, values_instructions) = case final {
        option.Some(parser.Vararg(_)) -> {
          let diff = names_count - values_count
          #(Context(..context, next_register: context.next_register + diff), [
            Vararg(context.next_register, diff),
            ..values_instructions
          ])
        }
        option.Some(parser.Call(_, _, _) as call) -> {
          case expression(call, context) {
            #(
              context,
              call_register,
              [Call(_, _, _) as call_instruction, ..rest],
            ) -> {
              let diff = names_count - values_count
              let context =
                Context(..context, next_register: call_register + diff)

              #(context, [Call(..call_instruction, result_count: diff), ..rest])
            }
            #(context, _, call_instructions) -> #(context, call_instructions)
          }
        }
        _ -> #(context, values_instructions)
      }

      let #(context, assign_instructions) =
        list.fold(names_and_values, #(context, []), fn(acc, pair) {
          let #(context, generated) = acc
          case pair {
            #(name, -1) -> {
              let start = context.next_register
              let context = advance(context)
              let #(context, name_instructions) =
                do_assignment(name, start, context)

              #(
                context,
                [LoadConstant(start, Nil), ..name_instructions]
                  |> list.append(generated),
              )
            }

            #(name, value_register) -> {
              let #(context, name_instructions) =
                do_assignment(name, value_register, context)

              #(context, list.append(name_instructions, generated))
            }
          }
        })

      #(context, list.append(assign_instructions, values_instructions))
    }
  }
}

fn do_assignment(
  name: parser.Expression,
  value_register: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  case name {
    parser.Variable(_, name) ->
      case dict.get(context.vars, name) {
        Ok(Local(var_register)) ->
          case var_register == value_register {
            True -> #(context, [])
            False -> #(context, [Move(var_register, value_register)])
          }
        Ok(Upvalue(var_register)) -> #(context, [
          Setupvalue(var_register, value_register),
        ])
        Ok(Global(_var_register)) -> #(context, [
          SetTableUp(0, String(name), value_register),
        ])
        Error(_) -> #(context, [])
      }
    parser.Index(_, table, key) -> {
      let #(context, table_register, table_instructions) =
        expression(table, context)

      case key {
        parser.LiteralString(_, name) -> #(context, [
          SetField(table_register, String(name), value_register),
          ..table_instructions
        ])
        _ -> {
          let #(context, key_register, key_instructions) =
            expression(key, context)

          #(
            context,
            [
              SetTable(table_register, key_register, value_register),
              ..key_instructions
            ]
              |> list.append(table_instructions),
          )
        }
      }
    }
    _ -> panic as "invalid assignment"
  }
}

fn while(
  condition: parser.Expression,
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, condition_register, condition_instructions) =
    expression(condition, context)
  let #(context, body_instructions) = block(body, context)

  #(context, [
    Loop(condition_register, condition_instructions, True, body_instructions),
  ])
}

fn repeat(
  condition: parser.Expression,
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, condition_register, condition_instructions) =
    expression(condition, context)
  let #(context, body_instructions) = block(body, context)

  #(context, [
    Loop(condition_register, condition_instructions, False, body_instructions),
  ])
}

fn if_(
  condition: parser.Expression,
  then: parser.Block,
  else_if_list: List(parser.Elseif),
  else_: option.Option(parser.Block),
  context: Context,
) -> #(Context, List(Instruction)) {
  case condition {
    parser.BooleanTrue(_) -> block(then, context)
    parser.BooleanFalse(_) | parser.BooleanNil(_) ->
      else_if(else_if_list, else_, context)
    _ -> {
      let #(context, condition_reg, condition_instructions) =
        expression(condition, context)

      let #(context, then_instructions) = block(then, context)
      let #(context, else_instructions) = else_if(else_if_list, else_, context)

      #(context, [
        Test(condition_reg, then_instructions, else_instructions),
        ..condition_instructions
      ])
    }
  }
}

fn else_if(
  else_if_list: List(parser.Elseif),
  else_: option.Option(parser.Block),
  context: Context,
) -> #(Context, List(Instruction)) {
  case else_if_list, else_ {
    [], option.None -> #(context, [])
    [], option.Some(else_block) -> block(else_block, context)
    [parser.Elseif(_, condition, else_if_block), ..rest], else_ -> {
      let #(context, condition_register, condition_instructions) =
        expression(condition, context)

      let #(context, else_if_block_instructions) = block(else_if_block, context)
      let #(context, else_instructions) = else_if(rest, else_, context)

      #(context, [
        Test(condition_register, else_if_block_instructions, else_instructions),
        ..condition_instructions
      ])
    }
  }
}

fn for(
  identifier: String,
  init: parser.Expression,
  limit: parser.Expression,
  step: option.Option(parser.Expression),
  body: parser.Block,
  span: parser.Span,
  context: Context,
) -> #(Context, List(Instruction)) {
  let assert Ok(identifier_register) =
    dict.get(context.statement_bindings, #(identifier, span))

  let start = context.next_register
  let context = Context(..context, next_register: start + 3)

  let #(context, init_register, init_instructions) = expression(init, context)
  let #(context, limit_register, limit_instructions) =
    expression(limit, context)
  let #(context, step_register, step_instructions) = case step {
    option.Some(step) -> expression(step, context)
    option.None -> #(advance(context), context.next_register, [
      LoadConstant(context.next_register, Int(1)),
    ])
  }

  let #(context, body) = block(body, context)

  let generated =
    [
      ForLoop(start, identifier_register, body),
      Move(start, init_register),
      Move(start + 1, limit_register),
      Move(start + 2, step_register),
    ]
    |> list.append(step_instructions)
    |> list.append(limit_instructions)
    |> list.append(init_instructions)

  #(context, generated)
}

fn for_in(
  names: List(String),
  in: List(parser.Expression),
  body: parser.Block,
  span: parser.Span,
  context: Context,
) -> #(Context, List(Instruction)) {
  let assert [first, ..] = names
  let assert Ok(names_start) =
    dict.get(context.statement_bindings, #(first, span))

  let names_count = list.length(names)

  let start = context.next_register
  let context = Context(..context, next_register: start + 3)

  let #(context, iterator_instructions) = case in {
    [in] -> {
      let #(context, _, iterator_instructions) = expression(in, context)
      #(context, iterator_instructions)
    }

    _ -> {
      let #(context, iterators_registers, iterators_instructions) =
        list.fold(in, #(context, [], []), fn(acc, iterator) {
          let #(context, registers, generated) = acc
          let #(context, iterator_register, iterator_instructions) =
            expression(iterator, context)

          #(
            context,
            [iterator_register, ..registers],
            list.append(generated, iterator_instructions),
          )
        })

      let move =
        list.reverse(iterators_registers)
        |> list.index_fold([], fn(acc, source, i) {
          let target = start + 1
          case source == target {
            True -> []
            False -> [Move(target, source), ..acc]
          }
        })
        |> list.reverse

      #(context, list.append(move, iterators_instructions))
    }
  }

  let #(context, body) = block(body, context)

  let generated = [
    TForLoop(start, names_start, names_count, body),
    ..iterator_instructions
  ]

  #(context, generated)
}

fn function_declaration(
  name: parser.FunctionName,
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, body_register, body_instructions) =
    closure(parameters, body, context)

  todo
}

fn local_function(
  name: parser.FunctionName,
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, body_register, body_instructions) =
    closure(parameters, body, context)

  todo
}

fn local(
  names: List(parser.AttributeName),
  expressions: List(parser.Expression),
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(final, values) = case list.reverse(expressions) {
    [parser.Vararg(_) as final, ..values] -> #(option.Some(final), values)
    [parser.Call(_, _, _) as final, ..values] -> #(option.Some(final), values)
    _ -> #(option.None, expressions)
  }

  let #(context, values_registers, values_instructions) =
    list.fold(values, #(context, [], []), fn(acc, value) {
      let #(context, registers, generated) = acc
      let #(context, value_register, value_instructions) =
        expression(value, context)
      #(
        context,
        [value_register, ..registers],
        list.append(value_instructions, generated),
      )
    })

  let values_register = list.reverse(values_registers)

  let names_count = list.length(names)
  let values_count = list.length(values_registers)

  let #(context, final_register, values_instructions) = case final {
    option.Some(parser.Vararg(_)) -> {
      let diff = names_count - values_count
      #(
        Context(..context, next_register: context.next_register + diff),
        context.next_register,
        [Vararg(context.next_register, diff), ..values_instructions],
      )
    }
    option.Some(parser.Call(_, _, _) as call) -> {
      case expression(call, context) {
        #(context, call_register, [Call(_, _, _) as call_instruction, ..rest]) -> {
          let diff = names_count - values_count
          let context = Context(..context, next_register: call_register + diff)

          #(context, call_register, [
            Call(..call_instruction, result_count: diff),
            ..rest
          ])
        }
        #(context, call_register, call_instructions) -> #(
          context,
          call_register,
          call_instructions,
        )
      }
    }
    _ -> #(context, 0, values_instructions)
  }

  let names_and_values = case names_count == values_count {
    True -> list.zip(names, values_registers)
    False -> {
      let diff = names_count - values_count
      let values_registers = list.append(values_register, list.repeat(-1, diff))

      list.zip(names, values_registers)
    }
  }

  let #(context, move_instructions) =
    list.fold(names_and_values, #(context, []), fn(acc, pair) {
      let #(context, generated) = acc
      let #(parser.AttributeName(_, name), index) = pair
      let assert Ok(Local(destination_register)) = dict.get(context.vars, name)

      case index {
        -1 ->
          case final {
            option.Some(_) -> #(context, [
              Move(
                destination_register,
                { index - values_count } + final_register,
              ),
            ])
            option.None -> #(context, [
              LoadConstant(destination_register, Nil),
              ..generated
            ])
          }
        index -> #(context, [Move(destination_register, index), ..generated])
      }
    })

  #(context, list.append(move_instructions, values_instructions))
}

fn return(
  values: List(parser.Expression),
  context: Context,
) -> #(Context, List(Instruction)) {
  case values {
    [] -> #(context, [Return(0, 0)])
    [parser.Vararg(_)] -> #(context, [
      ReturnMulti(context.next_register, 0),
      Vararg(context.next_register, 0),
    ])
    [parser.Call(_, _, _) as call] -> {
      let #(context, _, call_instructions) = expression(call, context)
      #(context, call_instructions)
    }
    [value] -> {
      let #(context, value_register, value_instructions) =
        expression(value, context)

      #(context, [Return(value_register, 1), ..value_instructions])
    }
    [_, ..] -> {
      let result_count = list.length(values)
      let #(final, values) = case list.reverse(values) {
        [parser.Vararg(_) as final, ..rest] -> #(
          option.Some(final),
          list.reverse(rest),
        )
        [parser.Call(_, _, _) as final, ..rest] -> #(
          option.Some(final),
          list.reverse(rest),
        )
        _ -> #(option.None, values)
      }

      let start = context.next_register
      let #(context, generated) =
        list.index_fold(values, #(context, []), fn(acc, value, i) {
          let #(context, instructions) = acc
          let target_register = start + i

          let #(context, value_register, value_instructions) =
            expression(value, context)

          case target_register == value_register {
            True -> #(context, list.append(value_instructions, instructions))
            False -> #(context, [
              Move(target_register, value_register),
              ..list.append(value_instructions, instructions)
            ])
          }
        })

      case final {
        option.Some(parser.Vararg(_)) -> {
          #(context, [
            ReturnMulti(start, result_count),
            Vararg(start + result_count - 1, 0),
            ..generated
          ])
        }
        option.Some(parser.Call(_, _, _) as call) -> {
          let #(context, _, call_instructions) =
            expression(
              call,
              Context(..context, next_register: start + result_count),
            )
          #(
            context,
            [ReturnMulti(start, result_count), ..call_instructions]
              |> list.append(generated),
          )
        }
        _ -> #(context, [Return(start, result_count), ..generated])
      }
    }
  }
}

fn expression(
  expr: parser.Expression,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  case expr {
    parser.BooleanTrue(_) -> #(advance(context), context.next_register, [
      LoadConstant(context.next_register, Bool(True)),
    ])
    parser.BooleanFalse(_) -> #(advance(context), context.next_register, [
      LoadConstant(context.next_register, Bool(False)),
    ])
    parser.BooleanNil(_) -> #(context, context.next_register, [
      LoadConstant(context.next_register, Nil),
    ])
    parser.Numeral(_, number) -> {
      let number = parse_number(number)
      #(advance(context), context.next_register, [
        LoadConstant(context.next_register, number),
      ])
    }
    parser.LiteralString(_, string) -> {
      #(advance(context), context.next_register, [
        LoadConstant(context.next_register, String(string)),
      ])
    }
    parser.Variable(_, var) -> variable(var, context)
    parser.Vararg(_) -> #(advance(context), context.next_register, [
      Vararg(context.next_register, 1),
    ])
    parser.Function(_, parameters, body) -> closure(parameters, body, context)
    parser.Call(_, function, args) -> call(function, args, context)
    parser.Table(_, fields) -> table(fields, context)
    parser.Index(_, table, key) -> index(table, key, context)
    parser.BinaryOperation(_, operator, lhs, rhs) ->
      binary_operation(operator, lhs, rhs, context)
    parser.UnaryOperation(_, operator, expr) ->
      unary_operation(operator, expr, context)
  }
}

fn variable(
  var: String,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  case dict.get(context.vars, var) {
    Ok(Local(var_register)) -> #(context, var_register, [])
    Ok(Upvalue(var_register)) -> #(advance(context), context.next_register, [
      Getupvalue(context.next_register, var_register),
    ])
    Ok(Global(var_register)) -> #(advance(context), context.next_register, [
      GetTableUp(context.next_register, 0, var_register),
    ])
    Error(_) -> #(advance(context), context.next_register, [
      LoadConstant(context.next_register, Nil),
    ])
  }
}

fn closure(
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  todo
}

fn call(
  expr: parser.Expression,
  args: List(parser.Expression),
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let start = context.next_register
  let #(context, function_register, function_instructions) =
    expression(expr, context)

  let function_instructions = case function_register == start {
    True -> function_instructions
    False -> [Move(start, function_register)]
  }

  let args = list.reverse(args)
  let args_count = list.length(args)
  let context = Context(..context, next_register: start + args_count + 1)

  case args {
    [parser.Vararg(_), ..args] -> {
      let #(context, args_instructions) = arguments(args, start + 1, context)

      let vararg_instruction = Vararg(start + args_count + 1, 0)
      let generated =
        [Call(start, args_count, 1), vararg_instruction, ..args_instructions]
        |> list.append(function_instructions)

      #(context, start, generated)
    }
    [parser.Call(_, _, _) as nested_call, ..args] -> {
      let #(_, args_instructions) = arguments(args, start + 1, context)

      let #(context, _, nested_call_instructions) =
        expression(nested_call, context)
      let generated =
        [Call(start, args_count, 1), ..nested_call_instructions]
        |> list.append(args_instructions)
        |> list.append(function_instructions)

      #(context, start, generated)
    }
    _ -> {
      let #(context, args_instructions) = arguments(args, start + 1, context)

      let generated =
        [Call(start, args_count, 1), ..args_instructions]
        |> list.append(function_instructions)

      #(context, start, generated)
    }
  }
}

fn arguments(
  args: List(parser.Expression),
  start: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, args_instructions, args_registers) =
    list.fold(args, #(context, [], []), fn(acc, arg) {
      let #(context, instructions, registers) = acc
      let #(context, arg_register, arg_instructions) = expression(arg, context)

      #(context, list.append(arg_instructions, instructions), [
        arg_register,
        ..registers
      ])
    })

  let generated =
    list.append(
      list.index_fold(list.reverse(args_registers), [], fn(acc, register, i) {
        let target = start + i
        case target == register {
          True -> acc
          False -> [Move(target, register), ..acc]
        }
      }),
      args_instructions,
    )

  #(context, generated)
}

fn table(
  fields: List(parser.Field),
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let #(hash_part, array_part) =
    list.fold(fields, #([], []), fn(acc, field) {
      let #(hash_part, array_part) = acc
      case field {
        parser.RecordField(_, key, value) -> #(
          [#(key, value), ..hash_part],
          array_part,
        )
        parser.ListField(_, value) -> #(hash_part, [value, ..array_part])
      }
    })

  let hash_part = list.reverse(hash_part)
  let array_part = list.reverse(array_part)

  let hash_count = list.length(hash_part)
  let array_count = list.length(array_part)

  let destination_register = context.next_register
  let generated = [
    NewTable(context.next_register, hash_count, array_count),
  ]

  let start = context.next_register + 1
  let #(final, values) = case list.reverse(array_part) {
    [parser.Vararg(_) as final, ..values] -> #(
      option.Some(final),
      list.reverse(values),
    )
    [parser.Call(_, _, _) as final, ..values] -> #(
      option.Some(final),
      list.reverse(values),
    )
    _ -> #(option.None, array_part)
  }

  let array_count = case final {
    option.Some(_) -> list.length(values)
    option.None -> array_count
  }

  let #(context, generated) =
    list.index_fold(
      values,
      #(Context(..context, next_register: start + array_count), generated),
      fn(acc, value, i) {
        let #(context, instructions) = acc
        let target_register = start + i

        let #(context, value_register, value_instructions) =
          expression(value, context)

        case target_register == value_register {
          True -> #(context, list.append(value_instructions, instructions))
          False -> #(context, [
            Move(target_register, value_register),
            ..list.append(value_instructions, instructions)
          ])
        }
      },
    )

  let #(context, generated) = case final {
    option.Some(parser.Vararg(_)) -> #(context, [
      SetListMulti(destination_register, start, array_count),
      Vararg(start + array_count, 0),
      ..generated
    ])

    option.Some(parser.Call(_, _, _) as call) -> {
      let #(context, _, call_instructions) =
        expression(call, Context(..context, next_register: start + array_count))

      let generated =
        [
          SetListMulti(destination_register, start, array_count),
          ..call_instructions
        ]
        |> list.append(generated)

      #(context, generated)
    }
    _ -> {
      let generated = case generated {
        [] -> generated
        _ -> [SetList(destination_register, start, array_count), ..generated]
      }

      #(context, generated)
    }
  }

  let #(context, generated) =
    list.fold(hash_part, #(context, generated), fn(acc, field) {
      let #(context, instructions) = acc
      let #(key, value) = field

      let #(context, value_register, value_instructions) =
        expression(value, context)
      let #(context, key_instructions) = case key {
        parser.LiteralString(_, name) -> #(context, [
          SetField(destination_register, String(name), value_register),
        ])
        _ -> {
          let #(context, key_register, key_instructions) =
            expression(key, context)
          #(context, [
            SetTable(destination_register, key_register, value_register),
            ..key_instructions
          ])
        }
      }

      #(
        context,
        list.append(key_instructions, value_instructions)
          |> list.append(instructions),
      )
    })

  #(context, destination_register, generated)
}

fn index(
  table: parser.Expression,
  key: parser.Expression,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let #(context, table_register, table_instructions) =
    expression(table, context)
  let #(context, key_register, key_instructions) = expression(key, context)

  let generated =
    list.append(table_instructions, key_instructions)
    |> list.append([
      GetTable(context.next_register, table_register, key_register),
    ])

  #(advance(context), context.next_register, generated)
}

fn binary_operation(
  operator: parser.BinaryOperator,
  lhs: parser.Expression,
  rhs: parser.Expression,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let #(context, lhs_register, lhs_instructions) = expression(lhs, context)
  let #(context, rhs_register, rhs_instructions) = expression(rhs, context)

  let operation_instruction = case operator {
    parser.Add -> Add(context.next_register, lhs_register, rhs_register)
    parser.Sub -> Sub(context.next_register, lhs_register, rhs_register)
    parser.Mult -> Mul(context.next_register, lhs_register, rhs_register)
    parser.Div -> Div(context.next_register, lhs_register, rhs_register)
    parser.FloorDiv ->
      FloorDiv(context.next_register, lhs_register, rhs_register)
    parser.Pow -> Pow(context.next_register, lhs_register, rhs_register)
    parser.Mod -> Mod(context.next_register, lhs_register, rhs_register)
    parser.Eq -> Eq(context.next_register, lhs_register, rhs_register)
    parser.NotEq -> NotEq(context.next_register, lhs_register, rhs_register)
    parser.Gt -> Gt(context.next_register, lhs_register, rhs_register)
    parser.GtEq -> GtEq(context.next_register, lhs_register, rhs_register)
    parser.Lt -> Lt(context.next_register, lhs_register, rhs_register)
    parser.LtEq -> LtEq(context.next_register, lhs_register, rhs_register)
    parser.BitwiseAnd ->
      BwAnd(context.next_register, lhs_register, rhs_register)
    parser.BitwiseOr -> BwOr(context.next_register, lhs_register, rhs_register)
    parser.BitwiseXor ->
      BwXor(context.next_register, lhs_register, rhs_register)
    parser.BitwiseShiftLeft ->
      ShiftLeft(context.next_register, lhs_register, rhs_register)
    parser.BitwiseShiftRight ->
      ShiftRight(context.next_register, lhs_register, rhs_register)
    parser.BooleanAnd -> And(context.next_register, lhs_register, rhs_register)
    parser.BooleanOr -> Or(context.next_register, lhs_register, rhs_register)
    parser.Concat -> Concat(context.next_register, lhs_register, rhs_register)
  }

  let generated =
    list.append([operation_instruction, ..rhs_instructions], lhs_instructions)

  #(advance(context), context.next_register, generated)
}

fn unary_operation(
  operator: parser.UnaryOperator,
  expr: parser.Expression,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let #(context, expr_reg, body_instructions) = expression(expr, context)
  let operation_instruction = case operator {
    parser.NumeralNegation -> Negate(context.next_register, expr_reg)
    parser.BooleanNegation -> Not(context.next_register, expr_reg)
    parser.BitwiseNegation -> BwNot(context.next_register, expr_reg)
    parser.Length -> Length(context.next_register, expr_reg)
  }

  #(advance(context), context.next_register, [
    operation_instruction,
    ..body_instructions
  ])
}

fn parse_number(number: String) -> Constant {
  case number {
    // hexadecimal number
    "0x" <> number | "0X" <> number -> {
      let #(number, float_part, radix) = parse_hexadecimal_number(number)
      let assert Ok(number) = int.base_parse(number, 16)

      case float_part, radix {
        option.None, option.None -> Int(number)
        option.None, option.Some(radix) ->
          Float(radix_point(int.to_float(number), radix))
        option.Some(float_part), option.None ->
          Float(hexadecimal_float(int.to_float(number), float_part))
        option.Some(float_part), option.Some(radix) ->
          hexadecimal_float(int.to_float(number), float_part)
          |> radix_point(radix)
          |> Float
      }
    }

    // decimal number
    _ -> {
      case string.contains(number, ".") {
        True -> {
          let assert Ok(number) = float.parse(number)
          Float(number)
        }
        False ->
          case string.split_once(number, "e") {
            Ok(#(number, exponent)) -> {
              let assert Ok(number) = int.parse(number)
              Float(exponential(int.to_float(number), exponent))
            }
            Error(_) ->
              case string.split_once(number, "E") {
                Ok(#(number, exponent)) -> {
                  let assert Ok(number) = int.parse(number)
                  Float(exponential(int.to_float(number), exponent))
                }
                Error(_) -> {
                  let assert Ok(number) = int.parse(number)
                  Int(number)
                }
              }
          }
      }
    }
  }
}

fn exponential(number: Float, exponent: String) -> Float {
  let assert Ok(exponent) = int.parse(exponent) |> result.map(int.to_float)
  let assert Ok(times) = int.power(10, exponent)

  number *. times
}

fn radix_point(number: Float, radix: String) -> Float {
  let assert Ok(radix) = int.parse(radix) |> result.map(int.to_float)
  let assert Ok(times) = int.power(2, radix)

  number *. times
}

fn hexadecimal_float(number: Float, float_part: String) -> Float {
  list.index_fold(string.to_graphemes(float_part), number, fn(acc, digit, i) {
    let assert Ok(digit) = int.base_parse(digit, 16) |> result.map(int.to_float)
    let assert Ok(by) = int.power(16, int.to_float(i + 1))

    acc +. digit /. by
  })
}

fn parse_hexadecimal_number(
  number: String,
) -> #(String, option.Option(String), option.Option(String)) {
  let #(number, float_part) = case string.split_once(number, ".") {
    Ok(#(number, float_part)) -> #(number, option.Some(float_part))
    Error(_) -> #(number, option.None)
  }

  case float_part {
    option.None ->
      case string.split_once(number, "p") {
        Ok(#(number, radix)) -> #(number, float_part, option.Some(radix))
        Error(_) ->
          case string.split_once(number, "P") {
            Ok(#(number, radix)) -> #(number, float_part, option.Some(radix))
            Error(_) -> #(number, float_part, option.None)
          }
      }
    option.Some(float_part) ->
      case string.split_once(float_part, "p") {
        Ok(#(float_part, radix)) -> #(
          number,
          option.Some(float_part),
          option.Some(radix),
        )
        Error(_) ->
          case string.split_once(number, "P") {
            Ok(#(number, radix)) -> #(
              number,
              option.Some(float_part),
              option.Some(radix),
            )
            Error(_) -> #(number, option.Some(float_part), option.None)
          }
      }
  }
}

fn advance(context: Context) -> Context {
  Context(..context, next_register: context.next_register + 1)
}
