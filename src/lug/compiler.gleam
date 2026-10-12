//// Compiler for Lua 5.4

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import lug/lexer
import lug/parser

pub opaque type Chunk {
  Chunk(
    name: option.Option(String),
    source: option.Option(String),
    instructions: List(Instruction),
    prototypes: List(Chunk),
    upvals: dict.Dict(String, Int),
    constant_registers: List(AttributeRegister),
    parameters: Parameters,
    span: Span,
  )
}

pub type Instruction {
  // move instructions
  Move(destination: Int, source: Int)
  LoadConstant(destination: Int, value: Constant)
  GetGlobal(destination: Int, key: Constant)
  GetUpvalue(destination: Int, index: Int)
  SetGlobal(key: Constant, value: Int)
  SetUpvalue(index: Int, value: Int)

  // table instructions
  NewTable(destination: Int, hash_count: Int, array_count: Int)
  GetTable(destination: Int, table: Int, key: Int)
  GetField(table: Int, destination: Int, key: Constant)
  GetTableUp(destination: Int, table: Int, index: Int)
  SetTable(table: Int, key: Int, value: Int)
  SetTableUp(index: Int, key: Int, value: Int)
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

pub type Constant {
  Nil
  Bool(Bool)
  String(String)
  Int(Int)
  Float(Float)
}

pub type Parameters {
  Parameters(count: Int, has_vararg: Bool)
}

pub type Span {
  Span(start: Int, end: Int)
}

pub type Context {
  Context(
    scopes: dict.Dict(Int, Scope),
    constant_registers: List(AttributeRegister),
    next_register: Int,
    next_global: Int,
    next_scope: Int,
    parent_scope: Int,
    prototypes: List(Chunk),
  )
}

pub type Scope {
  Scope(vars: dict.Dict(String, Variable), parent: Int)
}

pub type Variable {
  Global(Int)
  Local(Int)
  Upvalue(Int)
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
  Chunk(
    ..chunk,
    instructions:,
    prototypes: context.prototypes,
    constant_registers: context.constant_registers,
  )
}

type ScanMode {
  Env
  Normal
  Function
}

fn init_context(ast: parser.Block) -> Context {
  let initial =
    Context(
      scopes: dict.new(),
      constant_registers: [],
      next_register: 1,
      next_global: 0,
      next_scope: 0,
      parent_scope: -1,
      prototypes: [],
    )
    |> scan_block(ast, Normal)

  // reset scope count to 0 so we traverse the scope in the same order when compiling
  Context(..initial, next_scope: 0)
}

fn scan_block(context: Context, ast: parser.Block, mode: ScanMode) -> Context {
  let scope = context.next_scope
  let context =
    Context(
      ..context,
      next_scope: context.next_scope + 1,
      scopes: dict.insert(
        context.scopes,
        scope,
        Scope(dict.new(), context.parent_scope),
      ),
      parent_scope: scope,
    )
    |> list.fold(ast.statements, _, fn(context, statement) {
      scan_statement(context, statement, mode)
    })

  Context(..context, parent_scope: context.parent_scope - 1)
}

fn scan_statement(
  context: Context,
  statement: parser.Statement,
  mode: ScanMode,
) -> Context {
  let sc_expr = fn(context, statement) {
    scan_expression(context, statement, mode)
  }

  case statement {
    parser.Assignment(_, names, expressions) ->
      list.fold(names, context, fn(context, expr) {
        scan_expression(context, expr, Env)
      })
      |> list.fold(expressions, _, sc_expr)
    parser.FunctionCall(_, function, args) ->
      sc_expr(context, function)
      |> list.fold(args, _, sc_expr)
    parser.Label(_, _) -> context
    parser.Break(_) -> context
    parser.Goto(_, _) -> context
    parser.Do(_, block) -> scan_block(context, block, mode)
    parser.While(_, condition, block) ->
      sc_expr(context, condition) |> scan_block(block, mode)
    parser.Repeat(_, condition, block) ->
      sc_expr(context, condition) |> scan_block(block, mode)
    parser.If(_, condition, then, else_if, else_) -> {
      let context =
        sc_expr(context, condition)
        |> scan_block(then, mode)
        |> list.fold(else_if, _, fn(context, else_if) {
          let parser.Elseif(_, condition, block) = else_if

          sc_expr(context, condition)
          |> scan_block(block, mode)
        })

      case else_ {
        option.None -> context
        option.Some(block) -> scan_block(context, block, mode)
      }
    }
    parser.For(_, identifier, init, limit, step, body) -> {
      let context =
        sc_expr(context, init)
        |> sc_expr(limit)

      let context = case step {
        option.None -> context
        option.Some(step) -> sc_expr(context, step)
      }

      let scope =
        Scope(
          dict.from_list([#(identifier, Local(context.next_register))]),
          context.parent_scope,
        )

      Context(
        ..context,
        scopes: dict.insert(context.scopes, context.next_scope, scope),
        next_register: context.next_register + 1,
      )
      |> scan_block(body, mode)
    }
    parser.ForIn(_, names, in, body) -> {
      let context =
        Context(
          ..context,
          scopes: dict.insert(
            context.scopes,
            context.next_scope,
            Scope(dict.new(), context.parent_scope),
          ),
        )

      list.fold(in, context, sc_expr)
      |> list.fold(names, _, fn(context, name) {
        insert_variable_in_scope(context, context.next_scope, name, Local)
      })
      |> scan_block(body, mode)
    }
    parser.FunctionDeclaration(_, name, parameters, body) -> {
      let context =
        Context(
          ..context,
          scopes: dict.insert(
            context.scopes,
            context.next_scope,
            Scope(dict.new(), context.parent_scope),
          ),
        )

      let parser.FunctionName(name, _, method) = name
      let context = scan_variable(context, name, Env)

      case method {
        option.Some(_) ->
          insert_variable_in_scope(context, context.next_scope, "self", Local)
        option.None -> context
      }
      |> list.fold(parameters, _, scan_parameter)
      |> scan_block(body, Function)
    }
    parser.LocalFunction(_, name, parameters, body) -> {
      let context =
        Context(
          ..context,
          scopes: dict.insert(
            context.scopes,
            context.next_scope,
            Scope(dict.new(), context.parent_scope),
          ),
        )

      insert_variable_in_scope(context, context.next_scope - 1, name, Local)
      |> list.fold(parameters, _, scan_parameter)
      |> scan_block(body, Function)
    }
    parser.Local(_, names, values) ->
      list.fold(values, context, sc_expr)
      |> list.fold(names, _, fn(context, name) {
        let parser.AttributeName(attribute, name) = name
        let destination = context.next_register
        let context =
          insert_variable_in_scope(context, context.next_scope - 1, name, Local)

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
    parser.Return(_, expressions) -> list.fold(expressions, context, sc_expr)
  }
}

fn scan_expression(
  context: Context,
  expr: parser.Expression,
  mode: ScanMode,
) -> Context {
  case expr {
    parser.BooleanNil(_) -> context
    parser.BooleanFalse(_) -> context
    parser.BooleanTrue(_) -> context
    parser.Numeral(_, _) -> context
    parser.LiteralString(_, _) -> context
    parser.Variable(_, name) -> scan_variable(context, name, mode)
    parser.Vararg(_) -> context
    parser.Function(_, parameters, body) ->
      list.fold(parameters, context, scan_parameter)
      |> scan_block(body, Function)
    parser.Call(_, function, args) ->
      scan_expression(context, function, mode)
      |> list.fold(args, _, fn(context, expression) {
        scan_expression(context, expression, mode)
      })
    parser.Table(_, fields) ->
      list.fold(fields, context, fn(context, expr) {
        scan_field(context, expr, mode)
      })
    parser.Index(_, table, key) ->
      scan_expression(context, table, mode) |> scan_expression(key, mode)
    parser.BinaryOperation(_, _, lhs, rhs) ->
      scan_expression(context, lhs, mode)
      |> scan_expression(rhs, mode)
    parser.UnaryOperation(_, _, expr) -> scan_expression(context, expr, mode)
  }
}

fn scan_variable(context: Context, name: String, mode: ScanMode) -> Context {
  let scope = get_current_scope(context)
  case dict.get(scope.vars, name) {
    Ok(_) -> context
    Error(_) ->
      case find_variable_in_parent_scopes(context, name, scope) {
        option.Some(_) -> context
        option.None -> {
          let scope_index = context.next_scope - 1

          case mode, scope_index {
            Env, 0 -> {
              let assert Ok(scope) = dict.get(context.scopes, scope_index)

              Context(
                ..context,
                scopes: dict.insert(
                  context.scopes,
                  scope_index,
                  Scope(
                    ..scope,
                    vars: dict.insert(
                      scope.vars,
                      name,
                      Global(context.next_register),
                    ),
                  ),
                ),
              )
            }
            Env, _ | Normal, _ ->
              insert_variable_in_scope(context, scope_index, name, Local)
            Function, _ ->
              insert_variable_in_scope(context, scope_index, name, Upvalue)
          }
        }
      }
  }
}

fn scan_parameter(context: Context, parameter: parser.Parameter) -> Context {
  case parameter {
    parser.NamedParameter(_, name) ->
      insert_variable_in_scope(context, context.next_scope, name, Local)
    parser.VariadicParameter(_) -> context
  }
}

fn scan_field(
  context: Context,
  field: parser.Field,
  mode: ScanMode,
) -> Context {
  case field {
    parser.ListField(_, expr) -> scan_expression(context, expr, mode)
    parser.RecordField(_, key, value) ->
      scan_expression(context, key, mode) |> scan_expression(value, mode)
  }
}

fn insert_variable_in_scope(
  context: Context,
  scope_index: Int,
  name: String,
  variable: fn(Int) -> Variable,
) -> Context {
  let assert Ok(scope) = dict.get(context.scopes, scope_index)

  Context(
    ..context,
    next_register: context.next_register + 1,
    scopes: dict.insert(
      context.scopes,
      scope_index,
      Scope(
        ..scope,
        vars: dict.insert(scope.vars, name, variable(context.next_register)),
      ),
    ),
  )
}

fn find_variable_in_parent_scopes(
  context: Context,
  var: String,
  scope: Scope,
) -> option.Option(Variable) {
  case scope.parent {
    // scope with parent -1 is the parent scope
    -1 -> option.None
    parent -> {
      let assert Ok(parent_scope) = dict.get(context.scopes, parent)
      case dict.get(parent_scope.vars, var) {
        Ok(var) -> option.Some(var)
        Error(_) -> find_variable_in_parent_scopes(context, var, parent_scope)
      }
    }
  }
}

fn init_chunk(ast: parser.Block) -> Chunk {
  Chunk(
    name: option.Some("main"),
    source: option.None,
    instructions: [],
    prototypes: [],
    upvals: dict.new(),
    constant_registers: [],
    parameters: Parameters(0, True),
    span: Span(ast.location.start.line, ast.location.end.line),
  )
}

fn block(block: parser.Block, context: Context) {
  do_block(
    block.statements,
    Context(..context, next_scope: context.next_scope + 1),
    [],
  )
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
    parser.Do(_, do_block) ->
      block(do_block, context) |> pair.map_second(list.reverse)
    parser.While(_, condition, body) -> while(condition, body, context)
    parser.Repeat(_, condition, body) -> repeat(condition, body, context)
    parser.If(_, condition, then, else_if, else_) ->
      if_(condition, then, else_if, else_, context)
    parser.For(_, identifier, init, limit, step, body) ->
      for(identifier, init, limit, step, body, context)
    parser.ForIn(_, names, in, body) -> for_in(names, in, body, context)
    parser.FunctionDeclaration(span, name, parameters, body) ->
      function_declaration(span, name, parameters, body, context)
    parser.LocalFunction(span, name, parameters, body) ->
      local_function(span, name, parameters, body, context)
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
  let scope = get_current_scope(context)

  case name {
    parser.Variable(_, name) ->
      case dict.get(scope.vars, name) {
        Ok(Global(var_register)) -> #(context, [
          SetTableUp(0, var_register, value_register),
        ])
        Ok(Local(var_register)) ->
          case var_register == value_register {
            True -> #(context, [])
            False -> #(context, [Move(var_register, value_register)])
          }
        Ok(Upvalue(var_register)) -> #(context, [
          SetUpvalue(var_register, value_register),
        ])
        Error(_) ->
          case find_variable_in_parent_scopes(context, name, scope) {
            option.Some(Global(var_register)) -> #(context, [
              SetTableUp(0, var_register, value_register),
            ])
            option.Some(Local(var_register)) ->
              case var_register == value_register {
                True -> #(context, [])
                False -> #(context, [Move(var_register, value_register)])
              }
            option.Some(Upvalue(var_register)) -> #(context, [
              SetUpvalue(var_register, value_register),
            ])
            option.None -> #(context, [])
          }
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
  let condition_instructions = list.reverse(condition_instructions)

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

  // negate the condition
  let condition_instructions =
    list.reverse([
      Not(context.next_register, condition_register),
      ..condition_instructions
    ])
  let condition_register = context.next_register
  let context = advance(context)

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
    parser.BooleanTrue(_) ->
      block(then, context) |> pair.map_second(list.reverse)
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
  context: Context,
) -> #(Context, List(Instruction)) {
  let scope = get_current_scope(context)
  let assert Ok(Local(identifier_register)) = dict.get(scope.vars, identifier)

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
  context: Context,
) -> #(Context, List(Instruction)) {
  let assert [first, ..] = names

  let scope = get_current_scope(context)
  let assert Ok(Local(names_start)) = dict.get(scope.vars, first)
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
        |> list.fold([], fn(acc, source) {
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
  span: parser.Span,
  name: parser.FunctionName,
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
  let scope = get_current_scope(context)
  let #(name, subfields, parameters) = case name {
    parser.FunctionName(name, subfields, option.Some(method)) -> #(
      name,
      list.append(subfields, [method]),
      [
        parser.NamedParameter(
          // dummy span
          parser.Span(lexer.Position(0, 0, 0), lexer.Position(0, 0, 0)),
          "self",
        ),
        ..parameters
      ],
    )
    parser.FunctionName(name, subfields, option.None) -> #(
      name,
      subfields,
      parameters,
    )
  }
  let fn_name =
    list.fold(subfields, name, fn(acc, subfield) { acc <> "." <> subfield })
    |> option.Some

  let #(context, closure_register, closure_instructions) =
    closure(span, fn_name, parameters, body, context)

  case name, subfields {
    name, [] -> {
      let scope = get_current_scope(context)
      let generated = case dict.get(scope.vars, name) {
        Ok(Global(destination)) -> SetTableUp(0, destination, closure_register)
        Ok(Local(destination)) -> Move(destination, closure_register)
        Ok(Upvalue(destination)) -> SetUpvalue(destination, closure_register)
        Error(_) ->
          case find_variable_in_parent_scopes(context, name, scope) {
            option.Some(Global(destination)) ->
              SetTableUp(0, destination, closure_register)
            option.Some(Local(destination)) ->
              Move(destination, closure_register)
            option.Some(Upvalue(destination)) ->
              SetUpvalue(destination, closure_register)
            _ -> panic
          }
      }

      #(context, [generated, ..closure_instructions])
    }
    root, subfields -> {
      let start = context.next_register
      let scope = get_current_scope(context)
      let init = case dict.get(scope.vars, root) {
        Ok(Global(destination)) -> SetTableUp(0, destination, start)
        Ok(Local(destination)) -> Move(destination, start)
        Ok(Upvalue(destination)) -> SetUpvalue(destination, start)
        Error(_) ->
          case find_variable_in_parent_scopes(context, name, scope) {
            option.Some(Global(destination)) ->
              SetTableUp(0, destination, start)
            option.Some(Local(destination)) -> Move(destination, start)
            option.Some(Upvalue(destination)) -> SetUpvalue(destination, start)
            _ -> panic
          }
      }
      let #(getters, end_register, context) =
        list.fold(subfields, #([init], start, advance(context)), fn(acc, field) {
          let #(getters, current, context) = acc
          #(
            [GetField(context.next_register, current, String(field)), ..getters],
            context.next_register,
            advance(context),
          )
        })

      let assert [end_field, ..] = list.reverse(subfields)
      #(
        context,
        [SetField(end_register, String(end_field), closure_register), ..getters]
          |> list.append(closure_instructions),
      )
    }
  }
}

fn local_function(
  span: parser.Span,
  name: String,
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
  let scope = get_current_scope(context)
  let assert Ok(Local(destination)) = dict.get(scope.vars, name)

  let #(context, closure_register, closure_instructions) =
    closure(span, option.Some(name), parameters, body, context)

  #(context, [Move(destination, closure_register), ..closure_instructions])
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
      let scope = get_current_scope(context)
      let assert Ok(Local(destination_register)) = dict.get(scope.vars, name)

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
    parser.Function(span, parameters, body) ->
      closure(span, option.None, parameters, body, context)
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
  let scope = get_current_scope(context)

  case dict.get(scope.vars, var) {
    Ok(Global(var_register)) -> #(advance(context), context.next_register, [
      GetTableUp(context.next_register, 0, var_register),
    ])
    Ok(Local(var_register)) -> #(context, var_register, [])
    Ok(Upvalue(var_register)) -> #(advance(context), context.next_register, [
      GetUpvalue(context.next_register, var_register),
    ])
    Error(_) ->
      case find_variable_in_parent_scopes(context, var, scope) {
        option.Some(Global(var_register)) -> #(advance(context), var_register, [
          GetTableUp(context.next_register, 0, var_register),
        ])
        option.Some(Local(var_register)) -> #(context, var_register, [])
        option.Some(Upvalue(var_register)) -> #(advance(context), var_register, [
          GetUpvalue(context.next_register, var_register),
        ])
        option.None -> #(advance(context), context.next_register, [
          LoadConstant(context.next_register, Nil),
        ])
      }
  }
}

fn closure(
  span: parser.Span,
  name: option.Option(String),
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let prototypes = context.prototypes
  let constant_registers = context.constant_registers

  let assert Ok(scope) = dict.get(context.scopes, context.next_scope)
  let upvals =
    dict.fold(scope.vars, dict.new(), fn(acc, name, var) {
      case var {
        Upvalue(register) -> dict.insert(acc, name, register)
        _ -> acc
      }
    })

  let #(context, instructions): #(Context, List(Instruction)) =
    block(body, Context(..context, prototypes: [], constant_registers: []))

  let arg_count =
    list.count(parameters, fn(parameter) {
      case parameter {
        parser.VariadicParameter(_) -> False
        _ -> True
      }
    })

  let has_vararg =
    list.any(parameters, fn(parameter) {
      case parameter {
        parser.VariadicParameter(_) -> True
        _ -> False
      }
    })

  let prototype =
    Chunk(
      name:,
      instructions:,
      source: option.None,
      prototypes: context.prototypes,
      upvals:,
      constant_registers: context.constant_registers,
      parameters: Parameters(arg_count, has_vararg),
      span: Span(span.start.line, span.end.line),
    )

  let context =
    Context(
      ..context,
      constant_registers:,
      prototypes: list.append(prototypes, [prototype]),
    )

  #(advance(context), context.next_register, [
    Closure(context.next_register, list.length(context.prototypes)),
  ])
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
    list.append(
      [
        GetTable(context.next_register, table_register, key_register),
        ..key_instructions
      ],
      table_instructions,
    )

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

  let generated = [
    operation_instruction,
    ..list.append(rhs_instructions, lhs_instructions)
  ]
  echo generated

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

fn get_current_scope(context: Context) -> Scope {
  let assert Ok(scope) = dict.get(context.scopes, context.next_scope - 1)
  scope
}

pub fn chunk_to_string(chunk: Chunk) -> String {
  let name = case chunk.name {
    option.Some(name) -> name
    option.None -> ""
  }

  let initial =
    "function "
    <> name
    <> "("
    <> string.join(list.repeat("_", chunk.parameters.count), ", ")

  case chunk.parameters.has_vararg {
    True -> initial <> "..."
    False -> initial
  }
  <> ")"
  <> " --from line "
  <> int.to_string(chunk.span.start)
  <> " to line "
  <> int.to_string(chunk.span.end)
  <> "\n"
  |> list.fold(chunk.instructions, _, fn(acc, instruction) {
    let #(instruction, _) = instruction_to_string(instruction, 1)

    acc <> "    " <> instruction <> "\n"
  })
  <> "end"
  |> list.fold(chunk.prototypes, _, fn(acc, prototype) {
    acc <> "\n\n" <> chunk_to_string(prototype)
  })
}

fn instruction_to_string(
  instruction: Instruction,
  identation: Int,
) -> #(String, Int) {
  case instruction {
    Move(destination, source) -> #(
      "MOVE "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "from: "
        <> int.to_string(source),
      identation,
    )
    LoadConstant(destination, constant) -> #(
      "LOADCONSTANT "
        <> "in: "
        <> int.to_string(destination)
        <> ", "
        <> "constant: "
        <> constant_to_string(constant),
      identation,
    )
    GetGlobal(destination, key) -> #(
      "GETGLOBAL "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "key: "
        <> constant_to_string(key),
      identation,
    )
    GetUpvalue(destination, index) -> #(
      "GETUPVALUE "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "upvalue: "
        <> int.to_string(index),
      identation,
    )
    SetGlobal(key, value) -> #(
      "SETGLOBAL"
        <> "field: "
        <> constant_to_string(key)
        <> ", "
        <> "to: "
        <> int.to_string(value),
      identation,
    )
    SetUpvalue(value, index) -> #(
      "SETUPVALUE "
        <> "value: "
        <> int.to_string(value)
        <> ", "
        <> "upvalue: "
        <> int.to_string(index),
      identation,
    )
    NewTable(destination, hash_count, array_count) -> #(
      "NEWTABLE "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "hash_count: "
        <> int.to_string(hash_count)
        <> ", "
        <> "array_count: "
        <> int.to_string(array_count),
      identation,
    )
    GetTable(destination, table, index) -> #(
      "GETTABLE "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "index: "
        <> int.to_string(index),
      identation,
    )
    GetTableUp(destination, table, index) -> #(
      "GETTABLEUP "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "upvalue: "
        <> int.to_string(index),
      identation,
    )
    GetField(table, destination, key) -> #(
      "GETFIELD "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "key: "
        <> constant_to_string(key),
      identation,
    )
    SetTable(table, key, value) -> #(
      "SETTABLE "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "key: "
        <> int.to_string(key)
        <> ", "
        <> "value: "
        <> int.to_string(value),
      identation,
    )
    SetTableUp(index, key, value) -> #(
      "SETTABLEUP "
        <> "upvalue: "
        <> int.to_string(index)
        <> ", "
        <> "key: "
        <> int.to_string(key)
        <> ", "
        <> "value: "
        <> int.to_string(value),
      identation,
    )
    SetField(table, key, value) -> #(
      "SETFIELD "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "key: "
        <> constant_to_string(key)
        <> ", "
        <> "value: "
        <> int.to_string(value),
      identation,
    )
    SetList(table, from, length) -> #(
      "SETLIST "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "starting: "
        <> int.to_string(from)
        <> ", "
        <> "length: "
        <> int.to_string(length),
      identation,
    )
    SetListMulti(table, from, length) -> #(
      "SETLISTMULTI "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "starting: "
        <> int.to_string(from)
        <> ", "
        <> "length: "
        <> int.to_string(length),
      identation,
    )
    Vararg(start, args) -> #(
      "VARARG "
        <> "starting: "
        <> int.to_string(start)
        <> ", "
        <> "args: "
        <> int.to_string(args),
      identation,
    )
    Call(base, arg_count, result_count) -> #(
      "CALL "
        <> "function: "
        <> int.to_string(base)
        <> ", "
        <> "arg_count: "
        <> int.to_string(arg_count)
        <> ", "
        <> "result_count: "
        <> int.to_string(result_count),
      identation,
    )
    Return(start, result_count) -> #(
      "RETURN "
        <> "starting: "
        <> int.to_string(start)
        <> ", "
        <> "count: "
        <> int.to_string(result_count),
      identation,
    )
    ReturnMulti(start, result_count) -> #(
      "RETURNMULTI "
        <> "starting: "
        <> int.to_string(start)
        <> ", "
        <> "count: "
        <> int.to_string(result_count),
      identation,
    )
    Closure(destination, prototype) -> #(
      "CLOSURE "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "prototype_index: "
        <> int.to_string(prototype),
      identation,
    )

    Test(condition, then, else_) -> {
      let base =
        "if has_truthy: "
        <> int.to_string(condition)
        <> " then\n"
        <> list.fold(then, "", fn(acc, instruction) {
          let #(instruction, _) =
            instruction_to_string(instruction, identation + 1)

          acc <> string.repeat("    ", identation + 1) <> instruction <> "\n"
        })

      case else_ {
        [] -> #(base <> string.repeat("    ", identation) <> "end", identation)
        _ -> {
          let else_ =
            string.repeat("    ", identation)
            <> "else\n"
            <> list.fold(else_, "", fn(acc, instruction) {
              let #(instruction, _) =
                instruction_to_string(instruction, identation + 1)

              acc
              <> string.repeat("    ", identation + 1)
              <> instruction
              <> "\n"
            })

          #(
            base <> else_ <> string.repeat("    ", identation) <> "end",
            identation,
          )
        }
      }
    }
    Label(label) -> #(label <> ":", identation)
    Goto(label) -> #("GOTO " <> "to: " <> label, identation)
    Loop(condition, condition_instructions, eval_condition_before_block, body) -> {
      let condition_body =
        list.fold(condition_instructions, "", fn(acc, instruction) {
          let #(instruction, _) =
            instruction_to_string(instruction, identation + 1)

          acc <> string.repeat("    ", identation + 1) <> instruction <> "\n"
        })

      let body =
        list.fold(body, "", fn(acc, instruction) {
          let #(instruction, _) =
            instruction_to_string(instruction, identation + 1)

          acc <> string.repeat("    ", identation + 1) <> instruction <> "\n"
        })

      let body = case eval_condition_before_block {
        True -> condition_body <> body
        False -> body <> condition_body
      }

      #(
        "loop\n"
          <> body
          <> string.repeat("    ", identation)
          <> "until has_truthy: "
          <> int.to_string(condition),
        identation,
      )
    }
    ForLoop(start, identifier, body) -> {
      let body =
        list.fold(body, "", fn(acc, instruction) {
          let #(instruction, _) =
            instruction_to_string(instruction, identation + 1)

          acc <> string.repeat("    ", identation + 1) <> instruction <> "\n"
        })

      #(
        "for identifier: "
          <> int.to_string(identifier)
          <> ", init: "
          <> int.to_string(start)
          <> ", step: "
          <> int.to_string(start + 1)
          <> ", limit: "
          <> int.to_string(start + 2)
          <> " loop\n"
          <> body
          <> string.repeat("    ", identation)
          <> "end",
        identation,
      )
    }
    TForLoop(start, identifier, count, body) -> {
      let body =
        list.fold(body, "", fn(acc, instruction) {
          let #(instruction, _) =
            instruction_to_string(instruction, identation + 1)

          acc <> string.repeat("    ", identation + 1) <> instruction <> "\n"
        })

      let names =
        int.range(from: 0, to: count, with: [], run: fn(acc, i) {
          [int.to_string(identifier + i + 1), ..acc]
        })
        |> list.reverse
        |> string.join(", ")

      let iterators =
        int.range(from: 0, to: count, with: [], run: fn(acc, i) {
          [int.to_string(start + i + 1), ..acc]
        })
        |> list.reverse
        |> string.join(", ")

      #(
        "for "
          <> names
          <> " in "
          <> iterators
          <> " loop\n"
          <> body
          <> string.repeat("    ", identation)
          <> "end",
        identation,
      )
    }
    Break -> #("BREAK", identation)
    Add(destination, lhs, rhs) -> #(
      "ADD "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Sub(destination, lhs, rhs) -> #(
      "SUB "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Mul(destination, lhs, rhs) -> #(
      "MUL "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Div(destination, lhs, rhs) -> #(
      "DIV "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    FloorDiv(destination, lhs, rhs) -> #(
      "FLOORDIV "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Pow(destination, lhs, rhs) -> #(
      "Pow "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Mod(destination, lhs, rhs) -> #(
      "MOD "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Eq(destination, lhs, rhs) -> #(
      "EQ "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    NotEq(destination, lhs, rhs) -> #(
      "NOTEQ "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Gt(destination, lhs, rhs) -> #(
      "GT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    GtEq(destination, lhs, rhs) -> #(
      "GTEQ "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Lt(destination, lhs, rhs) -> #(
      "LT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    LtEq(destination, lhs, rhs) -> #(
      "LTEQ "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    BwAnd(destination, lhs, rhs) -> #(
      "BWAND "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    BwOr(destination, lhs, rhs) -> #(
      "BWOR "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    BwXor(destination, lhs, rhs) -> #(
      "BWXOR "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    BwNot(destination, value) -> #(
      "BWNOT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "not: "
        <> int.to_string(value),
      identation,
    )
    ShiftLeft(destination, lhs, rhs) -> #(
      "SHIFTLEFT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    ShiftRight(destination, lhs, rhs) -> #(
      "SHIFTRIGHT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    And(destination, lhs, rhs) -> #(
      "AND "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Or(destination, lhs, rhs) -> #(
      "OR "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
    Not(destination, value) -> #(
      "NOT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "not: "
        <> int.to_string(value),
      identation,
    )
    Negate(destination, value) -> #(
      "NEGATE "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "negate: "
        <> int.to_string(value),
      identation,
    )
    Length(destination, value) -> #(
      "LENGTH "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "of: "
        <> int.to_string(value),
      identation,
    )
    Concat(destination, lhs, rhs) -> #(
      "CONCAT "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> ", "
        <> "right: "
        <> int.to_string(rhs),
      identation,
    )
  }
}

fn constant_to_string(constant: Constant) -> String {
  case constant {
    Nil -> "nil"
    Bool(True) -> "true"
    Bool(False) -> "false"
    String(s) -> "\"" <> s <> "\""
    Int(n) -> int.to_string(n)
    Float(n) -> float.to_string(n)
  }
}
