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

pub type Chunk {
  Chunk(
    name: option.Option(String),
    source: option.Option(String),
    instructions: List(Instruction),
    prototypes: dict.Dict(Int, Chunk),
    upvalues: dict.Dict(Int, Int),
    arg_count: Int,
    is_vararg: Bool,
    span: Span,
  )
}

pub type Instruction {
  // move instructions
  Move(destination: Int, source: Int)
  LoadConstant(destination: Int, value: Constant)
  GetGlobal(destination: Int, name: String)
  SetGlobal(name: String, value: Int)
  GetUpvalue(destination: Int, index: Int)
  SetUpvalue(index: Int, value: Int)

  // table instructions
  NewTable(destination: Int, hash_count: Int, array_count: Int)
  GetTable(destination: Int, table: Int, key: Int)
  SetTable(table: Int, key: Int, value: Int)
  SetList(table: Int, length: Int)

  // function instructions
  VarargPrep(arg_count: Int)
  Call(base: Int, arg_count: Int, result_count: Int)
  Return(start: Int, result_count: Int)
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
  ForTest(destination: Int, left: Int, right: Int, by: Int)
  TForLoop(start: Int, count: Int, body: List(Instruction))
  Break

  // arithmetic instructions
  Add(destination: Int, left: Int, right: Int)
  Sub(destination: Int, left: Int, right: Int)
  Mul(destination: Int, left: Int, right: Int)
  Div(destination: Int, left: Int, right: Int)
  FloorDiv(destination: Int, left: Int, right: Int)
  Pow(destination: Int, left: Int, right: Int)
  Mod(destination: Int, left: Int, right: Int)
  Unm(destination: Int, value: Int)

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
    next_register: Int,
    next_upvalue: Int,
    next_prototype: Int,
    next_scope: Int,
    parent_scope: Int,
    prototypes: dict.Dict(Int, Chunk),
    upvalues: dict.Dict(Int, String),
  )
}

pub type Scope {
  Scope(vars: dict.Dict(String, Variable), parent: Int, kind: ScopeKind)
}

pub type ScopeKind {
  Block
  Function
}

pub type Variable {
  Local(register: Int)
  Upvalue(register: Int)
}

pub fn compile(code: String) -> Result(Chunk, parser.Error) {
  parser.parse(code) |> result.map(compile_ast)
}

pub fn compile_ast(ast: parser.Block) -> Chunk {
  do_compile(ast, init_context(), init_chunk(ast))
}

fn do_compile(ast: parser.Block, context: Context, chunk: Chunk) -> Chunk {
  let #(context, instructions) = block(ast, context)

  Chunk(..chunk, instructions:, prototypes: context.prototypes)
}

fn init_context() -> Context {
  Context(
    scopes: dict.new(),
    next_register: 1,
    next_upvalue: 0,
    next_prototype: 0,
    next_scope: 0,
    parent_scope: -1,
    prototypes: dict.new(),
    upvalues: dict.new(),
  )
}

fn scan_local_variables(context: Context, block: parser.Block) -> Context {
  list.fold(block.statements, context, fn(context, stmt) {
    case stmt {
      parser.LocalFunction(_, name, _, _) ->
        insert_variable_in_scope(
          context,
          context.next_scope - 1,
          name,
          Local(context.next_register),
        )
        |> advance
      parser.Local(_, names, _) ->
        list.fold(names, context, fn(context, variable) {
          let parser.AttributeName(_attr, name) = variable
          let local_register = context.next_register
          insert_variable_in_scope(
            context,
            context.next_scope - 1,
            name,
            Local(local_register),
          )
          |> advance
        })
      _ -> context
    }
  })
}

fn insert_variable_in_scope(
  context: Context,
  scope_index: Int,
  name: String,
  variable: Variable,
) -> Context {
  let assert Ok(scope) = dict.get(context.scopes, scope_index)

  Context(
    ..context,
    scopes: dict.insert(
      context.scopes,
      scope_index,
      Scope(..scope, vars: dict.insert(scope.vars, name, variable)),
    ),
  )
}

fn find_variable_in_scope(
  context: Context,
  scope_index: Int,
  name: String,
) -> Result(Variable, Nil) {
  let assert Ok(scope) = dict.get(context.scopes, scope_index)
  dict.get(scope.vars, name)
}

fn find_variable_in_parent_scopes(
  context: Context,
  var: String,
  scope_index: Int,
) -> option.Option(Variable) {
  let assert Ok(scope) = dict.get(context.scopes, scope_index)
  case scope.parent {
    // scope with parent -1 is the upmost scope
    -1 -> option.None
    parent_index -> {
      let assert Ok(parent_scope) = dict.get(context.scopes, parent_index)
      case dict.get(parent_scope.vars, var) {
        Ok(var) -> option.Some(var)
        Error(_) -> find_variable_in_parent_scopes(context, var, parent_index)
      }
    }
  }
}

fn enter_block(
  context: Context,
  then: fn(Context) -> #(Context, List(Instruction)),
) -> #(Context, List(Instruction)) {
  let scope = context.next_scope
  let context =
    Context(
      ..context,
      next_scope: context.next_scope + 1,
      scopes: dict.insert(
        context.scopes,
        scope,
        Scope(dict.new(), context.parent_scope, Block),
      ),
      parent_scope: scope,
    )

  let #(context, generated) = then(context)
  #(
    Context(
      ..context,
      next_scope: context.next_scope - 1,
      parent_scope: context.parent_scope - 1,
    ),
    generated,
  )
}

fn enter_function(
  context: Context,
  then: fn(Context) -> #(Context, a),
) -> #(Context, a) {
  let saved_register = context.next_register
  let fun_scope = context.next_scope
  let context =
    Context(
      prototypes: dict.new(),
      next_prototype: 0,
      next_register: 1,
      next_upvalue: 0,
      next_scope: context.next_scope + 1,
      scopes: dict.insert(
        context.scopes,
        fun_scope,
        Scope(dict.new(), context.parent_scope, Function),
      ),
      parent_scope: fun_scope,
      upvalues: dict.new(),
    )

  let #(context, result) = then(context)

  #(
    Context(
      ..context,
      next_register: saved_register,
      next_scope: context.next_scope - 1,
      parent_scope: context.parent_scope - 1,
    ),
    result,
  )
}

fn init_chunk(ast: parser.Block) -> Chunk {
  Chunk(
    name: option.Some("main"),
    source: option.None,
    instructions: [],
    prototypes: dict.new(),
    upvalues: dict.new(),
    arg_count: 0,
    is_vararg: True,
    span: Span(ast.location.start.line, ast.location.end.line),
  )
}

fn block(block: parser.Block, context: Context) {
  use context <- enter_block(context)
  let #(context, instructions) =
    scan_local_variables(context, block)
    |> do_block(block.statements, _, [])

  #(context, instructions)
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
    parser.FunctionCall(_, name, args) ->
      call(name, args, context.next_register, advance(context))
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

      let #(context, assign_instructions) =
        do_assignment(name, value_register, context)

      #(context, list.append(assign_instructions, value_instructions))
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

      let names_count = list.length(names)
      let values_count = list.length(values)
      let diff = names_count - values_count

      let #(names, stray_names) =
        list.index_fold(names, #([], []), fn(acc, name, i) {
          case i + 1 <= values_count {
            True -> #([name, ..acc.0], acc.1)
            False -> #(acc.0, [name, ..acc.1])
          }
        })

      let names = list.reverse(names)
      let stray_names = list.reverse(stray_names)

      let #(context, generated) =
        list.zip(names, values)
        |> list.fold(#(context, []), fn(acc, pair) {
          let #(context, generated) = acc
          let #(name, value) = pair

          // for calls and varargs expressions in the middle,
          // use just one result/value from them
          let #(context, value_register, value_instructions) = case value {
            parser.Vararg(_) -> #(advance(context), context.next_register, [
              Vararg(context.next_register, 1),
            ])
            parser.Call(_, name, args) -> {
              let call_register = context.next_register
              let #(context, call_instructions) =
                call(name, args, call_register, advance(context))
              let call_instructions = case call_instructions {
                [Call(..) as call, ..rest] -> [
                  Call(..call, result_count: 1),
                  ..rest
                ]
                other -> other
              }

              #(context, call_register, call_instructions)
            }
            _ -> expression(value, context)
          }

          let #(context, assign_instructions) =
            do_assignment(name, value_register, context)

          let generated =
            list.append(assign_instructions, value_instructions)
            |> list.append(generated)

          #(context, generated)
        })

      case stray_names {
        [] -> #(context, generated)
        stray_names ->
          case final {
            option.Some(parser.Vararg(_)) -> {
              let vararg_destination = context.next_register
              let context = advance(context)
              let generated = [Vararg(vararg_destination, diff), ..generated]
              let #(context, generated) =
                list.index_fold(
                  stray_names,
                  #(context, generated),
                  fn(acc, name, i) {
                    let #(context, generated) = acc
                    let value_register = vararg_destination + i
                    let #(context, assign_instructions) =
                      do_assignment(name, value_register, context)

                    #(context, list.append(assign_instructions, generated))
                  },
                )

              #(context, generated)
            }
            option.Some(parser.Call(_, name, args)) -> {
              let call_destination = context.next_register
              let context = advance(context)
              let #(context, call_instructions) =
                call(name, args, call_destination, context)
              let call_instructions = case call_instructions {
                [Call(_, _, _) as call, ..rest] -> [
                  Call(..call, result_count: diff),
                  ..rest
                ]
                other -> other
              }
              let #(context, move) =
                list.index_fold(stray_names, #(context, []), fn(acc, name, i) {
                  let #(context, generated) = acc
                  let value_register = call_destination + i
                  let #(context, assign_instructions) =
                    do_assignment(name, value_register, context)

                  #(context, list.append(assign_instructions, generated))
                })
              let generated =
                list.append(move, call_instructions)
                |> list.append(generated)

              #(context, generated)
            }
            _ ->
              list.fold(stray_names, #(context, generated), fn(acc, name) {
                let #(context, generated) = acc
                let value_register = context.next_register
                let #(context, assign_instructions) =
                  do_assignment(name, value_register, advance(context))

                #(
                  context,
                  list.reverse([
                    LoadConstant(value_register, Nil),
                    ..assign_instructions
                  ])
                    |> list.append(generated),
                )
              })
          }
      }
    }
  }
}

fn do_assignment(
  name: parser.Expression,
  value_register: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let scope_index = context.next_scope - 1
  let scope = get_current_scope(context)

  case name {
    parser.Variable(_, name) ->
      case dict.get(scope.vars, name) {
        Ok(Local(destination)) ->
          case value_register == destination {
            True -> #(context, [])
            False -> #(context, [Move(destination, value_register)])
          }
        Ok(Upvalue(index)) -> #(context, [
          SetUpvalue(index, value_register),
        ])

        Error(_) ->
          case find_variable_in_parent_scopes(context, name, scope_index) {
            option.Some(Local(destination)) ->
              case scope.kind {
                Block ->
                  case value_register == destination {
                    True -> #(context, [])
                    False -> #(context, [Move(destination, value_register)])
                  }
                Function -> {
                  let index = context.next_upvalue
                  let context =
                    insert_variable_in_scope(
                      Context(..context, next_upvalue: index + 1),
                      context.next_scope - 1,
                      name,
                      Upvalue(destination),
                    )

                  #(context, [
                    SetUpvalue(index, value_register),
                  ])
                }
              }
            option.Some(Upvalue(var_register)) -> #(context, [
              SetUpvalue(var_register, value_register),
            ])
            option.None -> #(context, [
              SetGlobal(name, value_register),
            ])
          }
      }
    parser.Index(_, table, key) -> {
      let #(context, table_register, table_instructions) =
        expression(table, context)

      case key {
        parser.LiteralString(_, name) -> {
          #(advance(context), [
            SetTable(table_register, context.next_register, value_register),
            LoadConstant(context.next_register, String(name)),
            ..table_instructions
          ])
        }
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

      #(
        context,
        list.reverse([
          Test(
            condition_register,
            else_if_block_instructions,
            else_instructions,
          ),
          ..condition_instructions
        ]),
      )
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
  let identifier_register = context.next_register
  let condition_register = context.next_register + 1
  let context = Context(..context, next_register: context.next_register + 2)

  let #(context, init_register, init_instructions) = expression(init, context)
  let init_instructions = [
    Move(identifier_register, init_register),
    ..init_instructions
  ]

  let #(context, limit_register, limit_instructions) =
    expression(limit, context)
  let #(context, step_register, step_instructions) = case step {
    option.Some(step) -> expression(step, context)
    option.None -> #(advance(context), context.next_register, [
      LoadConstant(context.next_register, Int(1)),
    ])
  }

  let condition_instruction = [
    ForTest(
      condition_register,
      identifier_register,
      limit_register,
      step_register,
    ),
  ]

  let #(context, body) = {
    use context <- enter_block(context)
    insert_variable_in_scope(
      context,
      context.next_scope - 1,
      identifier,
      Local(identifier_register),
    )
    |> do_block(body.statements, _, [])
  }

  let body =
    [
      Add(identifier_register, identifier_register, step_register),
      ..list.reverse(body)
    ]
    |> list.reverse

  let generated =
    [
      Loop(condition_register, condition_instruction, True, body),
      ..step_instructions
    ]
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
  let start = context.next_register
  let context = Context(..context, next_register: start + 4)

  let #(final, iterators) = case list.reverse(in) {
    [parser.Vararg(_) as final, ..rest] -> #(
      option.Some(final),
      list.reverse(rest),
    )
    [parser.Call(_, _, _) as final, ..rest] -> #(
      option.Some(final),
      list.reverse(rest),
    )
    _ -> #(option.None, in)
  }

  let #(context, iterators_instructions) =
    list.index_fold(iterators, #(context, []), fn(acc, iterator, i) {
      let #(context, generated) = acc
      let target_register = start + i

      // for calls and varargs expressions in the middle,
      // use just one result/value from them
      let #(context, iterator_instructions) = case iterator {
        parser.Vararg(_) -> #(context, [
          Vararg(target_register, 1),
        ])
        parser.Call(_, name, args) ->
          case call(name, args, target_register, context) {
            #(context, [Call(_, _, _) as call, ..rest]) -> #(context, [
              Call(..call, result_count: 1),
              ..rest
            ])
            other -> other
          }
        _ -> expression_to(iterator, target_register, context)
      }

      #(context, list.append(generated, iterator_instructions))
    })

  let iterators_instructions = list.reverse(iterators_instructions)

  let #(context, generated) = case final {
    option.Some(parser.Vararg(_)) -> {
      let iterators_count = list.length(iterators)
      let diff = 4 - iterators_count
      let generated = [
        Vararg(start + iterators_count, diff),
        ..iterators_instructions
      ]

      #(context, generated)
    }
    option.Some(parser.Call(_, name, args)) -> {
      let iterators_count = list.length(iterators)
      let diff = 4 - iterators_count
      let #(context, call_instructions) =
        call(name, args, start + diff, context)

      let call_instructions =
        case call_instructions {
          [Call(..) as call, ..rest] ->
            [Call(..call, result_count: diff), ..rest] |> list.reverse
          _ -> call_instructions
        }
        |> list.reverse

      #(context, call_instructions)
    }
    _ -> {
      let iterators_count = list.length(iterators)
      let diff = 4 - iterators_count

      case diff {
        0 -> #(context, iterators_instructions)
        diff -> {
          let #(context, nils) =
            int.range(from: 0, to: diff, with: #(context, []), run: fn(acc, i) {
              let #(context, acc) = acc
              let target = start + iterators_count + i
              #(context, [LoadConstant(target, Nil), ..acc])
            })

          #(context, list.append(nils, iterators_instructions))
        }
      }
    }
  }

  let #(context, body) = {
    use context <- enter_block(context)
    list.fold(names, context, fn(context, name) {
      insert_variable_in_scope(
        context,
        context.next_scope - 1,
        name,
        Local(context.next_register),
      )
      |> advance
    })
    |> do_block(body.statements, _, [])
  }

  let names_count = list.length(names)

  let generated = [TForLoop(start, names_count, body), ..generated]

  #(context, generated)
}

fn function_declaration(
  span: parser.Span,
  name: parser.FunctionName,
  parameters: List(parser.Parameter),
  body: parser.Block,
  context: Context,
) -> #(Context, List(Instruction)) {
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

  let closure_register = context.next_register
  let #(context, closure_instructions) =
    closure(span, fn_name, parameters, body, closure_register, advance(context))

  case name, list.reverse(subfields) {
    name, [] -> #(context, [
      SetGlobal(name, closure_register),
      ..closure_instructions
    ])

    name, [key] -> {
      let #(context, name_register, name_instructions) = variable(name, context)

      #(advance(context), [
        SetTable(name_register, context.next_register, closure_register),
        LoadConstant(context.next_register, String(key)),
        ..list.append(name_instructions, closure_instructions)
      ])
    }
    name, [key, ..subfields] -> {
      let #(context, name_register, name_instructions) = variable(name, context)

      let table_register = context.next_register
      let context = advance(context)
      let subfields_instructions =
        list.fold(subfields, [], fn(acc, subfield) {
          [
            LoadConstant(context.next_register, String(subfield)),
            GetTable(table_register, table_register, context.next_register),
            ..acc
          ]
        })
        |> list.reverse

      let generated =
        list.append(
          [
            SetTable(table_register, context.next_register, closure_register),
            LoadConstant(context.next_register, String(key)),
          ],
          closure_instructions,
        )
        |> list.append(subfields_instructions)
        |> list.append([
          Move(table_register, name_register),
          ..name_instructions
        ])

      #(advance(context), generated)
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
  let assert Ok(Local(destination)) =
    find_variable_in_scope(context, context.next_scope - 1, name)

  closure(span, option.Some(name), parameters, body, destination, context)
}

fn local(
  names: List(parser.AttributeName),
  expressions: List(parser.Expression),
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(final, values) = case list.reverse(expressions) {
    [parser.Vararg(_) as final, ..rest] -> #(
      option.Some(final),
      list.reverse(rest),
    )
    [parser.Call(_, _, _) as final, ..rest] -> #(
      option.Some(final),
      list.reverse(rest),
    )
    _ -> #(option.None, expressions)
  }

  let names_count = list.length(names)
  let values_count = list.length(values)
  let diff = names_count - values_count

  let #(names, stray_names) =
    list.index_fold(names, #([], []), fn(acc, name, i) {
      case i + 1 <= values_count {
        True -> #([name, ..acc.0], acc.1)
        False -> #(acc.0, [name, ..acc.1])
      }
    })

  let names = list.reverse(names)
  let stray_names = list.reverse(stray_names)

  let #(context, generated) =
    list.zip(names, values)
    |> list.fold(#(context, []), fn(acc, pair) {
      let #(context, generated) = acc
      let #(parser.AttributeName(_, name), value) = pair
      let assert Ok(Local(destination)) =
        find_variable_in_scope(context, context.next_scope - 1, name)
      let #(context, value_instructions) = case value {
        // for calls and varargs expressions in the middle,
        // use just one result/value from them
        parser.Vararg(_) -> #(context, [Vararg(destination, 1)])
        parser.Call(_, name, args) ->
          case call(name, args, destination, context) {
            #(context, [Call(..) as call, ..rest]) -> #(context, [
              Call(..call, result_count: 1),
              ..rest
            ])
            other -> other
          }
        _ -> expression_to(value, destination, context)
      }

      #(context, list.append(value_instructions, generated))
    })

  case stray_names {
    [] -> #(context, generated)
    [first, ..] as stray_names -> {
      let parser.AttributeName(_, name) = first
      let assert Ok(Local(final_register)) =
        find_variable_in_scope(context, context.next_scope - 1, name)

      case final {
        option.Some(parser.Vararg(_)) -> #(context, [
          Vararg(final_register, diff),
          ..generated
        ])
        option.Some(parser.Call(_, name, args)) -> {
          let #(context, call_instructions) =
            call(name, args, final_register, context)

          let call_instructions = case call_instructions {
            [Call(_, _, _) as call, ..rest] -> [
              Call(..call, result_count: diff),
              ..rest
            ]
            other -> other
          }

          #(context, list.append(call_instructions, generated))
        }
        _ ->
          list.fold(stray_names, #(context, generated), fn(acc, name) {
            let #(context, generated) = acc
            let parser.AttributeName(_, name) = name
            let assert Ok(Local(destination)) =
              find_variable_in_scope(context, context.next_scope - 1, name)

            #(context, [LoadConstant(destination, Nil), ..generated])
          })
      }
    }
  }
}

fn return(
  values: List(parser.Expression),
  context: Context,
) -> #(Context, List(Instruction)) {
  case values {
    [] -> #(context, [Return(0, 0)])
    [parser.Vararg(_)] -> #(context, [
      Return(context.next_register, -1),
      Vararg(context.next_register, -1),
    ])
    [parser.Call(_, name, args)] -> {
      let start = context.next_register
      let #(context, call_instructions) =
        call(name, args, start, advance(context))
      let length = case call_instructions {
        [Call(_, _, length), ..] -> length
        _ -> -1
      }

      #(context, [Return(start, length), ..call_instructions])
    }
    [value] -> {
      let #(context, value_register, value_instructions) =
        expression(value, context)

      #(context, [Return(value_register, 1), ..value_instructions])
    }
    [_, ..] -> {
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
      let result_count = list.length(values)

      let start = context.next_register
      let #(context, generated) =
        list.index_fold(values, #(context, []), fn(acc, value, i) {
          let #(context, instructions) = acc
          let target_register = start + i

          let #(context, value_register, value_instructions) =
            expression(value, context)

          let generated = case target_register == value_register {
            True -> value_instructions
            False -> [
              Move(target_register, value_register),
              ..value_instructions
            ]
          }

          #(context, list.append(generated, instructions))
        })

      case final {
        option.Some(parser.Vararg(_)) -> {
          let vararg_start = start + result_count
          #(context, [Return(start, -1), Vararg(vararg_start, -1), ..generated])
        }
        option.Some(parser.Call(_, name, args)) -> {
          let context = Context(..context, next_register: start + result_count)
          let call_register = context.next_register
          let #(context, call_instructions) =
            call(name, args, call_register, advance(context))

          let length = case call_instructions {
            [Call(_, _, length), ..] -> length + result_count
            _ -> -1
          }

          #(
            context,
            [Return(start, length), ..call_instructions]
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
    // we don't use `expression_to` here because it will emit a move instruction
    // and we want to avoid doing that when we don't have a concrete destination
    parser.Variable(_, var) -> variable(var, context)
    _ -> {
      let destination = context.next_register
      let #(context, instructions) =
        expression_to(expr, destination, advance(context))

      #(context, destination, instructions)
    }
  }
}

fn expression_to(
  expr: parser.Expression,
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  case expr {
    parser.BooleanTrue(_) -> #(context, [
      LoadConstant(destination, Bool(True)),
    ])
    parser.BooleanFalse(_) -> #(context, [
      LoadConstant(destination, Bool(False)),
    ])
    parser.BooleanNil(_) -> #(context, [
      LoadConstant(destination, Nil),
    ])
    parser.Numeral(_, number) -> {
      let number = parse_number(number)
      #(context, [
        LoadConstant(destination, number),
      ])
    }
    parser.LiteralString(_, string) -> {
      #(context, [
        LoadConstant(destination, String(string)),
      ])
    }
    parser.Variable(_, var) -> variable_to(var, destination, context)
    parser.Vararg(_) -> #(context, [
      Vararg(destination, 1),
    ])
    parser.Function(span, parameters, body) ->
      closure(span, option.None, parameters, body, destination, context)
    parser.Call(_, function, args) -> call(function, args, destination, context)
    parser.Table(_, fields) -> table(fields, destination, context)
    parser.Index(_, table, key) -> index(table, key, destination, context)
    parser.BinaryOperation(_, operator, lhs, rhs) ->
      binary_operation(operator, lhs, rhs, destination, context)
    parser.UnaryOperation(_, operator, expr) ->
      unary_operation(operator, expr, destination, context)
  }
}

fn variable(
  var: String,
  context: Context,
) -> #(Context, Int, List(Instruction)) {
  let scope_index = context.next_scope - 1
  let scope = get_current_scope(context)

  case dict.get(scope.vars, var) {
    Ok(Local(var_register)) -> #(context, var_register, [])
    Ok(Upvalue(index)) -> #(advance(context), context.next_register, [
      GetUpvalue(context.next_register, index),
    ])
    Error(_) ->
      case find_variable_in_parent_scopes(context, var, scope_index) {
        option.Some(Local(var_register)) ->
          case scope.kind {
            Block -> #(context, var_register, [])
            Function -> {
              let index = context.next_upvalue
              let context =
                insert_variable_in_scope(
                  Context(..context, next_upvalue: index + 1),
                  context.next_scope - 1,
                  var,
                  Upvalue(var_register),
                )

              #(advance(context), context.next_register, [
                GetUpvalue(context.next_register, index),
              ])
            }
          }
        option.Some(Upvalue(index)) -> #(
          advance(context),
          context.next_register,
          [
            GetUpvalue(context.next_register, index),
          ],
        )
        option.None -> #(advance(context), context.next_register, [
          GetGlobal(context.next_register, var),
        ])
      }
  }
}

fn variable_to(
  var: String,
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let scope_index = context.next_scope - 1
  let scope = get_current_scope(context)

  case dict.get(scope.vars, var) {
    Ok(Local(var_register)) ->
      case var_register == destination {
        True -> #(context, [])
        False -> #(context, [Move(destination, var_register)])
      }
    Ok(Upvalue(index)) -> #(context, [
      GetUpvalue(destination, index),
    ])
    Error(_) ->
      case find_variable_in_parent_scopes(context, var, scope_index) {
        option.Some(Local(var_register)) ->
          case scope.kind {
            Block -> {
              case var_register == destination {
                True -> #(context, [])
                False -> #(context, [Move(destination, var_register)])
              }
            }
            Function -> {
              let index = context.next_upvalue
              let context =
                insert_variable_in_scope(
                  Context(..context, next_upvalue: index + 1),
                  context.next_scope - 1,
                  var,
                  Upvalue(var_register),
                )

              #(context, [
                GetUpvalue(destination, index),
              ])
            }
          }
        option.Some(Upvalue(index)) -> #(context, [
          GetUpvalue(destination, index),
        ])
        option.None -> #(context, [
          GetGlobal(destination, var),
        ])
      }
  }
}

fn closure(
  span: parser.Span,
  name: option.Option(String),
  parameters: List(parser.Parameter),
  body: parser.Block,
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let prototypes = context.prototypes
  let prototype_index = context.next_prototype

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

  let #(context, instructions) = {
    use context <- enter_function(context)
    let context =
      list.fold(parameters, context, fn(context, parameter) {
        case parameter {
          parser.NamedParameter(_, name) ->
            insert_variable_in_scope(
              context,
              context.next_scope - 1,
              name,
              Local(context.next_register),
            )
            |> advance
          parser.VariadicParameter(_) -> context
        }
      })

    do_block(body.statements, context, [])
    |> pair.map_second(fn(body) {
      case has_vararg {
        True -> [VarargPrep(arg_count), ..body]
        False -> body
      }
    })
  }

  let assert Ok(scope) = dict.get(context.scopes, context.next_scope)
  let #(upvalues, _) =
    dict.filter(scope.vars, fn(_key, value) {
      case value {
        Upvalue(_) -> True
        _ -> False
      }
    })
    |> dict.fold(#(dict.new(), 0), fn(acc, _key, value) {
      #(dict.insert(acc.0, acc.1, value.register), acc.1 + 1)
    })

  let prototype =
    Chunk(
      name:,
      instructions:,
      source: option.None,
      prototypes: context.prototypes,
      upvalues: dict.new(),
      arg_count:,
      is_vararg: has_vararg,
      span: Span(span.start.line, span.end.line),
    )

  let context =
    Context(
      ..context,
      next_prototype: prototype_index + 1,
      prototypes: dict.insert(prototypes, prototype_index, prototype),
    )
  let generated =
    dict.fold(upvalues, [], fn(acc, upvalue_index, local_register) {
      [Move(upvalue_index, local_register), ..acc]
    })
    |> list.reverse
    |> list.append([Closure(destination, prototype_index)])

  #(context, generated)
}

fn call(
  expr: parser.Expression,
  args: List(parser.Expression),
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, function_instructions) =
    expression_to(expr, destination, context)

  let #(final, args) = case list.reverse(args) {
    [parser.Vararg(_) as final, ..args] -> #(
      option.Some(final),
      list.reverse(args),
    )
    [parser.Call(_, _, _) as final, ..args] -> #(
      option.Some(final),
      list.reverse(args),
    )
    _ -> #(option.None, args)
  }
  let args_count = list.length(args)

  let context =
    Context(..context, next_register: context.next_register + args_count)
  let #(context, args_instructions) = arguments(args, destination + 1, context)
  case final {
    option.Some(parser.Vararg(_)) -> {
      let generated =
        [
          Call(destination, -1, -1),
          Vararg(destination + args_count + 1, -1),
          ..args_instructions
        ]
        |> list.append(function_instructions)

      #(advance(context), generated)
    }
    option.Some(parser.Call(_, _, _) as nested_call) -> {
      let #(context, _, nested_call_instructions) =
        expression(nested_call, context)

      let generated =
        [Call(destination, -1, -1), ..nested_call_instructions]
        |> list.append(args_instructions)
        |> list.append(function_instructions)

      #(context, generated)
    }
    _ -> {
      let generated =
        [Call(destination, args_count, -1), ..args_instructions]
        |> list.append(function_instructions)

      #(context, generated)
    }
  }
}

fn arguments(
  args: List(parser.Expression),
  start: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  list.index_fold(args, #(context, []), fn(acc, arg, i) {
    let #(context, instructions) = acc
    let destination = start + i

    // for calls and varargs expressions in the middle,
    // use just one result/value from them
    let #(context, arg_instructions) = case arg {
      parser.Vararg(_) -> #(context, [
        Vararg(destination, 1),
      ])
      parser.Call(_, name, args) ->
        case call(name, args, destination, context) {
          #(context, [Call(_, _, _) as call, ..rest]) -> #(context, [
            Call(..call, result_count: 1),
            ..rest
          ])
          other -> other
        }
      _ -> expression_to(arg, destination, context)
    }

    #(context, list.append(arg_instructions, instructions))
  })
}

fn table(
  fields: List(parser.Field),
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
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

  let #(final, array_part) = case list.reverse(array_part) {
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

  let array_count = list.length(array_part)

  let generated = [
    NewTable(destination, hash_count, array_count),
  ]

  let context =
    Context(..context, next_register: context.next_register + array_count)
  let #(context, generated) =
    list.index_fold(array_part, #(context, generated), fn(acc, value, i) {
      let #(context, instructions) = acc
      let target_register = destination + i + 1

      // for calls and varargs expressions in the middle,
      // use just one result/value from them
      let #(context, value_instructions) = case value {
        parser.Vararg(_) -> #(context, [
          Vararg(target_register, 1),
        ])
        parser.Call(_, name, args) ->
          case call(name, args, target_register, context) {
            #(context, [Call(_, _, _) as call, ..rest]) -> #(context, [
              Call(..call, result_count: 1),
              ..rest
            ])
            other -> other
          }
        _ -> expression_to(value, target_register, context)
      }

      #(context, list.append(value_instructions, instructions))
    })

  let #(context, generated) = case final {
    option.Some(parser.Vararg(_)) -> #(context, [
      SetList(destination, -1),
      Vararg(destination + array_count + 1, -1),
      ..generated
    ])

    option.Some(parser.Call(_, name, args)) -> {
      let #(context, call_instructions) =
        call(name, args, destination + array_count + 1, context)
      let length = case call_instructions {
        [Call(_, _, length)] -> length + array_count + 1
        _ -> -1
      }

      let generated =
        [SetList(destination, length), ..call_instructions]
        |> list.append(generated)

      #(context, generated)
    }
    _ -> {
      let generated = case generated {
        [NewTable(_, _, _)] -> generated
        _ -> [SetList(destination, array_count), ..generated]
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
        parser.LiteralString(_, name) -> #(advance(context), [
          SetTable(destination, context.next_register, value_register),
          LoadConstant(context.next_register, String(name)),
        ])
        _ -> {
          let #(context, key_register, key_instructions) =
            expression(key, context)
          #(context, [
            SetTable(destination, key_register, value_register),
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

  #(context, generated)
}

fn index(
  table: parser.Expression,
  key: parser.Expression,
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, table_register, table_instructions) =
    expression(table, context)
  let #(context, key_register, key_instructions) = expression(key, context)

  let generated =
    list.append(
      [GetTable(destination, table_register, key_register), ..key_instructions],
      table_instructions,
    )

  #(context, generated)
}

fn binary_operation(
  operator: parser.BinaryOperator,
  lhs: parser.Expression,
  rhs: parser.Expression,
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, lhs_register, lhs_instructions) = expression(lhs, context)
  let #(context, rhs_register, rhs_instructions) = expression(rhs, context)

  let operation_instruction = case operator {
    parser.Add -> Add(destination, lhs_register, rhs_register)
    parser.Sub -> Sub(destination, lhs_register, rhs_register)
    parser.Mult -> Mul(destination, lhs_register, rhs_register)
    parser.Div -> Div(destination, lhs_register, rhs_register)
    parser.FloorDiv -> FloorDiv(destination, lhs_register, rhs_register)
    parser.Pow -> Pow(destination, lhs_register, rhs_register)
    parser.Mod -> Mod(destination, lhs_register, rhs_register)
    parser.Eq -> Eq(destination, lhs_register, rhs_register)
    parser.NotEq -> NotEq(destination, lhs_register, rhs_register)
    parser.Gt -> Gt(destination, lhs_register, rhs_register)
    parser.GtEq -> GtEq(destination, lhs_register, rhs_register)
    parser.Lt -> Lt(destination, lhs_register, rhs_register)
    parser.LtEq -> LtEq(destination, lhs_register, rhs_register)
    parser.BitwiseAnd -> BwAnd(destination, lhs_register, rhs_register)
    parser.BitwiseOr -> BwOr(destination, lhs_register, rhs_register)
    parser.BitwiseXor -> BwXor(destination, lhs_register, rhs_register)
    parser.BitwiseShiftLeft ->
      ShiftLeft(destination, lhs_register, rhs_register)
    parser.BitwiseShiftRight ->
      ShiftRight(destination, lhs_register, rhs_register)
    parser.BooleanAnd -> And(destination, lhs_register, rhs_register)
    parser.BooleanOr -> Or(destination, lhs_register, rhs_register)
    parser.Concat -> Concat(destination, lhs_register, rhs_register)
  }

  let generated = [
    operation_instruction,
    ..list.append(rhs_instructions, lhs_instructions)
  ]

  #(context, generated)
}

fn unary_operation(
  operator: parser.UnaryOperator,
  expr: parser.Expression,
  destination: Int,
  context: Context,
) -> #(Context, List(Instruction)) {
  let #(context, expr_register, expr_instructions) = expression(expr, context)
  let operation_instruction = case operator {
    parser.NumeralNegation -> Unm(destination, expr_register)
    parser.BooleanNegation -> Not(destination, expr_register)
    parser.BitwiseNegation -> BwNot(destination, expr_register)
    parser.Length -> Length(destination, expr_register)
  }

  #(context, [operation_instruction, ..expr_instructions])
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
    <> string.join(list.repeat("_", chunk.arg_count), ", ")

  case chunk.is_vararg {
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
  |> dict.fold(chunk.prototypes, _, fn(acc, _, prototype) {
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
    GetGlobal(destination, name) -> #(
      "GETGLOBAL "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "name: "
        <> name,
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
    SetGlobal(name, value) -> #(
      "SETGLOBAL" <> "name: " <> name <> ", " <> "to: " <> int.to_string(value),
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
    SetList(table, length) -> #(
      "SETLIST "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "length: "
        <> int.to_string(length),
      identation,
    )
    SetListMulti(table, from, array_count, length) -> #(
      "SETLISTMULTI "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "starting: "
        <> int.to_string(from)
        <> ", "
        <> "array_count: "
        <> int.to_string(array_count)
        <> ", "
        <> "length: "
        <> int.to_string(length),
      identation,
    )
    SetListVararg(table, from, array_count, length) -> #(
      "SETLISTVARARG "
        <> "table: "
        <> int.to_string(table)
        <> ", "
        <> "starting: "
        <> int.to_string(from)
        <> ", "
        <> "array_count: "
        <> int.to_string(array_count)
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
    VarargPrep(start) -> #(
      "VARARGPREP " <> "arg_count: " <> int.to_string(start),
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
    ReturnVararg(start, fixed_result_count, result_count) -> #(
      "RETURNVARARG "
        <> "starting: "
        <> int.to_string(start)
        <> ", "
        <> "fixed: "
        <> int.to_string(fixed_result_count)
        <> ", "
        <> "count: "
        <> int.to_string(result_count),
      identation,
    )
    ReturnMulti(start, fixed_result_count, result_count) -> #(
      "RETURNVARARG "
        <> "starting: "
        <> int.to_string(start)
        <> ", "
        <> "fixed: "
        <> int.to_string(fixed_result_count)
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
    TForLoop(start, count, body) -> {
      let body =
        list.fold(body, "", fn(acc, instruction) {
          let #(instruction, _) =
            instruction_to_string(instruction, identation + 1)

          acc <> string.repeat("    ", identation + 1) <> instruction <> "\n"
        })

      let names =
        list.repeat("_", count)
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
    ForTest(destination, lhs, rhs, by) -> #(
      "FORTEST "
        <> "to: "
        <> int.to_string(destination)
        <> ", "
        <> "left: "
        <> int.to_string(lhs)
        <> "right: "
        <> int.to_string(rhs)
        <> "by: "
        <> int.to_string(by),
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
    Unm(destination, value) -> #(
      "UNM "
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
