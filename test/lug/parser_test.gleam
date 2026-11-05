import birdie
import gleam/string
import lug/parser
import pprint

fn snap(code code: String, title title: String) -> Nil {
  let result = case parser.parse(code) {
    Ok(ast) -> pprint.format(ast)
    Error(e) -> pprint.format(Error(e))
  }

  let content = code <> "\n\n------------------------------\n\n" <> result
  let title = "parser__" <> title

  birdie.snap(content, title)
}

fn snap_arithmetic_expression(code code: String, title title: String) -> Nil {
  let assert Ok(parser.Block(_, [parser.Return(_, [expr])])) =
    parser.parse(code)

  let result = arithmetic_expression_to_prefix_notation(expr)
  let content = code <> "\n\n------------------------------\n\n" <> result
  let title = "parser__" <> title

  birdie.snap(content, title)
}

fn arithmetic_expression_to_prefix_notation(
  expression: parser.Expression,
) -> String {
  case expression {
    parser.BinaryOperation(_span, operator, lhs, rhs) ->
      "("
      <> arithmetic_operator_to_string(operator)
      <> " "
      <> arithmetic_expression_to_prefix_notation(lhs)
      <> " "
      <> arithmetic_expression_to_prefix_notation(rhs)
      <> ")"
    parser.Numeral(_span, n) -> n
    parser.Variable(_span, var) -> var
    _ -> panic as "not number, variable or arithmetic expression"
  }
}

fn arithmetic_operator_to_string(op: parser.BinaryOperator) {
  case op {
    parser.Add -> "+"
    parser.Sub -> "-"
    parser.Mult -> "*"
    parser.Div -> "/"
    parser.FloorDiv -> "//"
    parser.Pow -> "^"
    parser.Mod -> "%"
    _ -> panic as "not an arithmetic operator"
  }
}

fn snap_statement_location(code code: String, title title: String) -> Nil {
  let assert Ok(parser.Block(_span, ast)) = parser.parse(code)
    as "failed to parse"
  let assert [statement] = ast as "more than one statement parsed"

  let content =
    highlight_code(
      code,
      statement.location.start.offset,
      statement.location.end.offset,
      0,
      "",
      "",
    )

  let title = "parser__" <> title

  birdie.snap(content, title)
}

fn snap_expression_location(code code: String, title title: String) -> Nil {
  let assert Ok(parser.Block(_span, ast)) = parser.parse(code)
    as "failed to parse"

  let assert [return] = ast as "more than one statement parsed"
  let assert parser.Return(_span, expressions) = return
    as "not a return statement"
  let assert [expression] = expressions as "more than one expression returned"

  let content =
    highlight_code(
      code,
      expression.location.start.offset,
      expression.location.end.offset,
      0,
      "",
      "",
    )

  let title = "parser__" <> title

  birdie.snap(content, title)
}

fn highlight_code(
  code: String,
  start: Int,
  end: Int,
  current: Int,
  acc: String,
  underline: String,
) -> String {
  case code {
    "" ->
      case string.contains(underline, "▔") {
        False -> code
        True -> acc <> "\n" <> underline
      }
    "\n" <> rest -> {
      let acc = case string.contains(underline, "▔") {
        False -> acc <> "\n"
        True -> acc <> "\n" <> underline <> "\n"
      }

      let next = current + 1
      case next >= end {
        True -> acc <> rest
        False -> highlight_code(rest, start, end, next, acc, "")
      }
    }
    _ ->
      case string.pop_grapheme(code) {
        Error(_) -> acc
        Ok(#(grapheme, rest)) -> {
          let acc = acc <> grapheme
          let to_move = string.byte_size(grapheme)
          let underline = case current {
            current if current >= start && current < end -> underline <> "▔"
            current if current >= start -> underline
            _ -> underline <> " "
          }

          highlight_code(rest, start, end, current + to_move, acc, underline)
        }
      }
  }
}

pub fn label_test() {
  "::label::"
  |> snap(title: "label")
}

pub fn break_test() {
  "break"
  |> snap(title: "break")
}

pub fn goto_test() {
  "goto label"
  |> snap(title: "goto")
}

pub fn do_test() {
  "do
     print('do_block')
     return 0
   end"
  |> snap(title: "do_block")
}

pub fn while_test() {
  "while a < 5 do a = a + 1 end"
  |> snap(title: "while_loop")
}

pub fn repeat_test() {
  "repeat a = a + 1 until a > 5"
  |> snap(title: "repeat_loop")
}

pub fn simple_if_test() {
  "if true then return false end"
  |> snap(title: "simple_if")
}

pub fn if_with_else_test() {
  "if a == 1 then
  return false
else
  return true
end"
  |> snap(title: "if_with_else")
}

pub fn if_with_else_if_test() {
  "if a == 1 then
  return false
elseif a == 2 then
  return true
end"
  |> snap(title: "if_with_else_if")
}

pub fn if_with_multiple_else_if_test() {
  "if a == 1 then
  return false
elseif a == 2 then
  return true
elseif b == 1 then
  return false
elseif b == 2 then
  return true
end"
  |> snap(title: "if_with_multiple_else_if")
}

pub fn if_with_else_if_and_else_test() {
  "if a == 1 then
  return false
elseif a == 2 then
  return true
else
  return nil
end"
  |> snap(title: "if_with_else_if_and_else")
}

pub fn numeric_for_test() {
  "for i = 0, 5, 2 do print(i) end"
  |> snap(title: "numeric_for")
}

pub fn numeric_for_withot_step_test() {
  "for i = 0, 5 do print(i) end"
  |> snap(title: "numeric_for_without_step")
}

pub fn generic_for_test() {
  "for a in b do print(a) end"
  |> snap(title: "generic_for")
}

pub fn generic_for_with_multiple_variables_test() {
  "for k, v in ipairs(tbl) do print(k, v) end"
  |> snap(title: "generic_for_with_multiple_variables")
}

pub fn for_in_with_multiple_variables_and_expressions_test() {
  "for a, b, c in d, e, f do print(a, b, c) end"
  |> snap(title: "for_in_with_multiple_variables_and_expressions")
}

pub fn function_declaration_without_arguments_test() {
  "function fun() return true end"
  |> snap(title: "function_declaration_without_arguments")
}

pub fn function_declaration_with_single_argument_test() {
  "function id(x) return x end"
  |> snap(title: "function_declaration_with_single_argument")
}

pub fn function_declaration_with_multiple_arguments_test() {
  "function sum(x, y, z) return x + y + z end"
  |> snap(title: "function_declaration_with_multiple_arguments")
}

pub fn function_declaration_with_variadic_argument_test() {
  "function variadic(...) return true end"
  |> snap(title: "function_declaration_with_variadic_argument")
}

pub fn function_declaration_with_normal_and_variadic_arguments_test() {
  "function concat(x, ...) return x end"
  |> snap(title: "function_declaration_with_normal_and_variadic_arguments")
}

pub fn function_declaration_with_single_subfield_test() {
  "function M.fun() return true end"
  |> snap(title: "function_declaration_with_single_subfield")
}

pub fn function_declaration_with_nested_subfields_test() {
  "function M.some_table.some_field.fun() return true end"
  |> snap(title: "function_declaration_with_nested_subfields")
}

pub fn function_declaration_with_method_test() {
  "function M:fun() return true end"
  |> snap(title: "function_declaration_with_method")
}

pub fn function_declaration_with_subfield_and_method_test() {
  "function M.some_table.some_field:method() return true end"
  |> snap(title: "function_declaration_with_subfield_and_method")
}

pub fn local_function_test() {
  "local function id(x) return x end"
  |> snap(title: "local_function")
}

pub fn local_test() {
  "local a = 1"
  |> snap(title: "local")
}

pub fn local_with_multiple_variables_test() {
  "local a, b, c = 1, 2, '3'"
  |> snap(title: "local_with_multiple_variables")
}

pub fn local_without_expressions_test() {
  "local a, b"
  |> snap(title: "local_without_expressions")
}

pub fn local_with_asymmetric_sides_test() {
  "local a, b, c = 1, true"
  |> snap(title: "local_with_asymmetric_sides")
}

pub fn local_with_attributes_test() {
  "local a <const>, b <close> = 1, 2"
  |> snap(title: "local_with_attributes")
}

pub fn local_with_attributes_mixed_test() {
  "local a <const>, b, c <close>, d = 1, 2, 3, 4"
  |> snap(title: "local_with_attributes_mixed")
}

pub fn return_test() {
  "return true"
  |> snap(title: "return")
}

pub fn return_multiple_values_test() {
  "return 'a string', 1, false"
  |> snap(title: "return_multiple_values")
}

pub fn nil_test() {
  "a = nil"
  |> snap(title: "nil")
}

pub fn booleans_test() {
  "a, b = true, false"
  |> snap(title: "booleans")
}

pub fn integers_test() {
  "a, b, c, d = 1, 2, 30, 500"
  |> snap(title: "integers")
}

pub fn hexadecimal_integers_test() {
  "a, b, c = 0x1, 0XabD3, 0x47Bc"
  |> snap(title: "hexadecimal_integers")
}

pub fn floats_test() {
  "a, b, c, d = 1.1, 2.12, 3.14, 4.51128"
  |> snap(title: "floats")
}

pub fn hexadecimal_floats_test() {
  "a, b, c  = 0x2.D1c, 0X3.aBe-2, -0xe1.8ap-1"
  |> snap(title: "hexadecimal_floats")
}

pub fn single_quoted_string_test() {
  "a = 'string'"
  |> snap(title: "single_quoted_string")
}

pub fn double_quoted_string_test() {
  "a = \"string\""
  |> snap(title: "double_quoted_string")
}

pub fn long_string_test() {
  "a = [[
a long string
with more text and
multiple lines
]]"
  |> snap(title: "long_string")
}

pub fn function_expression_without_arguments_test() {
  "return function() return true end"
  |> snap(title: "function_expression_without_arguments")
}

pub fn function_expression_with_arguments_test() {
  "return function(a, b, ...) return a + b end"
  |> snap(title: "function_expression_with_arguments")
}

pub fn function_call_without_arguments_test() {
  "fun()"
  |> snap(title: "function_call_without_arguments")
}

pub fn function_call_with_arguments_test() {
  "fun(a, b)"
  |> snap(title: "function_call_with_arguments")
}

pub fn function_call_with_string_test() {
  "print 'a string'"
  |> snap(title: "function_call_with_string")
}

pub fn function_call_with_table_test() {
  "run { config = { debug = true } }"
  |> snap(title: "function_call_with_table")
}

pub fn function_call_as_expression_test() {
  "local a = fun(b)"
  |> snap(title: "function_call_as_expression")
}

pub fn table_function_call_test() {
  "return tbl.fun(x)"
  |> snap(title: "table_function_call")
}

pub fn method_call_without_arguments_test() {
  "obj:method()"
  |> snap(title: "method_call_without_arguments")
}

pub fn method_call_with_arguments_test() {
  "obj:method(a, b)"
  |> snap(title: "method_call_with_arguments")
}

pub fn method_call_with_string_test() {
  "obj:print 'a string'"
  |> snap(title: "method_call_with_string")
}

pub fn method_call_with_table_test() {
  "obj:run { config = { debug = true } }"
  |> snap(title: "method_call_with_table")
}

pub fn method_call_as_expression_test() {
  "local a = obj:fun(b)"
  |> snap(title: "method_call_as_expression")
}

pub fn empty_table_test() {
  "return {}"
  |> snap(title: "empty_table")
}

pub fn list_table_test() {
  "return {1, 2, 3, true, 4 - 2 * 3, fun(x)}"
  |> snap(title: "list_table")
}

pub fn map_table_test() {
  "return {a = 1, b = 2, c = 3, d = true, e = 2 + 4 / 3, f = fun(x)}"
  |> snap(title: "map_table")
}

pub fn map_table_with_record_syntax_test() {
  "return {['a'] = 1, ['b' .. '2'] = 2, [1 + 2 * 3] = 3}"
  |> snap(title: "map_table_with_record_syntax")
}

pub fn map_table_with_different_syntax_test() {
  "return {['a'] = 1, b = 2, ['c'] = 3, d = 4}"
  |> snap(title: "map_table_with_different_syntax")
}

pub fn table_with_list_and_record_fields_test() {
  "return {1, 2, ['a'] = true, nil, [4 * 3] = false, 3}"
  |> snap(title: "table_with_list_and_record_fields")
}

pub fn integer_index_test() {
  "return tbl[1]"
  |> snap(title: "integer_index")
}

pub fn integer_index_nested_test() {
  "return tbl[1][2][3]"
  |> snap(title: "integer_index_nested")
}

pub fn string_index_test() {
  "return tbl['a']"
  |> snap(title: "string_index")
}

pub fn string_index_nested_test() {
  "return tbl['a']['b']['c']"
  |> snap(title: "string_index_nested")
}

pub fn index_dot_test() {
  "return tbl.a"
  |> snap(title: "index_dot_test")
}

pub fn index_nested_dot_test() {
  "return tbl.a.b.c"
  |> snap(title: "index_nested_dot")
}

pub fn index_integer_and_string_test() {
  "return tbl[1]['b']"
  |> snap(title: "index_integer_and_string")
}

pub fn index_string_and_integer_test() {
  "return tbl['a'][2]"
  |> snap(title: "index_string_and_integer")
}

pub fn index_dot_and_integer_test() {
  "return tbl.a[2]"
  |> snap(title: "index_dot_and_integer")
}

pub fn index_integer_and_dot_test() {
  "return tbl[1].b"
  |> snap(title: "index_integer_and_dot")
}

pub fn index_dot_and_string_test() {
  "return tbl.a['b']"
  |> snap(title: "index_dot_and_string")
}

pub fn index_string_and_dot_test() {
  "return tbl['a'].b"
  |> snap(title: "index_string_and_dot")
}

pub fn index_booleans_test() {
  "return tbl[true], tbl[false]"
  |> snap(title: "index_booleans")
}

pub fn add_test() {
  "return 1 + x"
  |> snap(title: "add")
}

pub fn add_multiple_test() {
  "return 1 + x + 3 + y"
  |> snap(title: "add_multiple")
}

pub fn sub_test() {
  "return 1 - x"
  |> snap(title: "sub")
}

pub fn sub_multiple_test() {
  "return 1 - x - 3 - y"
  |> snap(title: "sub_multiple")
}

pub fn mult_test() {
  "return 1 * x"
  |> snap(title: "mult")
}

pub fn mult_multiple_test() {
  "return 1 * x * 3 * y"
  |> snap(title: "mult_multiple")
}

pub fn div_test() {
  "return 1 / x"
  |> snap(title: "div")
}

pub fn div_multiple_test() {
  "return 1 / x / 3 / y"
  |> snap(title: "div_multiple")
}

pub fn floor_div_test() {
  "return 1 // x"
  |> snap(title: "floor_div")
}

pub fn floor_div_multiple_test() {
  "return 1 // x // 3 // y"
  |> snap(title: "floot_div_multiple")
}

pub fn pow_test() {
  "return 1 ^ x"
  |> snap(title: "pow")
}

pub fn pow_multiple_test() {
  "return 1 ^ x ^ 3 ^ y"
  |> snap(title: "pow_multiple")
}

pub fn mod_test() {
  "return 1 % x"
  |> snap(title: "mod")
}

pub fn mod_multiple_test() {
  "return 1 % x % 3 % y"
  |> snap(title: "mod_multiple")
}

pub fn equality_test() {
  "return x == y"
  |> snap(title: "equality")
}

pub fn equality_multiple_test() {
  "return x == y == z"
  |> snap(title: "equality_multiple")
}

pub fn non_equality_test() {
  "return x ~= y"
  |> snap(title: "non_equality")
}

pub fn non_equality_multiple_test() {
  "return x ~= y ~= z"
  |> snap(title: "non_equality_multiple")
}

pub fn greater_test() {
  "return x > y"
  |> snap(title: "greater")
}

pub fn greater_multiple_test() {
  "return x > y > z"
  |> snap(title: "greater_multiple")
}

pub fn less_test() {
  "return x < y"
  |> snap(title: "less")
}

pub fn less_multiple_test() {
  "return x < y < z"
  |> snap(title: "less_multiple")
}

pub fn greater_equal_test() {
  "return x >= y"
  |> snap(title: "greater_equal")
}

pub fn greater_equal_multiple_test() {
  "return x >= y >= z"
  |> snap(title: "greater_equal_multiple")
}

pub fn less_equal_test() {
  "return x <= y"
  |> snap(title: "less_equal")
}

pub fn less_equal_multiple_test() {
  "return x <= y <= z"
  |> snap(title: "less_equal_multiple")
}

pub fn bitwise_and_test() {
  "return x & y"
  |> snap(title: "bitwise_and")
}

pub fn bitwise_and_multiple_test() {
  "return x & y & z"
  |> snap(title: "bitwise_and_multiple")
}

pub fn bitwise_or_test() {
  "return x | y"
  |> snap(title: "bitwise_or")
}

pub fn bitwise_or_multiple_test() {
  "return x | y | z"
  |> snap(title: "bitwise_or_multiple")
}

pub fn bitwise_xor_test() {
  "return x ~ y"
  |> snap(title: "bitwise_xor")
}

pub fn bitwise_xor_multiple_test() {
  "return x ~ y ~ z"
  |> snap(title: "bitwise_xor_multiple")
}

pub fn bitwise_shift_left_test() {
  "return x << y"
  |> snap(title: "bitwise_shift_left")
}

pub fn bitwise_shift_left_multiple_test() {
  "return x << y << z"
  |> snap(title: "bitwise_shift_left_multiple")
}

pub fn bitwise_shift_right_test() {
  "return x >> y"
  |> snap(title: "bitwise_shift_right")
}

pub fn bitwise_shift_right_multiple_test() {
  "return x >> y >> z"
  |> snap(title: "bitwise_shift_right_multiple")
}

pub fn boolean_and_test() {
  "return x and y"
  |> snap(title: "boolean_and")
}

pub fn boolean_and_multiple_test() {
  "return x and y and z"
  |> snap(title: "boolean_and_multiple")
}

pub fn boolean_or_test() {
  "return x or y"
  |> snap(title: "boolean_or")
}

pub fn boolean_or_multiple_test() {
  "return x or y or z"
  |> snap(title: "boolean_or_multiple")
}

pub fn concat_test() {
  "return x .. 'a'"
  |> snap(title: "concat")
}

pub fn concat_multiple_test() {
  "return x .. 'a' .. y .. 'b'"
  |> snap(title: "concat_multiple")
}

pub fn bitwise_negation_test() {
  "return ~ x"
  |> snap(title: "bitwise_negation")
}

pub fn boolean_negation_test() {
  "return not x"
  |> snap(title: "boolean_negation")
}

pub fn bitwise_negation_precedence_test() {
  "return ~ x ~ y"
  |> snap(title: "bitwise_negation_precedence")
}

pub fn boolean_negation_precedence_test() {
  "return not true and false"
  |> snap(title: "boolean_negation_precedence")
}

pub fn numeral_negation_precedence_test() {
  "return -1 + -2"
  |> snap(title: "numeral_negation_precedence")
}

pub fn arithmetic_expression_test() {
  "return 1 + 2 - 3 * 4 / 5 // 6 ^ 7 % 8"
  |> snap_arithmetic_expression(title: "arithmetic_expression")
}

pub fn arithmetic_expression_with_parentheses_test() {
  "return 1 + (2 - 3) * 4 / 5 // 6 ^ (7 % 8)"
  |> snap_arithmetic_expression(title: "arithmetic_expression_with_parentheses")
}

pub fn expression_inside_multiple_parentheses_test() {
  "return (((0)))"
  |> snap(title: "expression_inside_multiple_parentheses")
}

pub fn label_location_test() {
  "::label::"
  |> snap_statement_location(title: "label_location")
}

pub fn break_location_test() {
  "break"
  |> snap_statement_location(title: "break_location")
}

pub fn goto_location_test() {
  "goto my_label"
  |> snap_statement_location(title: "goto_location")
}

pub fn do_block_location_test() {
  "do
  local a = 1
  return a
end"
  |> snap_statement_location(title: "do_block_location")
}

pub fn while_location_test() {
  "while a < 5 do
  print(a)
  a = a + 1
end"
  |> snap_statement_location(title: "do_location")
}

pub fn repeat_location_test() {
  "repeat a = a + 1 until a > 5"
  |> snap_statement_location(title: "repeat_location")
}

pub fn if_location_test() {
  "if a == 1 then return a end"
  |> snap_statement_location(title: "if_location")
}

pub fn if_with_else_location_test() {
  "if a == 1 then
  return true
else
  return false
end"
  |> snap_statement_location(title: "if_with_else_location")
}

pub fn if_with_else_if_location_test() {
  "if a == 1 then
  return true
elseif b == 2 then
  return true
end"
  |> snap_statement_location(title: "if_with_else_if_location")
}

pub fn if_with_else_if_and_else_location_test() {
  "if a == 1 then
  return true
elseif b == 2 then
  return true
else
  return true
end"
  |> snap_statement_location(title: "if_with_else_if_and_else_location")
}

pub fn numeric_for_location_test() {
  "for i = 0, 5, 1 do print(i) end"
  |> snap_statement_location(title: "numeric_for_location")
}

pub fn generic_for_location_test() {
  "for k, v in pairs(table) do print(k, v) end"
  |> snap_statement_location(title: "generic_for_location")
}

pub fn function_declaration_location_test() {
  "function add(x, y)
  return x + y
end"
  |> snap_statement_location(title: "function_location")
}

pub fn local_function_location_test() {
  "local function id(x)
  return x
end"
  |> snap_statement_location(title: "local_function_location")
}

pub fn local_location_test() {
  "local a, b = 1, 2"
  |> snap_statement_location(title: "local_location")
}

pub fn return_location_test() {
  "return true"
  |> snap_statement_location(title: "return_location")
}

pub fn function_expression_location_test() {
  "return function(x)
  return x
end"
  |> snap_expression_location(title: "function_expression_location")
}

pub fn call_location_test() {
  "return fun(a)"
  |> snap_expression_location(title: "call_location")
}

pub fn call_method_location_test() {
  "return obj:method(a)"
  |> snap_expression_location(title: "call_method_location")
}

pub fn table_location_test() {
  "return { 1, true, ['b' .. 2] = 3, d = false }"
  |> snap_expression_location(title: "table_location")
}

pub fn index_location_test() {
  "return tbl['a']"
  |> snap_expression_location(title: "index_location")
}

pub fn index_dot_location_test() {
  "return tbl.a"
  |> snap_expression_location(title: "index_dot_location")
}

pub fn empty_test() {
  let assert Ok(_) = parser.parse("")
}
