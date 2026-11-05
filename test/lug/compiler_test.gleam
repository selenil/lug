import birdie
import gleam/list
import lug/compiler
import pprint

fn snap(code code: String, title title: String) -> Nil {
  let result = case compiler.compile(code) {
    Ok(chunk) -> compiler.chunk_to_string(chunk)
    Error(e) -> pprint.format(e)
  }

  let content = code <> "\n\n------------------------------\n\n" <> result

  birdie.snap(content, "compiler__" <> title)
}

type Number {
  Int(Int)
  Float(Float)
}

fn numbers(code code: String) -> List(Number) {
  let assert Ok(chunk) = compiler.compile(code) as "failed to compile the code"
  list.fold_until(chunk.instructions, [], fn(acc, instruction) {
    case instruction {
      compiler.LoadConstant(_, compiler.Int(n)) ->
        list.Continue([Int(n), ..acc])
      compiler.LoadConstant(_, compiler.Float(n)) ->
        list.Continue([Float(n), ..acc])
      _ -> list.Stop(acc)
    }
  })
  |> list.reverse
}

pub fn simple_assignment_test() {
  "a = 1"
  |> snap(title: "simple_assignment")
}

pub fn assignment_index_test() {
  "a.b = 1; a['b'] = 2"
  |> snap(title: "assignment_index")
}

pub fn assignment_with_multiple_values_test() {
  "a, b, c = 1, 2, 3"
  |> snap(title: "assignment_with_multiple_values")
}

pub fn assignment_with_asymetric_sides_test() {
  "a, b, c = 1, 2"
  |> snap(title: "assignment_with_asymetric_sides")
}

pub fn assignment_ending_with_vararg_test() {
  "a, b, c = 1, ..."
  |> snap(title: "assignment_ending_with_vararg")
}

pub fn assignment_ending_with_call_test() {
  "a, b, c = 1, fun()"
  |> snap(title: "assignment_ending_with_call")
}

pub fn assignment_variable_in_outer_scope_test() {
  "local a = 1
do
  a = 2
end
"
  |> snap(title: "assignment_variable_in_outer_scope")
}

pub fn assignment_variable_in_inner_scope_test() {
  "a = 'outer'
do
  local a = 'inner'
  a = 0
end
"
  |> snap(title: "assignment_variable_in_inner_scope")
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
  print('hello, world!')
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
  "if 1 == 1 then return false end"
  |> snap(title: "simple_if")
}

pub fn if_with_else_test() {
  "if 1 == 2 then
  return false
else
  return true
end"
  |> snap(title: "if_with_else")
}

pub fn if_with_else_if_test() {
  "if a == 0 then
  return false
elseif a == 1 then
  return true
elseif a == 2 then
  return false
elseif a == 3 then
  return true
end"
  |> snap(title: "if_with_else_if")
}

pub fn if_with_else_if_and_else_test() {
  "if a == 0 then
  return false
elseif a == 1 then
  return true
else
  return nil
end"
  |> snap(title: "if_with_else_if_and_else")
}

pub fn numeric_for_test() {
  "for i = 0, 5, 1 do print(i) end"
  |> snap(title: "numeric_for")
}

pub fn numeric_for_without_step_test() {
  "for i = 0, 5 do print(i) end"
  |> snap(title: "numeric_for_without_step")
}

pub fn numeric_for_with_negative_step_test() {
  "for i = 5, 0, -1 do print(i) end"
  |> snap(title: "numeric_for_with_negative_step")
}

pub fn generic_for_test() {
  "for a in b do print(a) end"
  |> snap(title: "generic_for")
}

pub fn generic_for_with_multiple_variables_test() {
  "for k, v in ipairs(tbl) do print(k, v) end"
  |> snap(title: "generic_for_with_multiple_variables")
}

pub fn for_in_with_multiple_expressions_ending_in_vararg_test() {
  "for a in b, c, ... do print(a) end"
  |> snap(title: "for_in_with_multiple_expressions_ending_in_vararg")
}

pub fn for_in_with_multiple_variables_and_expressions_test() {
  "for a, b, c in d, e, f do print(a, b, c) end"
  |> snap(title: "for_in_with_multiple_variables_and_expressions")
}

pub fn function_declaration_test() {
  "function add(a, b) return a + b end"
  |> snap(title: "function_declaration")
}

pub fn function_declaration_with_variadic_argument_test() {
  "function variadic(...) return true end"
  |> snap(title: "function_declaration_with_variadic_argument")
}

pub fn function_declaration_with_normal_and_variadic_arguments_test() {
  "function concat(x, ...) return x end"
  |> snap(title: "function_declaration_with_normal_and_variadic_arguments")
}

pub fn function_declaration_with_subfields_test() {
  "function M.field.fun() return true end"
  |> snap(title: "function_declaration_with_subfields")
}

pub fn function_declaration_with_method_test() {
  "function M:fun() return true end"
  |> snap(title: "function_declaration_with_method")
}

pub fn function_declaration_with_subfield_and_method_test() {
  "function M.some_table.some_field:method() return true end"
  |> snap(title: "function_declaration_with_subfield_and_method")
}

pub fn get_upvalue_test() {
  "local a = 1
function add(x) return x + a end"
  |> snap(title: "get_upvalue_test")
}

pub fn set_upvalue_test() {
  "local a = 1
function set() a = 2 end"
  |> snap(title: "set_upvalue_test")
}

pub fn local_function_test() {
  "local function id(x) return x end"
  |> snap(title: "local_function")
}

pub fn local_test() {
  "local hello = 'world!'"
  |> snap(title: "local")
}

pub fn local_with_multiple_variables_test() {
  "local a, b, c = 1, false, 'd'"
  |> snap(title: "local_with_multiple_variables")
}

pub fn local_without_expressions_test() {
  "local a, b"
  |> snap(title: "local_without_expressions")
}

pub fn local_with_asymmetric_sides_test() {
  "local a, b, c = 1"
  |> snap(title: "local_with_asymmetric_sides")
}

pub fn local_ending_with_vararg_test() {
  "local a, b, c = 1, ..."
  |> snap(title: "local_ending_with_vararg")
}

pub fn local_ending_with_call_test() {
  "local a, b = fun()"
  |> snap(title: "local_ending_with_call")
}

pub fn local_with_attributes_test() {
  "local a <const>, b <close> = 1, 2"
  |> snap(title: "local_with_attributes")
}

pub fn return_test() {
  "return 0"
  |> snap(title: "return")
}

pub fn return_multiple_values_test() {
  "return 0, 'a string', true"
  |> snap(title: "return_multiple_values")
}

pub fn return_vararg_test() {
  "return ..."
  |> snap(title: "return_vararg")
}

pub fn return_multiple_values_ending_with_vararg_test() {
  "return 0, true, ..."
  |> snap(title: "return_multiple_values_ending_with_vararg")
}

pub fn return_multiple_values_ending_with_call_test() {
  "return 0, true, fun()"
  |> snap(title: "return_multiple_values_ending_with_call")
}

pub fn variable_in_outer_scope_test() {
  "a = 2
do
  return a
end"
  |> snap(title: "variable_in_outer_scope")
}

pub fn variable_in_inner_scope_test() {
  "a = 'outer'
do
  local a = 'inner'
  return a
end"
  |> snap(title: "variable_in_inner_scope")
}

pub fn variable_in_multiple_scopes_test() {
  "do
  local b = 1
  return b
end

do
  local b = 2
  return b
end"
  |> snap(title: "variable_in_multiple_scopes")
}

pub fn function_call_test() {
  "return fun(a, b)"
  |> snap(title: "function_call")
}

pub fn function_call_with_vararg_test() {
  "return fun(a, b, ...)"
  |> snap(title: "function_call_with_vararg")
}

pub fn function_call_with_another_call_as_last_argument_test() {
  "return fun(a, b, other(c))"
  |> snap(title: "function_call_with_another_call_as_last_argument")
}

pub fn method_call_test() {
  "obj:method(a, b)"
  |> snap(title: "method_call")
}

pub fn empty_table_test() {
  "return {}"
  |> snap(title: "empty_table")
}

pub fn list_table_test() {
  "return { 1, 2, 3 }"
  |> snap(title: "list_table")
}

pub fn list_table_ending_with_vararg_test() {
  "return { 1, 2, 3, ... }"
  |> snap(title: "list_table_ending_with_vararg")
}

pub fn list_table_ending_with_call_test() {
  "return { 1, 2, 3, fun() }"
  |> snap(title: "list_table_ending_with_call")
}

pub fn map_table_test() {
  "return { a = 1, b = 2, c = 3 }"
  |> snap(title: "map_table")
}

pub fn table_with_list_and_record_fields_test() {
  "return { 1, 2, a = true, b = false }"
  |> snap(title: "table_with_list_and_record_fields")
}

pub fn index_test() {
  "return tbl[1]"
  |> snap(title: "index")
}

pub fn index_nested_test() {
  "return tbl[1][2][3]"
  |> snap(title: "index_nested")
}

pub fn index_dot_test() {
  "return tbl.a"
  |> snap(title: "index_dot")
}

pub fn index_nested_dot_test() {
  "return tbl.a.b.c"
  |> snap(title: "index_nested_dot")
}

pub fn add_test() {
  "return 1 + x"
  |> snap(title: "add")
}

pub fn sub_test() {
  "return 1 - x"
  |> snap(title: "sub")
}

pub fn mult_test() {
  "return 1 * x"
  |> snap(title: "mult")
}

pub fn div_test() {
  "return 1 / x"
  |> snap(title: "div")
}

pub fn floor_div_test() {
  "return 1 // x"
  |> snap(title: "floor_div")
}

pub fn pow_test() {
  "return 1 ^ x"
  |> snap(title: "pow")
}

pub fn mod_test() {
  "return 1 % x"
  |> snap(title: "mod")
}

pub fn numeral_negation_test() {
  "local a = 1; return -a"
  |> snap(title: "numeral_negation")
}

pub fn equality_test() {
  "return x == y"
  |> snap(title: "equality")
}

pub fn non_equality_test() {
  "return x ~= y"
  |> snap(title: "non_equality")
}

pub fn greater_test() {
  "return x > y"
  |> snap(title: "greater")
}

pub fn less_test() {
  "return x < y"
  |> snap(title: "less")
}

pub fn greater_equal_test() {
  "return x >= y"
  |> snap(title: "greater_equal")
}

pub fn less_equal_test() {
  "return x <= y"
  |> snap(title: "less_equal")
}

pub fn bitwise_and_test() {
  "return x & y"
  |> snap(title: "bitwise_and")
}

pub fn bitwise_or_test() {
  "return x | y"
  |> snap(title: "bitwise_or")
}

pub fn bitwise_xor_test() {
  "return x ~ y"
  |> snap(title: "bitwise_xor")
}

pub fn bitwise_shift_left_test() {
  "return x << y"
  |> snap(title: "bitwise_shift_left")
}

pub fn bitwise_shift_right_test() {
  "return x >> y"
  |> snap(title: "bitwise_shift_right")
}

pub fn boolean_and_test() {
  "return x and y"
  |> snap(title: "boolean_and")
}

pub fn boolean_or_test() {
  "return x or y"
  |> snap(title: "boolean_or")
}

pub fn concat_test() {
  "return x .. 'a'"
  |> snap(title: "concat")
}

pub fn bitwise_negation_test() {
  "return ~ x"
  |> snap(title: "bitwise_negation")
}

pub fn boolean_negation_test() {
  "return not x"
  |> snap(title: "boolean_negation")
}

pub fn length_test() {
  "return #{}"
  |> snap(title: "length")
}

pub fn int_test() {
  assert numbers("return 2, 20, 200") == [Int(2), Int(20), Int(200)]
}

pub fn int_with_exponent_test() {
  assert numbers("return 2e2") == [Float(200.0)]
}

pub fn float_test() {
  assert numbers("return 2.3, 3.2") == [Float(2.3), Float(3.2)]
}

pub fn float_with_exponent_test() {
  assert numbers("return 2.3e2, 2.3e+2, 2.3e-2")
    == [Float(230.0), Float(230.0), Float(0.023)]
}

pub fn hexadecimal_test() {
  assert numbers("return 0xb2") == [Int(178)]
}

pub fn hexadecimal_with_float_part_test() {
  assert numbers("return 0xb2.3") == [Float(178.1875)]
}

pub fn hexadecimal_with_radix_part_test() {
  assert numbers("return 0xb2p3, 0xb2p+3, 0xb2p-3")
    == [Float(1424.0), Float(1424.0), Float(22.25)]
}

pub fn hexadecimal_with_float_and_radix_part_test() {
  assert numbers("return 0xb2.3p3, 0xb2.3p+3, 0xb2.3p-3")
    == [Float(1425.5), Float(1425.5), Float(22.2734375)]
}
