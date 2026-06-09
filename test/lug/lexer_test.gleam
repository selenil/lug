import lug/lexer

pub fn lex_with_shebang_test() {
  let tokens =
    lexer.new("#!/usr/bin/env lua\nprint('Hello, world')")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("print"), lexer.Position(0, 1, 1)),
      #(lexer.LeftParen, lexer.Position(5, 6, 1)),
      #(lexer.String("Hello, world"), lexer.Position(6, 7, 1)),
      #(lexer.RightParen, lexer.Position(20, 21, 1)),
      #(lexer.EndOfFile, lexer.Position(21, 22, 1)),
    ]
}

pub fn lex_with_starting_hash_test() {
  let tokens =
    lexer.new("# starting line\nprint('Hello, world')")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("print"), lexer.Position(0, 1, 1)),
      #(lexer.LeftParen, lexer.Position(5, 6, 1)),
      #(lexer.String("Hello, world"), lexer.Position(6, 7, 1)),
      #(lexer.RightParen, lexer.Position(20, 21, 1)),
      #(lexer.EndOfFile, lexer.Position(21, 22, 1)),
    ]
}

pub fn lex_identifier_lowercase_test() {
  let tokens =
    lexer.new("variable_name1")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("variable_name1"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(14, 15, 1)),
    ]
}

pub fn lex_indentifier_uppercase_test() {
  let tokens =
    lexer.new("VariableName1")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("VariableName1"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(13, 14, 1)),
    ]

  let tokens =
    lexer.new("variableName1")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("variableName1"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(13, 14, 1)),
    ]
}

pub fn lex_identifier_starting_with_an_underscore_test() {
  let tokens =
    lexer.new("_variable_name")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("_variable_name"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(14, 15, 1)),
    ]

  let tokens =
    lexer.new("_VariableName")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("_VariableName"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(13, 14, 1)),
    ]
}

pub fn lex_identifier_with_keyword_prefix_test() {
  let tokens =
    lexer.new("ending end repeatuntil repeat until Function")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("ending"), lexer.Position(0, 1, 1)),
      #(lexer.End, lexer.Position(7, 8, 1)),
      #(lexer.Identifier("repeatuntil"), lexer.Position(11, 12, 1)),
      #(lexer.Repeat, lexer.Position(23, 24, 1)),
      #(lexer.Until, lexer.Position(30, 31, 1)),
      #(lexer.Identifier("Function"), lexer.Position(36, 37, 1)),
      #(lexer.EndOfFile, lexer.Position(44, 45, 1)),
    ]
}

pub fn lex_string_test() {
  let tokens =
    lexer.new("'single line string'")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.String("single line string"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(20, 21, 1)),
    ]

  let tokens =
    lexer.new("\"single line string\"")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.String("single line string"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(20, 21, 1)),
    ]

  let tokens =
    lexer.new("''")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.String(""), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(2, 3, 1)),
    ]

  let tokens =
    lexer.new("\"\"")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.String(""), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(2, 3, 1)),
    ]
}

pub fn lex_string_with_newline_at_the_end_test() {
  let tokens =
    lexer.new("'a string \n'")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.UnterminatedString("a string \n'"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(13, 14, 1)),
    ]
}

pub fn lex_string_with_newlines_using_z_test() {
  let tokens =
    lexer.new("\"a string \\z \n\n whith newlines\"")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(
        lexer.String("a string \\z \n\n whith newlines"),
        lexer.Position(0, 1, 1),
      ),
      #(lexer.EndOfFile, lexer.Position(31, 17, 3)),
    ]
}

pub fn lex_unterminated_string_test() {
  let tokens =
    lexer.new("'string without closing mark")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(
        lexer.UnterminatedString("string without closing mark"),
        lexer.Position(0, 1, 1),
      ),
      #(lexer.EndOfFile, lexer.Position(28, 29, 1)),
    ]

  let tokens =
    lexer.new("\"string without closing mark")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(
        lexer.UnterminatedString("string without closing mark"),
        lexer.Position(0, 1, 1),
      ),
      #(lexer.EndOfFile, lexer.Position(28, 29, 1)),
    ]

  let tokens =
    lexer.new("'string without closing mark\"")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(
        lexer.UnterminatedString("string without closing mark\""),
        lexer.Position(0, 1, 1),
      ),
      #(lexer.EndOfFile, lexer.Position(29, 30, 1)),
    ]
}

pub fn lex_long_string_test() {
  let tokens =
    lexer.new(
      "[[
long string
]]",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LongString("\nlong string\n"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(17, 3, 3)),
    ]

  let tokens =
    lexer.new(
      "[=[
long string
]=]",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LongString("\nlong string\n"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(19, 4, 3)),
    ]

  let tokens =
    lexer.new(
      "[==[
long string
]==]",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LongString("\nlong string\n"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(21, 5, 3)),
    ]
}

pub fn lex_nested_long_string_test() {
  let tokens =
    lexer.new(
      "[[
a long string
[=[
  with nested long strings

  [==[
    in multiple levels
  ]==]
]=]

 [=[
  nested
]=]
]]",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(
        lexer.LongString(
          "\na long string\n[=[\n  with nested long strings\n\n  [==[\n    in multiple levels\n  ]==]\n]=]\n\n [=[\n  nested\n]=]\n",
        ),
        lexer.Position(0, 1, 1),
      ),
      #(lexer.EndOfFile, lexer.Position(111, 3, 14)),
    ]
}

pub fn lex_unterminated_long_string_test() {
  let tokens =
    lexer.new("[[a string]=]")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.UnterminatedLongString("a string]=]"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(14, 15, 1)),
    ]
}

pub fn lex_int_test() {
  let tokens =
    lexer.new("1 2 30 940")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("1"), lexer.Position(0, 1, 1)),
      #(lexer.Int("2"), lexer.Position(2, 3, 1)),
      #(lexer.Int("30"), lexer.Position(4, 5, 1)),
      #(lexer.Int("940"), lexer.Position(7, 8, 1)),
      #(lexer.EndOfFile, lexer.Position(10, 11, 1)),
    ]
}

pub fn lex_negative_int_test() {
  let tokens =
    lexer.new("-1 -2 -570")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("-1"), lexer.Position(0, 1, 1)),
      #(lexer.Int("-2"), lexer.Position(3, 4, 1)),
      #(lexer.Int("-570"), lexer.Position(6, 7, 1)),
      #(lexer.EndOfFile, lexer.Position(10, 11, 1)),
    ]
}

pub fn lex_hexadecimal_int_lowercase_test() {
  let tokens =
    lexer.new("0xa 0x2fff 0x5ab87")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("0xa"), lexer.Position(0, 1, 1)),
      #(lexer.Int("0x2fff"), lexer.Position(4, 5, 1)),
      #(lexer.Int("0x5ab87"), lexer.Position(11, 12, 1)),
      #(lexer.EndOfFile, lexer.Position(18, 19, 1)),
    ]
}

pub fn lex_hexadecimal_int_uppercase_test() {
  let tokens =
    lexer.new("0XD 0X3BBB 0X8FBEE")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("0XD"), lexer.Position(0, 1, 1)),
      #(lexer.Int("0X3BBB"), lexer.Position(4, 5, 1)),
      #(lexer.Int("0X8FBEE"), lexer.Position(11, 12, 1)),
      #(lexer.EndOfFile, lexer.Position(18, 19, 1)),
    ]
}

pub fn lex_hexadecimal_int_with_mixed_case_test() {
  let tokens =
    lexer.new("0xdA 0X21ee 0xAbDDc")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("0xdA"), lexer.Position(0, 1, 1)),
      #(lexer.Int("0X21ee"), lexer.Position(5, 6, 1)),
      #(lexer.Int("0xAbDDc"), lexer.Position(12, 13, 1)),
      #(lexer.EndOfFile, lexer.Position(19, 20, 1)),
    ]
}

pub fn lex_negative_int_with_negative_int_test() {
  let tokens =
    lexer.new("-1-1")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("-1"), lexer.Position(0, 1, 1)),
      #(lexer.Minus, lexer.Position(2, 3, 1)),
      #(lexer.Int("1"), lexer.Position(3, 4, 1)),
      #(lexer.EndOfFile, lexer.Position(4, 5, 1)),
    ]
}

pub fn lex_float_test() {
  let tokens =
    lexer.new("1.2 3.4")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("1.2"), lexer.Position(0, 1, 1)),
      #(lexer.Float("3.4"), lexer.Position(4, 5, 1)),
      #(lexer.EndOfFile, lexer.Position(7, 8, 1)),
    ]
}

pub fn lex_negative_float_test() {
  let tokens =
    lexer.new("-5.6 -7.8")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("-5.6"), lexer.Position(0, 1, 1)),
      #(lexer.Float("-7.8"), lexer.Position(5, 6, 1)),
      #(lexer.EndOfFile, lexer.Position(9, 10, 1)),
    ]
}

pub fn lex_float_with_exponential_lowercase_test() {
  let tokens =
    lexer.new("48.5e-2 9e+2")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("48.5e-2"), lexer.Position(0, 1, 1)),
      #(lexer.Float("9e+2"), lexer.Position(8, 9, 1)),
      #(lexer.EndOfFile, lexer.Position(12, 13, 1)),
    ]
}

pub fn lex_number_with_exponential_uppercase_test() {
  let tokens =
    lexer.new("31.9E-3 7E+6")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("31.9E-3"), lexer.Position(0, 1, 1)),
      #(lexer.Float("7E+6"), lexer.Position(8, 9, 1)),
      #(lexer.EndOfFile, lexer.Position(12, 13, 1)),
    ]
}

pub fn lex_hexadecimal_float_lowercase_test() {
  let tokens =
    lexer.new("0x4f.2")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("0x4f.2"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(6, 7, 1)),
    ]
}

pub fn lex_hexadecimal_float_with_radix_lowercase_test() {
  let tokens =
    lexer.new("0x58d.1p+2")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("0x58d.1p+2"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(10, 11, 1)),
    ]
}

pub fn lex_hexadecimal_float_with_radix_uppercase_test() {
  let tokens =
    lexer.new("0xa47.3P-8")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("0xa47.3P-8"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(10, 11, 1)),
    ]
}

pub fn lex_hexadecimal_float_uppercase_test() {
  let tokens =
    lexer.new("0X3CA.8")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("0X3CA.8"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(7, 8, 1)),
    ]
}

pub fn lex_float_with_mixed_cases_test() {
  let tokens =
    lexer.new("0xBDca.45 0XcaEA.31")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("0xBDca.45"), lexer.Position(0, 1, 1)),
      #(lexer.Float("0XcaEA.31"), lexer.Position(10, 11, 1)),
      #(lexer.EndOfFile, lexer.Position(19, 20, 1)),
    ]
}

pub fn lex_incorrect_float_test() {
  let tokens =
    lexer.new("1..2")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("1"), lexer.Position(0, 1, 1)),
      #(lexer.DotDot, lexer.Position(1, 2, 1)),
      #(lexer.Int("2"), lexer.Position(3, 4, 1)),
      #(lexer.EndOfFile, lexer.Position(4, 5, 1)),
    ]

  let tokens =
    lexer.new("1.2.3")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("1.2"), lexer.Position(0, 1, 1)),
      #(lexer.Dot, lexer.Position(3, 4, 1)),
      #(lexer.Int("3"), lexer.Position(4, 5, 1)),
      #(lexer.EndOfFile, lexer.Position(5, 6, 1)),
    ]
}

pub fn lex_float_with_trailing_dot_test() {
  let tokens =
    lexer.new("1.")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Float("1.0"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(2, 3, 1)),
    ]
}

pub fn lex_invalid_hexadecimal_number_test() {
  let tokens =
    lexer.new("0xagh")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("0xa"), lexer.Position(0, 1, 1)),
      #(lexer.Identifier("gh"), lexer.Position(3, 4, 1)),
      #(lexer.EndOfFile, lexer.Position(5, 6, 1)),
    ]
}

pub fn lex_single_comment_test() {
  let tokens =
    lexer.new("-- full line comment")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Comment(" full line comment"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(20, 21, 1)),
    ]

  let tokens =
    lexer.new("local a = 'b' -- a comment")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Local, lexer.Position(0, 1, 1)),
      #(lexer.Identifier("a"), lexer.Position(6, 7, 1)),
      #(lexer.Equal, lexer.Position(8, 9, 1)),
      #(lexer.String("b"), lexer.Position(10, 11, 1)),
      #(lexer.Comment(" a comment"), lexer.Position(14, 15, 1)),
      #(lexer.EndOfFile, lexer.Position(26, 27, 1)),
    ]
}

pub fn lex_single_comment_starting_with_brace_test() {
  let tokens =
    lexer.new("local a = 'b' --[ a comment")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Local, lexer.Position(0, 1, 1)),
      #(lexer.Identifier("a"), lexer.Position(6, 7, 1)),
      #(lexer.Equal, lexer.Position(8, 9, 1)),
      #(lexer.String("b"), lexer.Position(10, 11, 1)),
      #(lexer.Comment("[ a comment"), lexer.Position(14, 15, 1)),
      #(lexer.EndOfFile, lexer.Position(27, 28, 1)),
    ]
}

pub fn lex_long_comment_test() {
  let tokens =
    lexer.new("--[[long comment]]")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LongComment("long comment"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(18, 19, 1)),
    ]

  let tokens =
    lexer.new("--[=[long comment]=]")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LongComment("long comment"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(20, 21, 1)),
    ]

  let tokens =
    lexer.new("--[==[long comment]==]")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LongComment("long comment"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(22, 23, 1)),
    ]
}

pub fn lex_nested_long_comment_test() {
  let tokens =
    lexer.new(
      "--[[
a long comment

  [=[
  nesting 1
    [==[
      nesting 2
    ]==]
  ]==]

  [=[
    nesting 1
  ]=]
]]",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(
        lexer.LongComment(
          "\na long comment\n\n  [=[\n  nesting 1\n    [==[\n      nesting 2\n    ]==]\n  ]==]\n\n  [=[\n    nesting 1\n  ]=]\n",
        ),
        lexer.Position(0, 1, 1),
      ),
      #(lexer.EndOfFile, lexer.Position(109, 3, 14)),
    ]
}

pub fn lex_unterminated_long_comment_test() {
  let tokens =
    lexer.new("--[[a comment]=]")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.UnterminatedLongComment("a comment]=]"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(17, 18, 1)),
    ]
}

pub fn lex_keywords_test() {
  let tokens =
    lexer.new(
      "and break do else elseif end false for function goto if in local nil not or repeat return then true until while",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.And, lexer.Position(0, 1, 1)),
      #(lexer.Break, lexer.Position(4, 5, 1)),
      #(lexer.Do, lexer.Position(10, 11, 1)),
      #(lexer.Else, lexer.Position(13, 14, 1)),
      #(lexer.Elseif, lexer.Position(18, 19, 1)),
      #(lexer.End, lexer.Position(25, 26, 1)),
      #(lexer.BFalse, lexer.Position(29, 30, 1)),
      #(lexer.For, lexer.Position(35, 36, 1)),
      #(lexer.Function, lexer.Position(39, 40, 1)),
      #(lexer.Goto, lexer.Position(48, 49, 1)),
      #(lexer.If, lexer.Position(53, 54, 1)),
      #(lexer.In, lexer.Position(56, 57, 1)),
      #(lexer.Local, lexer.Position(59, 60, 1)),
      #(lexer.Nil, lexer.Position(65, 66, 1)),
      #(lexer.Not, lexer.Position(69, 70, 1)),
      #(lexer.Or, lexer.Position(73, 74, 1)),
      #(lexer.Repeat, lexer.Position(76, 77, 1)),
      #(lexer.Return, lexer.Position(83, 84, 1)),
      #(lexer.Then, lexer.Position(90, 91, 1)),
      #(lexer.BTrue, lexer.Position(95, 96, 1)),
      #(lexer.Until, lexer.Position(100, 101, 1)),
      #(lexer.While, lexer.Position(106, 107, 1)),
      #(lexer.EndOfFile, lexer.Position(111, 112, 1)),
    ]
}

pub fn lex_arithmetic_operators_test() {
  let tokens =
    lexer.new("+ - * / // % ^")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Plus, lexer.Position(0, 1, 1)),
      #(lexer.Minus, lexer.Position(2, 3, 1)),
      #(lexer.Star, lexer.Position(4, 5, 1)),
      #(lexer.Slash, lexer.Position(6, 7, 1)),
      #(lexer.SlashSlash, lexer.Position(8, 9, 1)),
      #(lexer.Percent, lexer.Position(11, 12, 1)),
      #(lexer.Circumflex, lexer.Position(13, 14, 1)),
      #(lexer.EndOfFile, lexer.Position(14, 15, 1)),
    ]
}

pub fn lex_arithmetic_expression_test() {
  let tokens =
    lexer.new("1 + 2 - 3 * 4 / (5 // 6) % (7 ^ 2)")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Int("1"), lexer.Position(0, 1, 1)),
      #(lexer.Plus, lexer.Position(2, 3, 1)),
      #(lexer.Int("2"), lexer.Position(4, 5, 1)),
      #(lexer.Minus, lexer.Position(6, 7, 1)),
      #(lexer.Int("3"), lexer.Position(8, 9, 1)),
      #(lexer.Star, lexer.Position(10, 11, 1)),
      #(lexer.Int("4"), lexer.Position(12, 13, 1)),
      #(lexer.Slash, lexer.Position(14, 15, 1)),
      #(lexer.LeftParen, lexer.Position(16, 17, 1)),
      #(lexer.Int("5"), lexer.Position(17, 18, 1)),
      #(lexer.SlashSlash, lexer.Position(19, 20, 1)),
      #(lexer.Int("6"), lexer.Position(22, 23, 1)),
      #(lexer.RightParen, lexer.Position(23, 24, 1)),
      #(lexer.Percent, lexer.Position(25, 26, 1)),
      #(lexer.LeftParen, lexer.Position(27, 28, 1)),
      #(lexer.Int("7"), lexer.Position(28, 29, 1)),
      #(lexer.Circumflex, lexer.Position(30, 31, 1)),
      #(lexer.Int("2"), lexer.Position(32, 33, 1)),
      #(lexer.RightParen, lexer.Position(33, 34, 1)),
      #(lexer.EndOfFile, lexer.Position(34, 35, 1)),
    ]
}

pub fn lex_bitwise_operators_test() {
  let tokens =
    lexer.new("& | ~ >> <<")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Amper, lexer.Position(0, 1, 1)),
      #(lexer.VBar, lexer.Position(2, 3, 1)),
      #(lexer.Tilde, lexer.Position(4, 5, 1)),
      #(lexer.GreaterGreater, lexer.Position(6, 7, 1)),
      #(lexer.LessLess, lexer.Position(9, 10, 1)),
      #(lexer.EndOfFile, lexer.Position(11, 12, 1)),
    ]
}

pub fn lex_relational_operators_test() {
  let tokens =
    lexer.new("== ~= > >= < <=")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.EqualEqual, lexer.Position(0, 1, 1)),
      #(lexer.NotEqual, lexer.Position(3, 4, 1)),
      #(lexer.Greater, lexer.Position(6, 7, 1)),
      #(lexer.GreaterEqual, lexer.Position(8, 9, 1)),
      #(lexer.Less, lexer.Position(11, 12, 1)),
      #(lexer.LessEqual, lexer.Position(13, 14, 1)),
      #(lexer.EndOfFile, lexer.Position(15, 16, 1)),
    ]
}

pub fn lex_other_operators_test() {
  let tokens =
    lexer.new(".. #")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.DotDot, lexer.Position(0, 1, 1)),
      #(lexer.Hash, lexer.Position(3, 4, 1)),
      #(lexer.EndOfFile, lexer.Position(4, 5, 1)),
    ]
}

pub fn lex_grouping_test() {
  let tokens =
    lexer.new("( ) { } [ ]")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LeftParen, lexer.Position(0, 1, 1)),
      #(lexer.RightParen, lexer.Position(2, 3, 1)),
      #(lexer.LeftBrace, lexer.Position(4, 5, 1)),
      #(lexer.RightBrace, lexer.Position(6, 7, 1)),
      #(lexer.LeftSquare, lexer.Position(8, 9, 1)),
      #(lexer.RightSquare, lexer.Position(10, 11, 1)),
      #(lexer.EndOfFile, lexer.Position(11, 12, 1)),
    ]
}

pub fn lex_punctuation_test() {
  let tokens =
    lexer.new("= : :: ; , . ...")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Equal, lexer.Position(0, 1, 1)),
      #(lexer.Colon, lexer.Position(2, 3, 1)),
      #(lexer.ColonColon, lexer.Position(4, 5, 1)),
      #(lexer.Semicolon, lexer.Position(7, 8, 1)),
      #(lexer.Comma, lexer.Position(9, 10, 1)),
      #(lexer.Dot, lexer.Position(11, 12, 1)),
      #(lexer.DotDotDot, lexer.Position(13, 14, 1)),
      #(lexer.EndOfFile, lexer.Position(16, 17, 1)),
    ]
}

pub fn lex_whitespaces_test() {
  let tokens =
    lexer.new("local a =  'b' ")
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Local, lexer.Position(0, 1, 1)),
      #(lexer.Space(" "), lexer.Position(5, 6, 1)),
      #(lexer.Identifier("a"), lexer.Position(6, 7, 1)),
      #(lexer.Space(" "), lexer.Position(7, 8, 1)),
      #(lexer.Equal, lexer.Position(8, 9, 1)),
      #(lexer.Space("  "), lexer.Position(9, 10, 1)),
      #(lexer.String("b"), lexer.Position(11, 12, 1)),
      #(lexer.Space(" "), lexer.Position(14, 15, 1)),
      #(lexer.EndOfFile, lexer.Position(15, 16, 1)),
    ]
}

pub fn lex_unexpected_grapheme_test() {
  let tokens =
    lexer.new("!")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.UnexpectedGrapheme("!"), lexer.Position(0, 1, 1)),
      #(lexer.EndOfFile, lexer.Position(1, 2, 1)),
    ]
}

pub fn lex_line_numbers_test() {
  let tokens =
    lexer.new(
      "function map(tbl, cbl)
  local new = {}
  for i, el in ipairs(tbl) do
    new[i] = cbl(el)
  end

  return new
end",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Function, lexer.Position(0, 1, 1)),
      #(lexer.Identifier("map"), lexer.Position(9, 10, 1)),
      #(lexer.LeftParen, lexer.Position(12, 13, 1)),
      #(lexer.Identifier("tbl"), lexer.Position(13, 14, 1)),
      #(lexer.Comma, lexer.Position(16, 17, 1)),
      #(lexer.Identifier("cbl"), lexer.Position(18, 19, 1)),
      #(lexer.RightParen, lexer.Position(21, 22, 1)),
      #(lexer.Local, lexer.Position(25, 3, 2)),
      #(lexer.Identifier("new"), lexer.Position(31, 9, 2)),
      #(lexer.Equal, lexer.Position(35, 13, 2)),
      #(lexer.LeftBrace, lexer.Position(37, 15, 2)),
      #(lexer.RightBrace, lexer.Position(38, 16, 2)),
      #(lexer.For, lexer.Position(42, 3, 3)),
      #(lexer.Identifier("i"), lexer.Position(46, 7, 3)),
      #(lexer.Comma, lexer.Position(47, 8, 3)),
      #(lexer.Identifier("el"), lexer.Position(49, 10, 3)),
      #(lexer.In, lexer.Position(52, 13, 3)),
      #(lexer.Identifier("ipairs"), lexer.Position(55, 16, 3)),
      #(lexer.LeftParen, lexer.Position(61, 22, 3)),
      #(lexer.Identifier("tbl"), lexer.Position(62, 23, 3)),
      #(lexer.RightParen, lexer.Position(65, 26, 3)),
      #(lexer.Do, lexer.Position(67, 28, 3)),
      #(lexer.Identifier("new"), lexer.Position(74, 5, 4)),
      #(lexer.LeftSquare, lexer.Position(77, 8, 4)),
      #(lexer.Identifier("i"), lexer.Position(78, 9, 4)),
      #(lexer.RightSquare, lexer.Position(79, 10, 4)),
      #(lexer.Equal, lexer.Position(81, 12, 4)),
      #(lexer.Identifier("cbl"), lexer.Position(83, 14, 4)),
      #(lexer.LeftParen, lexer.Position(86, 17, 4)),
      #(lexer.Identifier("el"), lexer.Position(87, 18, 4)),
      #(lexer.RightParen, lexer.Position(89, 20, 4)),
      #(lexer.End, lexer.Position(93, 3, 5)),
      #(lexer.Return, lexer.Position(100, 3, 7)),
      #(lexer.Identifier("new"), lexer.Position(107, 10, 7)),
      #(lexer.End, lexer.Position(111, 1, 8)),
      #(lexer.EndOfFile, lexer.Position(114, 4, 8)),
    ]
}

pub fn lex_function_calls() {
  let tokens =
    lexer.new("fun(a, b) tbl:fun(a, b)")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Identifier("fun"), lexer.Position(0, 1, 1)),
      #(lexer.LeftParen, lexer.Position(3, 4, 1)),
      #(lexer.Identifier("a"), lexer.Position(4, 5, 1)),
      #(lexer.Comma, lexer.Position(5, 6, 1)),
      #(lexer.Identifier("b"), lexer.Position(7, 8, 1)),
      #(lexer.RightParen, lexer.Position(8, 9, 1)),
      #(lexer.Identifier("tbl"), lexer.Position(10, 11, 1)),
      #(lexer.Colon, lexer.Position(11, 12, 1)),
      #(lexer.Identifier("fun"), lexer.Position(12, 13, 1)),
      #(lexer.LeftParen, lexer.Position(15, 16, 1)),
      #(lexer.Identifier("a"), lexer.Position(16, 17, 1)),
      #(lexer.Comma, lexer.Position(17, 18, 1)),
      #(lexer.Identifier("b"), lexer.Position(19, 20, 1)),
      #(lexer.RightParen, lexer.Position(21, 22, 1)),
      #(lexer.EndOfFile, lexer.Position(22, 23, 1)),
    ]
}

pub fn lex_simple_program_test() {
  let tokens =
    lexer.new(
      "function fib(n)
if n == 1 or n == 2 then
  return 1
else
  return fib(n-2) + fib(n-1)
end

print(fib(8))
",
    )
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.Function, lexer.Position(0, 1, 1)),
      #(lexer.Identifier("fib"), lexer.Position(9, 10, 1)),
      #(lexer.LeftParen, lexer.Position(12, 13, 1)),
      #(lexer.Identifier("n"), lexer.Position(13, 14, 1)),
      #(lexer.RightParen, lexer.Position(14, 15, 1)),
      #(lexer.If, lexer.Position(16, 1, 2)),
      #(lexer.Identifier("n"), lexer.Position(19, 4, 2)),
      #(lexer.EqualEqual, lexer.Position(21, 6, 2)),
      #(lexer.Int("1"), lexer.Position(24, 9, 2)),
      #(lexer.Or, lexer.Position(26, 11, 2)),
      #(lexer.Identifier("n"), lexer.Position(29, 14, 2)),
      #(lexer.EqualEqual, lexer.Position(31, 16, 2)),
      #(lexer.Int("2"), lexer.Position(34, 19, 2)),
      #(lexer.Then, lexer.Position(36, 21, 2)),
      #(lexer.Return, lexer.Position(43, 3, 3)),
      #(lexer.Int("1"), lexer.Position(50, 10, 3)),
      #(lexer.Else, lexer.Position(52, 1, 4)),
      #(lexer.Return, lexer.Position(59, 3, 5)),
      #(lexer.Identifier("fib"), lexer.Position(66, 10, 5)),
      #(lexer.LeftParen, lexer.Position(69, 13, 5)),
      #(lexer.Identifier("n"), lexer.Position(70, 14, 5)),
      #(lexer.Minus, lexer.Position(71, 15, 5)),
      #(lexer.Int("2"), lexer.Position(72, 16, 5)),
      #(lexer.RightParen, lexer.Position(73, 17, 5)),
      #(lexer.Plus, lexer.Position(75, 19, 5)),
      #(lexer.Identifier("fib"), lexer.Position(77, 21, 5)),
      #(lexer.LeftParen, lexer.Position(80, 24, 5)),
      #(lexer.Identifier("n"), lexer.Position(81, 25, 5)),
      #(lexer.Minus, lexer.Position(82, 26, 5)),
      #(lexer.Int("1"), lexer.Position(83, 27, 5)),
      #(lexer.RightParen, lexer.Position(84, 28, 5)),
      #(lexer.End, lexer.Position(86, 1, 6)),
      #(lexer.Identifier("print"), lexer.Position(91, 1, 8)),
      #(lexer.LeftParen, lexer.Position(96, 6, 8)),
      #(lexer.Identifier("fib"), lexer.Position(97, 7, 8)),
      #(lexer.LeftParen, lexer.Position(100, 10, 8)),
      #(lexer.Int("8"), lexer.Position(101, 11, 8)),
      #(lexer.RightParen, lexer.Position(102, 12, 8)),
      #(lexer.RightParen, lexer.Position(103, 13, 8)),
      #(lexer.EndOfFile, lexer.Position(105, 1, 9)),
    ]
}

pub fn lex_tables_test() {
  let tokens =
    lexer.new("{ 1 = 'a', 2 = 'b', 3 = { 1 = { 1 = 'c' } } }")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LeftBrace, lexer.Position(0, 1, 1)),
      #(lexer.Int("1"), lexer.Position(2, 3, 1)),
      #(lexer.Equal, lexer.Position(4, 5, 1)),
      #(lexer.String("a"), lexer.Position(6, 7, 1)),
      #(lexer.Comma, lexer.Position(9, 10, 1)),
      #(lexer.Int("2"), lexer.Position(11, 12, 1)),
      #(lexer.Equal, lexer.Position(13, 14, 1)),
      #(lexer.String("b"), lexer.Position(15, 16, 1)),
      #(lexer.Comma, lexer.Position(18, 19, 1)),
      #(lexer.Int("3"), lexer.Position(20, 21, 1)),
      #(lexer.Equal, lexer.Position(22, 23, 1)),
      #(lexer.LeftBrace, lexer.Position(24, 25, 1)),
      #(lexer.Int("1"), lexer.Position(26, 27, 1)),
      #(lexer.Equal, lexer.Position(28, 29, 1)),
      #(lexer.LeftBrace, lexer.Position(30, 31, 1)),
      #(lexer.Int("1"), lexer.Position(32, 33, 1)),
      #(lexer.Equal, lexer.Position(34, 35, 1)),
      #(lexer.String("c"), lexer.Position(36, 37, 1)),
      #(lexer.RightBrace, lexer.Position(40, 41, 1)),
      #(lexer.RightBrace, lexer.Position(42, 43, 1)),
      #(lexer.RightBrace, lexer.Position(44, 45, 1)),
      #(lexer.EndOfFile, lexer.Position(45, 46, 1)),
    ]

  let tokens =
    lexer.new("{ 1 = 'a', ['key'] = 1 }")
    |> lexer.discard_whitespaces
    |> lexer.lex

  assert tokens
    == [
      #(lexer.LeftBrace, lexer.Position(0, 1, 1)),
      #(lexer.Int("1"), lexer.Position(2, 3, 1)),
      #(lexer.Equal, lexer.Position(4, 5, 1)),
      #(lexer.String("a"), lexer.Position(6, 7, 1)),
      #(lexer.Comma, lexer.Position(9, 10, 1)),
      #(lexer.LeftSquare, lexer.Position(11, 12, 1)),
      #(lexer.String("key"), lexer.Position(12, 13, 1)),
      #(lexer.RightSquare, lexer.Position(17, 18, 1)),
      #(lexer.Equal, lexer.Position(19, 20, 1)),
      #(lexer.Int("1"), lexer.Position(21, 22, 1)),
      #(lexer.RightBrace, lexer.Position(23, 24, 1)),
      #(lexer.EndOfFile, lexer.Position(24, 25, 1)),
    ]
}

pub fn lex_empty_input_test() {
  let tokens =
    lexer.new("")
    |> lexer.lex

  assert tokens == [#(lexer.EndOfFile, lexer.Position(0, 1, 1))]
}
