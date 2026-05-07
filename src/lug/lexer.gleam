//// Lexer for Lua 5.4

import gleam/list
import gleam/option
import gleam/string

pub opaque type Lexer {
  Lexer(
    original: String,
    source: String,
    offset: Int,
    column: Int,
    line: Int,
    keep_comments: Bool,
    keep_whitespaces: Bool,
    new_lines: BinaryPattern,
  )
}

pub type Position {
  Position(offset: Int, column: Int, line: Int)
}

pub type Token {
  // literals
  Identifier(String)
  String(String)
  LongString(String)
  Int(String)
  Float(String)
  CommentSingle(String)
  CommentMultiple(String)

  // keywords
  And
  Break
  Do
  Else
  Elseif
  End
  BFalse
  For
  Function
  Goto
  If
  In
  Local
  Nil
  Not
  Or
  Repeat
  Return
  Then
  BTrue
  Until
  While

  // Arithmetic operators
  Plus
  Minus
  Star
  Slash
  SlashSlash
  Percent
  Circumflex

  // Bitwise operators
  Amper
  VBar
  Tilde
  GreaterGreater
  LessLess

  // Relational operators
  EqualEqual
  NotEqual
  Greater
  GreaterEqual
  Less
  LessEqual

  // other operators
  DotDot
  Hash

  // Grouping
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  LeftSquare
  RightSquare

  // Punctuation
  Equal
  Colon
  ColonColon
  Semicolon
  Comma
  Dot
  DotDotDot

  // whitespace
  Space(String)

  EndOfFile

  // Invalid code
  UnterminatedString(String)
  UnterminatedLongString(String)
  UnterminatedCommentMultiple(String)
  UnexpectedGrapheme(String)
}

pub fn new(source: String) -> Lexer {
  Lexer(
    original: source,
    source:,
    offset: 0,
    column: 1,
    line: 1,
    keep_comments: True,
    keep_whitespaces: True,
    new_lines: compile_binary_pattern(["\r", "\n"]),
  )
}

pub fn discard_comments(lexer: Lexer) -> Lexer {
  Lexer(..lexer, keep_comments: False)
}

pub fn discard_whitespaces(lexer: Lexer) -> Lexer {
  Lexer(..lexer, keep_whitespaces: False)
}

pub fn lex(lexer: Lexer) -> List(#(Token, Position)) {
  check_for_shebang(lexer)
  |> lex_loop([])
}

// removes shebang from first line if it is present
fn check_for_shebang(lexer: Lexer) -> Lexer {
  case lexer.source {
    "#!" <> _rest -> {
      let #(_shebang, rest) = split_until_new_line(lexer, lexer.source)
      Lexer(..lexer, original: rest, source: rest)
    }
    _ -> lexer
  }
}

fn lex_loop(
  lexer: Lexer,
  acc: List(#(Token, Position)),
) -> List(#(Token, Position)) {
  case lexer.source {
    // Whitespace
    " " <> rest | "\t" <> rest -> {
      let start = lexer.offset
      let #(lexer, content) =
        advance(lexer, rest, 1) |> lex_whitespace(start, 1)

      case lexer.keep_whitespaces {
        True -> {
          let pos = Position(start, lexer.column, lexer.line)
          lex_loop(lexer, [#(Space(content), pos), ..acc])
        }
        False -> lex_loop(lexer, acc)
      }
    }

    // Newline
    "\n" <> rest | "\r" <> rest -> {
      let start = lexer.offset
      let #(lexer, content) =
        advance_line(lexer, rest, 1) |> lex_whitespace(start, 1)

      case lexer.keep_whitespaces {
        True -> {
          let pos = Position(start, lexer.column, lexer.line)
          lex_loop(lexer, [#(Space(content), pos), ..acc])
        }
        False -> lex_loop(lexer, acc)
      }
    }

    // comments
    "--[" <> rest ->
      // see first if it is a single line comment starting with [
      case rest {
        "[" <> rest -> {
          let lexer = advance(lexer, rest, 4)
          let #(lexer, token) =
            lex_multi_line_comment(lexer, lexer.offset, 0, 0)

          case lexer.keep_comments {
            True -> lex_loop(lexer, [token, ..acc])
            False -> lex_loop(lexer, acc)
          }
        }

        _ -> {
          let #(lexer, token) =
            advance(lexer, rest, 2) |> lex_single_line_comment(lexer.offset)

          case lexer.keep_comments {
            True -> lex_loop(lexer, [token, ..acc])
            False -> lex_loop(lexer, acc)
          }
        }
      }

    "--" <> rest -> {
      let #(lexer, token) =
        advance(lexer, rest, 2) |> lex_single_line_comment(lexer.offset)

      case lexer.keep_comments {
        True -> lex_loop(lexer, [token, ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // Single line strings
    "\"" <> rest -> {
      let #(lexer, token) =
        advance(lexer, rest, 1) |> lex_string(lexer.offset, 0, Double, String)

      lex_loop(lexer, [token, ..acc])
    }

    "'" <> rest -> {
      let #(lexer, token) =
        advance(lexer, rest, 1) |> lex_string(lexer.offset, 0, Single, String)

      lex_loop(lexer, [token, ..acc])
    }

    // Long strings
    "[[" <> rest -> {
      let #(lexer, token) =
        advance(lexer, rest, 2) |> lex_long_string(lexer.offset, 0, 0)

      lex_loop(lexer, [token, ..acc])
    }

    // Need to appear first in the pattern matching
    "..." <> rest ->
      advance(lexer, rest, 3) |> lex_loop([token(lexer, DotDotDot), ..acc])
    "~=" <> rest ->
      advance(lexer, rest, 2) |> lex_loop([token(lexer, NotEqual), ..acc])

    // keywords and names 
    "a" <> rest
    | "b" <> rest
    | "c" <> rest
    | "d" <> rest
    | "e" <> rest
    | "f" <> rest
    | "g" <> rest
    | "h" <> rest
    | "i" <> rest
    | "j" <> rest
    | "k" <> rest
    | "l" <> rest
    | "m" <> rest
    | "n" <> rest
    | "o" <> rest
    | "p" <> rest
    | "q" <> rest
    | "r" <> rest
    | "s" <> rest
    | "t" <> rest
    | "u" <> rest
    | "v" <> rest
    | "w" <> rest
    | "x" <> rest
    | "y" <> rest
    | "z" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      let #(lexer, word) =
        advance(lexer, rest, 1)
        |> lex_lowercase_word(pos.offset, 1)

      let tok = case word {
        "and" -> And
        "break" -> Break
        "do" -> Do
        "else" -> Else
        "elseif" -> Elseif
        "end" -> End
        "false" -> BFalse
        "for" -> For
        "function" -> Function
        "goto" -> Goto
        "if" -> If
        "in" -> In
        "local" -> Local
        "nil" -> Nil
        "not" -> Not
        "or" -> Or
        "repeat" -> Repeat
        "return" -> Return
        "then" -> Then
        "true" -> BTrue
        "until" -> Until
        "while" -> While
        identifier -> Identifier(identifier)
      }

      lex_loop(lexer, [#(tok, pos), ..acc])
    }

    // Uppercase Name
    "A" <> rest
    | "B" <> rest
    | "C" <> rest
    | "D" <> rest
    | "E" <> rest
    | "F" <> rest
    | "G" <> rest
    | "H" <> rest
    | "I" <> rest
    | "J" <> rest
    | "K" <> rest
    | "L" <> rest
    | "M" <> rest
    | "N" <> rest
    | "O" <> rest
    | "P" <> rest
    | "Q" <> rest
    | "R" <> rest
    | "S" <> rest
    | "T" <> rest
    | "U" <> rest
    | "V" <> rest
    | "W" <> rest
    | "X" <> rest
    | "Y" <> rest
    | "Z" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      let #(lexer, identifier) =
        advance(lexer, rest, 1)
        |> lex_uppercase_word(pos.offset, 1)

      lex_loop(lexer, [#(Identifier(identifier), pos), ..acc])
    }

    // hexadecimal numbers
    "0x" <> rest | "0X" <> rest -> {
      let #(lexer, token) =
        advance(lexer, rest, 2) |> lex_hexadecimal_int(lexer.offset, 2)

      lex_loop(lexer, [token, ..acc])
    }

    // Decimal Numbers
    "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest -> {
      let start = lexer.offset
      let lexer = advance(lexer, rest, 1)
      let #(lexer, acc) = case lex_int(lexer, start, 1) {
        #(lexer, int, option.Some(minus)) -> #(lexer, [minus, int, ..acc])
        #(lexer, int, _) -> #(lexer, [int, ..acc])
      }

      lex_loop(lexer, acc)
    }

    "-0" <> rest
    | "-1" <> rest
    | "-2" <> rest
    | "-3" <> rest
    | "-4" <> rest
    | "-5" <> rest
    | "-6" <> rest
    | "-7" <> rest
    | "-8" <> rest
    | "-9" <> rest -> {
      let start = lexer.offset
      let lexer = advance(lexer, rest, 1)
      let #(lexer, acc) = case lex_int(lexer, start, 1) {
        #(lexer, int, option.Some(minus)) -> #(lexer, [minus, int, ..acc])
        #(lexer, int, _) -> #(lexer, [int, ..acc])
      }

      lex_loop(lexer, acc)
    }

    // Arithmetic operators
    "+" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Plus), ..acc])
    "-" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Minus), ..acc])
    "*" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Star), ..acc])
    "//" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, SlashSlash), ..acc])
    "/" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Slash), ..acc])
    "%" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Percent), ..acc])
    "^" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Circumflex), ..acc])

    // Bitwise operators
    "&" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Amper), ..acc])
    "|" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, VBar), ..acc])
    "~" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Tilde), ..acc])
    ">>" <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, GreaterGreater), ..acc])
    "<<" <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, LessLess), ..acc])

    // Relational operators
    "==" <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, EqualEqual), ..acc])
    ">=" <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, GreaterEqual), ..acc])
    "<=" <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, LessEqual), ..acc])
    ">" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Greater), ..acc])
    "<" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Less), ..acc])

    // other operators
    ".." <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, DotDot), ..acc])
    "#" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Hash), ..acc])

    // Grouping
    "(" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, LeftParen), ..acc])
    ")" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, RightParen), ..acc])
    "{" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, LeftBrace), ..acc])
    "}" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, RightBrace), ..acc])
    "[" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, LeftSquare), ..acc])
    "]" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, RightSquare), ..acc])

    // Punctuation
    "=" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Equal), ..acc])
    "::" <> rest ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, ColonColon), ..acc])
    ":" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Colon), ..acc])
    ";" <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Semicolon), ..acc])
    "," <> rest ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Comma), ..acc])
    "." <> rest -> lex_loop(advance(lexer, rest, 1), [token(lexer, Dot), ..acc])

    // check if we are at the end of the file
    _ ->
      case string.pop_grapheme(lexer.source) {
        // end of file, stop lexing
        Error(_) -> list.reverse([token(lexer, EndOfFile), ..acc])

        // unexpected grapheme, continue lexing
        Ok(#(unexpected, rest)) ->
          lex_loop(advance(lexer, rest, string.length(unexpected)), [
            token(lexer, UnexpectedGrapheme(unexpected)),
            ..acc
          ])
      }
  }
}

fn lex_whitespace(lexer: Lexer, start: Int, slice: Int) {
  case lexer.source {
    "\n" <> rest | "\r" <> rest ->
      lex_whitespace(advance_line(lexer, rest, 1), start, slice + 1)
    " " <> rest | "\t" <> rest ->
      lex_whitespace(advance(lexer, rest, 1), start, slice + 1)
    _ -> {
      let content = slice_bytes(lexer.original, start, slice)
      #(lexer, content)
    }
  }
}

fn lex_single_line_comment(
  lexer: Lexer,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  let #(comment, rest) = split_until_new_line(lexer, lexer.source)
  let pos = Position(start, lexer.column, lexer.line)

  let lexer = advance(lexer, rest, string.length(comment))
  #(lexer, #(CommentSingle(comment), pos))
}

fn lex_multi_line_comment(lexer: Lexer, start: Int, slice: Int, depth: Int) {
  case lexer.source {
    // check if we are entering in a nested multine comment
    "[" <> rest ->
      case find_opening_brace(rest, depth, 0) {
        option.Some(rest) ->
          advance(lexer, rest, depth + 2)
          |> lex_multi_line_comment(start, slice + depth + 2, depth + 1)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_multi_line_comment(start, slice + 1, depth)
      }

    "]" <> rest ->
      case find_closing_brace(rest, depth, 0) {
        option.Some(rest) if depth == 0 -> {
          let content = slice_bytes(lexer.original, start + 1, slice)
          #(
            advance(lexer, rest, depth + 1),
            token(lexer, CommentMultiple(content)),
          )
        }

        option.Some(rest) ->
          advance(lexer, rest, depth + 1)
          |> lex_multi_line_comment(start, slice + depth + 1, depth - 1)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_multi_line_comment(start, slice + 1, depth + 1)
      }

    "\n" <> rest ->
      advance_line(lexer, rest, 1)
      |> lex_multi_line_comment(start, slice + 1, depth)

    "" -> {
      let content = slice_bytes(lexer.original, start + 1, slice)
      #(lexer, token(lexer, UnterminatedCommentMultiple(content)))
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_multi_line_comment(start, slice + 1, depth)
  }
}

type StringClosingQuote {
  Single
  Double
}

fn lex_string(
  lexer: Lexer,
  start: Int,
  slice: Int,
  closing_quote: StringClosingQuote,
  emit: fn(String) -> Token,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "'" <> rest if closing_quote == Single ->
      consume_string(lexer, rest, start, slice, emit)
    "\"" <> rest if closing_quote == Double ->
      consume_string(lexer, rest, start, slice, emit)

    "\n" <> rest ->
      advance(lexer, rest, 2)
      |> lex_string(start, slice + 2, closing_quote, UnterminatedString)

    "\\z" <> rest -> {
      let #(lexer, content) =
        lex_whitespace(advance(lexer, rest, 1), lexer.offset, 2)

      lex_string(
        lexer,
        start,
        slice + string.length(content),
        closing_quote,
        emit,
      )
    }

    "" -> {
      let content = slice_bytes(lexer.original, start + 1, slice)
      #(lexer, token(lexer, UnterminatedString(content)))
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_string(start, slice + 1, closing_quote, emit)
  }
}

fn consume_string(
  lexer: Lexer,
  rest: String,
  offset: Int,
  slice: Int,
  emit: fn(String) -> Token,
) {
  let content = slice_bytes(lexer.original, offset + 1, slice)
  let pos = Position(offset, lexer.column, lexer.line)

  #(advance(lexer, rest, 1), #(emit(content), pos))
}

fn lex_long_string(lexer: Lexer, start: Int, slice: Int, depth: Int) {
  case lexer.source {
    "[" <> rest ->
      case find_opening_brace(rest, depth, 0) {
        option.Some(rest) ->
          advance(lexer, rest, depth + 2)
          |> lex_long_string(start, slice + depth + 2, depth + 1)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_long_string(start, slice + 1, depth)
      }

    "]" <> rest ->
      case find_closing_brace(rest, depth, 0) {
        option.Some(rest) if depth == 0 -> {
          let content = slice_bytes(lexer.original, start + 1, slice)
          #(advance(lexer, rest, depth + 1), token(lexer, LongString(content)))
        }

        option.Some(rest) ->
          advance(lexer, rest, depth + 1)
          |> lex_long_string(start, slice + depth + 1, depth - 1)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_long_string(start, slice + 1, depth + 1)
      }

    "\n" <> rest ->
      advance_line(lexer, rest, 1)
      |> lex_multi_line_comment(start, slice + 1, depth)

    "" -> {
      let content = slice_bytes(lexer.original, start + 1, slice)
      #(lexer, token(lexer, UnterminatedLongString(content)))
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_long_string(start, slice + 1, depth)
  }
}

fn find_opening_brace(content: String, depth: Int, current: Int) {
  case content {
    "[" <> rest if depth + 1 == current -> option.Some(rest)
    "=" <> rest -> find_opening_brace(rest, depth, current + 1)
    _ -> option.None
  }
}

fn find_closing_brace(content: String, depth: Int, current: Int) {
  case content {
    "]" <> rest if depth == current -> option.Some(rest)
    "=" <> rest -> find_closing_brace(rest, depth, current + 1)
    _ -> option.None
  }
}

fn lex_lowercase_word(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, String) {
  case lexer.source {
    "a" <> rest
    | "b" <> rest
    | "c" <> rest
    | "d" <> rest
    | "e" <> rest
    | "f" <> rest
    | "g" <> rest
    | "h" <> rest
    | "i" <> rest
    | "j" <> rest
    | "k" <> rest
    | "l" <> rest
    | "m" <> rest
    | "n" <> rest
    | "o" <> rest
    | "p" <> rest
    | "q" <> rest
    | "r" <> rest
    | "s" <> rest
    | "t" <> rest
    | "u" <> rest
    | "v" <> rest
    | "w" <> rest
    | "x" <> rest
    | "y" <> rest
    | "z" <> rest
    | "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest
    | "_" <> rest ->
      advance(lexer, rest, 1)
      |> lex_lowercase_word(start, slice + 1)
    _ -> {
      let name = slice_bytes(lexer.original, start, slice)
      #(lexer, name)
    }
  }
}

fn lex_uppercase_word(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, String) {
  case lexer.source {
    "a" <> rest
    | "b" <> rest
    | "c" <> rest
    | "d" <> rest
    | "e" <> rest
    | "f" <> rest
    | "g" <> rest
    | "h" <> rest
    | "i" <> rest
    | "j" <> rest
    | "k" <> rest
    | "l" <> rest
    | "m" <> rest
    | "n" <> rest
    | "o" <> rest
    | "p" <> rest
    | "q" <> rest
    | "r" <> rest
    | "s" <> rest
    | "t" <> rest
    | "u" <> rest
    | "v" <> rest
    | "w" <> rest
    | "x" <> rest
    | "y" <> rest
    | "z" <> rest
    | "A" <> rest
    | "B" <> rest
    | "C" <> rest
    | "D" <> rest
    | "E" <> rest
    | "F" <> rest
    | "G" <> rest
    | "H" <> rest
    | "I" <> rest
    | "J" <> rest
    | "K" <> rest
    | "L" <> rest
    | "M" <> rest
    | "N" <> rest
    | "O" <> rest
    | "P" <> rest
    | "Q" <> rest
    | "R" <> rest
    | "S" <> rest
    | "T" <> rest
    | "U" <> rest
    | "V" <> rest
    | "W" <> rest
    | "X" <> rest
    | "Y" <> rest
    | "Z" <> rest
    | "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest ->
      advance(lexer, rest, 1)
      |> lex_uppercase_word(start, slice + 1)
    _ -> {
      let name = slice_bytes(lexer.original, start, slice)
      #(lexer, name)
    }
  }
}

fn lex_int(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, #(Token, Position), option.Option(#(Token, Position))) {
  case lexer.source {
    "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest ->
      advance(lexer, rest, 1)
      |> lex_int(start, slice + 1)

    // prevent numbers like "0..4" to be mistakenly lexed as a float number
    ".." <> _rest -> {
      let #(lexer, int) = consume_int(lexer, start, slice)
      #(lexer, int, option.None)
    }

    // float number
    "." <> rest -> {
      let #(lexer, float) =
        advance(lexer, rest, 1) |> lex_float(start, slice + 1)

      #(lexer, float, option.None)
    }

    rest ->
      // we are done with lexing the integer, but we need to check
      // if the next token is a minus symbol and if it is lex it as a minus operator
      // instead of a negative number
      case rest {
        "-" <> rest -> {
          let #(lexer, int) = consume_int(lexer, start, slice)
          let minus = token(lexer, Minus)

          #(advance(lexer, rest, 1), int, option.Some(minus))
        }
        _ -> {
          let #(lexer, int) = consume_int(lexer, start, slice)
          #(lexer, int, option.None)
        }
      }
  }
}

fn consume_int(lexer: Lexer, start: Int, slice: Int) {
  let content = slice_bytes(lexer.original, start, slice)
  let pos = Position(start, lexer.column, lexer.line)

  #(lexer, #(Int(content), pos))
}

fn lex_float(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest ->
      advance(lexer, rest, 1)
      |> lex_float(start, slice + 1)

    // float number in exponential notation
    "e-" <> rest | "E-" <> rest ->
      advance(lexer, rest, 2)
      |> lex_float(start, slice + 2)

    "e" <> rest | "E" <> rest ->
      advance(lexer, rest, 1) |> lex_float(start, slice + 1)

    _ -> consume_float(lexer, start, slice)
  }
}

fn consume_float(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  let content = slice_bytes(lexer.original, start, slice)
  let pos = Position(start, lexer.column, lexer.line)
  #(lexer, #(Float(content), pos))
}

fn lex_hexadecimal_int(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest
    | "a" <> rest
    | "A" <> rest
    | "b" <> rest
    | "B" <> rest
    | "c" <> rest
    | "C" <> rest
    | "d" <> rest
    | "D" <> rest
    | "e" <> rest
    | "E" <> rest
    | "f" <> rest
    | "F" <> rest
    | "p" <> rest
    | "P" <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_int(start, slice + 1)

    // prevent numbers like "0xA..4" to be mistakenly lexed as an hexadecimal float number
    ".." <> _rest -> consume_int(lexer, start, slice)

    // hexadecimal float number
    "." <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    _ -> consume_int(lexer, start, slice)
  }
}

fn lex_hexadecimal_float(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest
    | "a" <> rest
    | "A" <> rest
    | "b" <> rest
    | "B" <> rest
    | "c" <> rest
    | "C" <> rest
    | "d" <> rest
    | "D" <> rest
    | "e" <> rest
    | "E" <> rest
    | "f" <> rest
    | "F" <> rest
    | "p" <> rest
    | "P" <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    _ -> consume_float(lexer, start, slice)
  }
}

fn advance(lexer: Lexer, source: String, offset: Int) -> Lexer {
  Lexer(
    ..lexer,
    source:,
    offset: lexer.offset + offset,
    column: lexer.column + offset,
  )
}

fn advance_line(lexer: Lexer, source: String, offset: Int) -> Lexer {
  Lexer(
    ..lexer,
    source:,
    offset: lexer.offset + offset,
    column: 1,
    line: lexer.line + 1,
  )
}

fn token(lexer: Lexer, token: Token) -> #(Token, Position) {
  #(token, Position(lexer.offset, lexer.column, lexer.line))
}

pub fn to_string(tokens: List(#(Token, Position))) -> String {
  list.fold(tokens, "", fn(acc, pair) {
    let #(token, _) = pair
    token_to_string(token) <> acc
  })
}

fn token_to_string(token: Token) -> String {
  case token {
    // literals
    Identifier(str) -> str
    String(str) -> str
    LongString(str) -> "[[" <> str <> "]]"
    Int(str) -> str
    Float(str) -> str
    CommentSingle(str) -> "--" <> str
    CommentMultiple(str) -> "--[[" <> str <> "]]"

    // keywords
    And -> "and"
    Break -> "break"
    Do -> "do"
    Else -> "else"
    Elseif -> "elseif"
    End -> "end"
    BFalse -> "false"
    For -> "for"
    Function -> "function"
    Goto -> "goto"
    If -> "if"
    In -> "in"
    Local -> "local"
    Nil -> "nil"
    Not -> "not"
    Or -> "or"
    Repeat -> "repeat"
    Return -> "return"
    Then -> "then"
    BTrue -> "true"
    Until -> "until"
    While -> "while"

    // Arithmetic operators
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    SlashSlash -> "//"
    Percent -> "%"
    Circumflex -> "^"

    // Relational operators
    EqualEqual -> "=="
    NotEqual -> "~="
    Greater -> ">"
    GreaterEqual -> ">="
    Less -> "<"
    LessEqual -> "<="

    // Grouping
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    LeftSquare -> "["
    RightSquare -> "]"

    // Bitwise operators
    Amper -> "&"
    VBar -> "|"
    Tilde -> "~"
    GreaterGreater -> ">>"
    LessLess -> "<<"

    // other operators
    DotDot -> ".."
    Hash -> "#"

    // Punctuation
    Equal -> "="
    Colon -> ":"
    ColonColon -> "::"
    Semicolon -> ";"
    Comma -> ","
    Dot -> "."
    DotDotDot -> "..."

    // whitespace
    Space(str) -> str

    EndOfFile -> ""

    // Invalid code
    UnterminatedString(str) -> "\"" <> str
    UnterminatedLongString(str) -> "[[" <> str
    UnterminatedCommentMultiple(str) -> "--[[" <> str
    UnexpectedGrapheme(str) -> str
  }
}

type BinaryPattern

@external(erlang, "lug_ffi", "compile_binary_pattern")
@external(javascript, "../lug_ffi.mjs", "compile_binary_patterns")
fn compile_binary_pattern(patterns: List(String)) -> BinaryPattern

@external(erlang, "lug_ffi", "split_before")
@external(javascript, "../lug_ffi.mjs", "split_before")
fn split_before(pattern: BinaryPattern, string: String) -> #(String, String)

fn split_until_new_line(lexer: Lexer, string: String) -> #(String, String) {
  split_before(lexer.new_lines, string)
}

@external(erlang, "binary", "part")
@external(javascript, "../lug_ffi.mjs", "slice_bytes")
fn slice_bytes(string: String, from byte: Int, sized bytes: Int) -> String

@external(erlang, "lug_ffi", "drop_byte")
@external(javascript, "../lug_ffi.mjs", "drop_byte")
fn drop_byte(string: String) -> String
