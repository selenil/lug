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
  Comment(String)
  LongComment(String)

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
  UnterminatedLongComment(String)
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
    "#" <> _rest -> {
      let source = case split_until_new_line(lexer, lexer.source) {
        #(_shebang, "\n" <> rest) -> rest
        #(_shebang, rest) -> rest
      }
      Lexer(..lexer, original: source, source:)
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
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, content) = advance(lexer, rest, 1) |> lex_whitespace(pos, 1)

      case lexer.keep_whitespaces {
        True -> lex_loop(lexer, [#(Space(content), pos), ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // Newline
    "\n" <> rest | "\r" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, content) =
        advance_line(lexer, rest, 1) |> lex_whitespace(pos, 1)

      case lexer.keep_whitespaces {
        True -> lex_loop(lexer, [#(Space(content), pos), ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // comments
    "--[" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      // see first if it is a single-line comment starting with a [
      let #(lexer, token) = case find_opening_brace(rest, 0) {
        option.Some(#(rest, depth)) ->
          advance(lexer, rest, 4 + depth) |> lex_long_comment(pos, 0, depth)

        option.None -> advance(lexer, "[" <> rest, 2) |> lex_comment(pos)
      }

      case lexer.keep_comments {
        True -> lex_loop(lexer, [token, ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    "--" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) = advance(lexer, rest, 2) |> lex_comment(pos)

      case lexer.keep_comments {
        True -> lex_loop(lexer, [token, ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // Single line strings
    "\"" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) =
        advance(lexer, rest, 1) |> lex_string(pos, 0, Double, String)

      lex_loop(lexer, [token, ..acc])
    }

    "'" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) =
        advance(lexer, rest, 1) |> lex_string(pos, 0, Single, String)

      lex_loop(lexer, [token, ..acc])
    }

    // Need to appear first in the pattern matching
    "..." <> rest ->
      advance(lexer, rest, 3) |> lex_loop([token(lexer, DotDotDot), ..acc])
    "~=" <> rest ->
      advance(lexer, rest, 2) |> lex_loop([token(lexer, NotEqual), ..acc])

    "[" <> rest ->
      case find_opening_brace(rest, 0) {
        // long string
        option.Some(#(rest, depth)) -> {
          let pos = Position(lexer.offset, lexer.column, lexer.line)
          let #(lexer, token) =
            advance(lexer, rest, 2 + depth) |> lex_long_string(pos, 0, depth)

          lex_loop(lexer, [token, ..acc])
        }

        // left square
        option.None ->
          lex_loop(advance(lexer, rest, 1), [token(lexer, LeftSquare), ..acc])
      }

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

      // check if there's a minus symbol next to avoid lexing it
      // as part of a negative number
      case lexer.source {
        "-" <> rest -> {
          let minus = token(lexer, Minus)
          lex_loop(advance(lexer, rest, 1), [minus, #(tok, pos), ..acc])
        }
        _ -> lex_loop(lexer, [#(tok, pos), ..acc])
      }
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

      // check if there's a minus symbol next to avoid lexing it
      // as part of a negative number
      case lexer.source {
        "-" <> rest -> {
          let minus = token(lexer, Minus)
          lex_loop(advance(lexer, rest, 1), [
            minus,
            #(Identifier(identifier), pos),
            ..acc
          ])
        }
        _ -> lex_loop(lexer, [#(Identifier(identifier), pos), ..acc])
      }
    }

    // discards names
    "_" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      let #(lexer, identifier) =
        advance(lexer, rest, 1)
        |> lex_discard_name(pos.offset, 1)

      lex_loop(lexer, [#(Identifier(identifier), pos), ..acc])
    }

    // hexadecimal numbers
    "0x" <> rest | "0X" <> rest -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) =
        advance(lexer, rest, 2) |> lex_hexadecimal_int(pos, 2)

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
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let lexer = advance(lexer, rest, 1)
      let #(lexer, acc) = case lex_int(lexer, pos, 1) {
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
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let lexer = advance(lexer, rest, 2)
      let #(lexer, acc) = case lex_int(lexer, pos, 2) {
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
      lex_loop(advance(lexer, rest, 2), [token(lexer, SlashSlash), ..acc])
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

fn lex_whitespace(lexer: Lexer, position: Position, slice: Int) {
  case lexer.source {
    "\n" <> rest | "\r" <> rest ->
      lex_whitespace(advance_line(lexer, rest, 1), position, slice + 1)
    " " <> rest | "\t" <> rest ->
      lex_whitespace(advance(lexer, rest, 1), position, slice + 1)
    _ -> {
      let content = slice_bytes(lexer.original, position.offset, slice)
      #(lexer, content)
    }
  }
}

fn lex_comment(
  lexer: Lexer,
  position: Position,
) -> #(Lexer, #(Token, Position)) {
  let #(comment, rest) = split_until_new_line(lexer, lexer.source)

  let lexer = advance(lexer, rest, string.length(comment))
  #(lexer, #(Comment(comment), position))
}

fn lex_long_comment(lexer: Lexer, position: Position, slice: Int, depth: Int) {
  case lexer.source {
    "]" <> rest ->
      case find_closing_brace(rest, 0) {
        option.Some(#(rest, found)) if found == depth -> {
          let content =
            slice_bytes(lexer.original, position.offset + depth + 4, slice)
          #(advance(lexer, rest, depth + 2), #(LongComment(content), position))
        }

        option.Some(#(rest, found)) ->
          advance(lexer, rest, found + 2)
          |> lex_long_comment(position, slice + found + 2, depth)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_long_comment(position, slice + 1, depth)
      }

    "\n" <> rest ->
      advance_line(lexer, rest, 1)
      |> lex_long_comment(position, slice + 1, depth)

    "" -> {
      let content =
        slice_bytes(lexer.original, position.offset + depth + 4, slice)
      #(
        advance(lexer, lexer.source, 1),
        #(UnterminatedLongComment(content), position),
      )
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_long_comment(position, slice + 1, depth)
  }
}

type StringClosingQuote {
  Single
  Double
}

fn lex_string(
  lexer: Lexer,
  position: Position,
  slice: Int,
  closing_quote: StringClosingQuote,
  emit: fn(String) -> Token,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "'" <> rest if closing_quote == Single ->
      consume_string(lexer, rest, position, slice, emit)
    "\"" <> rest if closing_quote == Double ->
      consume_string(lexer, rest, position, slice, emit)

    "\n" <> rest ->
      advance(lexer, rest, 2)
      |> lex_string(position, slice + 2, closing_quote, UnterminatedString)

    "\\z" <> rest -> {
      let #(lexer, content) =
        lex_whitespace(advance(lexer, rest, 2), position, 2)

      lex_string(
        lexer,
        position,
        slice + string.length(content),
        closing_quote,
        emit,
      )
    }

    "\\" <> rest ->
      case string.pop_grapheme(rest) {
        Ok(#(grapheme, rest)) -> {
          let to_move = string.length(grapheme) + 1
          let slice = slice + to_move

          advance(lexer, rest, to_move)
          |> lex_string(position, slice, closing_quote, emit)
        }
        Error(_) ->
          advance(lexer, rest, 1)
          |> lex_string(position, slice + 1, closing_quote, emit)
      }

    "" -> {
      let content = slice_bytes(lexer.original, position.offset + 1, slice)
      #(lexer, #(UnterminatedString(content), position))
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_string(position, slice + 1, closing_quote, emit)
  }
}

fn consume_string(
  lexer: Lexer,
  rest: String,
  position: Position,
  slice: Int,
  emit: fn(String) -> Token,
) {
  let content = slice_bytes(lexer.original, position.offset + 1, slice)
  #(advance(lexer, rest, 1), #(emit(content), position))
}

fn lex_long_string(lexer: Lexer, position: Position, slice: Int, depth: Int) {
  case lexer.source {
    "]" <> rest ->
      case find_closing_brace(rest, 0) {
        option.Some(#(rest, found)) if found == depth -> {
          let content =
            slice_bytes(lexer.original, position.offset + depth + 2, slice)

          #(advance(lexer, rest, depth + 2), #(LongString(content), position))
        }

        option.Some(#(rest, found)) ->
          advance(lexer, rest, found + 2)
          |> lex_long_string(position, slice + found + 2, depth)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_long_string(position, slice + 1, depth)
      }

    "\n" <> rest ->
      advance_line(lexer, rest, 1)
      |> lex_long_string(position, slice + 1, depth)

    "" -> {
      let content =
        slice_bytes(lexer.original, position.offset + depth + 2, slice)
      #(
        advance(lexer, lexer.source, 1),
        #(UnterminatedLongString(content), position),
      )
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_long_string(position, slice + 1, depth)
  }
}

fn find_opening_brace(
  content: String,
  current: Int,
) -> option.Option(#(String, Int)) {
  case content {
    "[" <> rest -> option.Some(#(rest, current))
    "=" <> rest -> find_opening_brace(rest, current + 1)
    _ -> option.None
  }
}

fn find_closing_brace(
  content: String,
  current: Int,
) -> option.Option(#(String, Int)) {
  case content {
    "]" <> rest -> option.Some(#(rest, current))
    "=" <> rest -> find_closing_brace(rest, current + 1)
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
    | "9" <> rest
    | "_" <> rest ->
      advance(lexer, rest, 1)
      |> lex_uppercase_word(start, slice + 1)
    _ -> {
      let name = slice_bytes(lexer.original, start, slice)
      #(lexer, name)
    }
  }
}

fn lex_discard_name(lexer: Lexer, start: Int, slice: Int) -> #(Lexer, String) {
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
    | "9" <> rest
    | "_" <> rest ->
      advance(lexer, rest, 1) |> lex_discard_name(start, slice + 1)
    _ -> {
      let name = slice_bytes(lexer.original, start, slice)
      #(lexer, name)
    }
  }
}

fn lex_int(
  lexer: Lexer,
  start: Position,
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

    // exponential notation
    "e+" <> rest | "E+" <> rest -> {
      let #(lexer, float) =
        advance(lexer, rest, 2) |> lex_float(start, slice + 2)

      #(lexer, float, option.None)
    }

    "e-" <> rest | "E-" <> rest -> {
      let #(lexer, float) =
        advance(lexer, rest, 2) |> lex_float(start, slice + 2)

      #(lexer, float, option.None)
    }

    "e" <> rest | "E" <> rest -> {
      let #(lexer, float) =
        advance(lexer, rest, 1) |> lex_float(start, slice + 1)

      #(lexer, float, option.None)
    }

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

fn consume_int(lexer: Lexer, position: Position, slice: Int) {
  let content = slice_bytes(lexer.original, position.offset, slice)
  #(lexer, #(Int(content), position))
}

fn lex_float(
  lexer: Lexer,
  start: Position,
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
    "e+" <> rest | "E+" <> rest ->
      advance(lexer, rest, 2)
      |> lex_float(start, slice + 2)

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
  position: Position,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  let content = slice_bytes(lexer.original, position.offset, slice)

  // handle float with trailing dot
  let number = case string.ends_with(content, ".") {
    True -> content <> "0"
    False -> content
  }

  #(lexer, #(Float(number), position))
}

fn lex_hexadecimal_int(
  lexer: Lexer,
  start: Position,
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
    | "F" <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_int(start, slice + 1)

    // prevent numbers like "0xA..4" to be mistakenly lexed as an hexadecimal float number
    ".." <> _rest -> consume_int(lexer, start, slice)

    // hexadecimal float number
    "." <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    // radix
    "p+" <> rest | "P+" <> rest ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    "p-" <> rest | "P-" <> rest ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    "p" <> rest | "P" <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    _ -> consume_int(lexer, start, slice)
  }
}

fn lex_hexadecimal_float(
  lexer: Lexer,
  start: Position,
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
    | "F" <> rest ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    // radix
    "p+" <> rest | "P+" <> rest ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    "p-" <> rest | "P-" <> rest ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    "p" <> rest | "P" <> rest ->
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

pub fn token_to_string(token: Token) -> String {
  case token {
    // literals
    Identifier(str) -> str
    String(str) -> str
    LongString(str) -> "[[" <> str <> "]]"
    Int(str) -> str
    Float(str) -> str
    Comment(str) -> "--" <> str
    LongComment(str) -> "--[[" <> str <> "]]"

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
    UnterminatedLongComment(str) -> "--[[" <> str
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
