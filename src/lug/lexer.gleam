//// Lexer for Lua 5.4

import gleam/bit_array
import gleam/list
import gleam/option

pub opaque type Lexer {
  Lexer(
    original: BitArray,
    source: BitArray,
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
  String(BitArray)
  LongString(BitArray)
  Int(String)
  Float(String)
  Comment(BitArray)
  LongComment(BitArray)

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
  Space(BitArray)

  EndOfFile

  // Invalid code
  UnterminatedString(BitArray)
  UnterminatedLongString(BitArray)
  UnterminatedLongComment(BitArray)
  UnexpectedGrapheme(BitArray)
}

pub fn from_string(source: String) -> Lexer {
  let source = bit_array.from_string(source)
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

pub fn new(source: BitArray) -> Result(Lexer, Nil) {
  case bit_array.bit_size(source) % 8 {
    0 ->
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
      |> Ok
    _ -> Error(Nil)
  }
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
    <<"#", _rest:bits>> -> {
      let source = case split_until_new_line(lexer, lexer.source) {
        #(_shebang, <<"\n", rest:bits>>) -> rest
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
    <<" ", rest:bits>> | <<"\t", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, content) = advance(lexer, rest, 1) |> lex_whitespace(pos, 1)

      case lexer.keep_whitespaces {
        True -> lex_loop(lexer, [#(Space(content), pos), ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // Newline
    <<"\n", rest:bits>> | <<"\r", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, content) =
        advance_line(lexer, rest, 1) |> lex_whitespace(pos, 1)

      case lexer.keep_whitespaces {
        True -> lex_loop(lexer, [#(Space(content), pos), ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // comments
    <<"--[", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      // see first if it is a single-line comment starting with a [
      let #(lexer, token) = case find_opening_brace(rest, 0) {
        option.Some(#(rest, depth)) ->
          advance(lexer, rest, 4 + depth) |> lex_long_comment(pos, 0, depth)

        option.None -> advance(lexer, <<"[", rest:bits>>, 2) |> lex_comment(pos)
      }

      case lexer.keep_comments {
        True -> lex_loop(lexer, [token, ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    <<"--", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) = advance(lexer, rest, 2) |> lex_comment(pos)

      case lexer.keep_comments {
        True -> lex_loop(lexer, [token, ..acc])
        False -> lex_loop(lexer, acc)
      }
    }

    // Single line strings
    <<"\"", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) =
        advance(lexer, rest, 1) |> lex_string(pos, 0, Double, String)

      lex_loop(lexer, [token, ..acc])
    }

    <<"'", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) =
        advance(lexer, rest, 1) |> lex_string(pos, 0, Single, String)

      lex_loop(lexer, [token, ..acc])
    }

    // Need to appear first in the pattern matching
    <<"...", rest:bits>> ->
      advance(lexer, rest, 3) |> lex_loop([token(lexer, DotDotDot), ..acc])
    <<"~=", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_loop([token(lexer, NotEqual), ..acc])

    <<"[", rest:bits>> ->
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
    <<"a", rest:bits>>
    | <<"b", rest:bits>>
    | <<"c", rest:bits>>
    | <<"d", rest:bits>>
    | <<"e", rest:bits>>
    | <<"f", rest:bits>>
    | <<"g", rest:bits>>
    | <<"h", rest:bits>>
    | <<"i", rest:bits>>
    | <<"j", rest:bits>>
    | <<"k", rest:bits>>
    | <<"l", rest:bits>>
    | <<"m", rest:bits>>
    | <<"n", rest:bits>>
    | <<"o", rest:bits>>
    | <<"p", rest:bits>>
    | <<"q", rest:bits>>
    | <<"r", rest:bits>>
    | <<"s", rest:bits>>
    | <<"t", rest:bits>>
    | <<"u", rest:bits>>
    | <<"v", rest:bits>>
    | <<"w", rest:bits>>
    | <<"x", rest:bits>>
    | <<"y", rest:bits>>
    | <<"z", rest:bits>> -> {
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
        <<"-", rest:bits>> -> {
          let minus = token(lexer, Minus)
          lex_loop(advance(lexer, rest, 1), [minus, #(tok, pos), ..acc])
        }
        _ -> lex_loop(lexer, [#(tok, pos), ..acc])
      }
    }

    // Uppercase Name
    <<"A", rest:bits>>
    | <<"B", rest:bits>>
    | <<"C", rest:bits>>
    | <<"D", rest:bits>>
    | <<"E", rest:bits>>
    | <<"F", rest:bits>>
    | <<"G", rest:bits>>
    | <<"H", rest:bits>>
    | <<"I", rest:bits>>
    | <<"J", rest:bits>>
    | <<"K", rest:bits>>
    | <<"L", rest:bits>>
    | <<"M", rest:bits>>
    | <<"N", rest:bits>>
    | <<"O", rest:bits>>
    | <<"P", rest:bits>>
    | <<"Q", rest:bits>>
    | <<"R", rest:bits>>
    | <<"S", rest:bits>>
    | <<"T", rest:bits>>
    | <<"U", rest:bits>>
    | <<"V", rest:bits>>
    | <<"W", rest:bits>>
    | <<"X", rest:bits>>
    | <<"Y", rest:bits>>
    | <<"Z", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      let #(lexer, identifier) =
        advance(lexer, rest, 1)
        |> lex_uppercase_word(pos.offset, 1)

      // check if there's a minus symbol next to avoid lexing it
      // as part of a negative number
      case lexer.source {
        <<"-", rest:bits>> -> {
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
    <<"_", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)

      let #(lexer, identifier) =
        advance(lexer, rest, 1)
        |> lex_discard_name(pos.offset, 1)

      lex_loop(lexer, [#(Identifier(identifier), pos), ..acc])
    }

    // hexadecimal numbers
    <<"0x", rest:bits>> | <<"0X", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, token) =
        advance(lexer, rest, 2) |> lex_hexadecimal_int(pos, 2)

      lex_loop(lexer, [token, ..acc])
    }

    // Decimal Numbers
    <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let lexer = advance(lexer, rest, 1)
      let #(lexer, acc) = case lex_int(lexer, pos, 1) {
        #(lexer, int, option.Some(minus)) -> #(lexer, [minus, int, ..acc])
        #(lexer, int, _) -> #(lexer, [int, ..acc])
      }

      lex_loop(lexer, acc)
    }

    // hexadecimal numbers prefixed with a negation
    <<"-0x", rest:bits>> | <<"-0X", rest:bits>> -> {
      let minus = token(lexer, Minus)
      let lexer = advance(lexer, <<"0x", rest:bits>>, 1)
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let #(lexer, number) =
        advance(lexer, rest, 2) |> lex_hexadecimal_int(pos, 2)

      lex_loop(lexer, [number, minus, ..acc])
    }

    <<"-0", rest:bits>>
    | <<"-1", rest:bits>>
    | <<"-2", rest:bits>>
    | <<"-3", rest:bits>>
    | <<"-4", rest:bits>>
    | <<"-5", rest:bits>>
    | <<"-6", rest:bits>>
    | <<"-7", rest:bits>>
    | <<"-8", rest:bits>>
    | <<"-9", rest:bits>> -> {
      let pos = Position(lexer.offset, lexer.column, lexer.line)
      let lexer = advance(lexer, rest, 2)
      let #(lexer, acc) = case lex_int(lexer, pos, 2) {
        #(lexer, int, option.Some(minus)) -> #(lexer, [minus, int, ..acc])
        #(lexer, int, _) -> #(lexer, [int, ..acc])
      }

      lex_loop(lexer, acc)
    }

    // Arithmetic operators
    <<"+", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Plus), ..acc])
    <<"-", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Minus), ..acc])
    <<"*", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Star), ..acc])
    <<"//", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, SlashSlash), ..acc])
    <<"/", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Slash), ..acc])
    <<"%", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Percent), ..acc])
    <<"^", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Circumflex), ..acc])

    // Bitwise operators
    <<"&", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Amper), ..acc])
    <<"|", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, VBar), ..acc])
    <<"~", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Tilde), ..acc])
    <<">>", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, GreaterGreater), ..acc])
    <<"<<", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, LessLess), ..acc])

    // Relational operators
    <<"==", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, EqualEqual), ..acc])
    <<">=", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, GreaterEqual), ..acc])
    <<"<=", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, LessEqual), ..acc])
    <<">", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Greater), ..acc])
    <<"<", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Less), ..acc])

    // other operators
    <<"..", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, DotDot), ..acc])
    <<"#", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Hash), ..acc])

    // Grouping
    <<"(", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, LeftParen), ..acc])
    <<")", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, RightParen), ..acc])
    <<"{", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, LeftBrace), ..acc])
    <<"}", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, RightBrace), ..acc])
    <<"]", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, RightSquare), ..acc])

    // Punctuation
    <<"=", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Equal), ..acc])
    <<"::", rest:bits>> ->
      lex_loop(advance(lexer, rest, 2), [token(lexer, ColonColon), ..acc])
    <<":", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Colon), ..acc])
    <<";", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Semicolon), ..acc])
    <<",", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Comma), ..acc])
    <<".", rest:bits>> ->
      lex_loop(advance(lexer, rest, 1), [token(lexer, Dot), ..acc])

    // check if we are at the end of the file
    _ ->
      case lexer.source {
        // unexpected grapheme, continue lexing
        <<unexpected, rest:bits>> ->
          lex_loop(advance(lexer, rest, 1), [
            token(lexer, UnexpectedGrapheme(<<unexpected>>)),
            ..acc
          ])
        // end of file, stop lexing
        <<>> -> list.reverse([token(lexer, EndOfFile), ..acc])
        _ -> panic as { "lexer.source must be byte aligned" }
      }
  }
}

fn lex_whitespace(lexer: Lexer, position: Position, slice: Int) {
  case lexer.source {
    <<"\n", rest:bits>> | <<"\r", rest:bits>> ->
      lex_whitespace(advance_line(lexer, rest, 1), position, slice + 1)
    <<" ", rest:bits>> | <<"\t", rest:bits>> ->
      lex_whitespace(advance(lexer, rest, 1), position, slice + 1)
    _ -> {
      let assert Ok(content) =
        bit_array.slice(lexer.original, position.offset, slice)
      #(lexer, content)
    }
  }
}

fn lex_comment(
  lexer: Lexer,
  position: Position,
) -> #(Lexer, #(Token, Position)) {
  let #(comment, rest) = split_until_new_line(lexer, lexer.source)

  let lexer = advance(lexer, rest, bit_array.byte_size(comment))
  #(lexer, #(Comment(comment), position))
}

fn lex_long_comment(lexer: Lexer, position: Position, slice: Int, depth: Int) {
  case lexer.source {
    <<"]", rest:bits>> ->
      case find_closing_brace(rest, 0) {
        option.Some(#(rest, found)) if found == depth -> {
          let assert Ok(content) =
            bit_array.slice(lexer.original, position.offset + depth + 4, slice)
          #(advance(lexer, rest, depth + 2), #(LongComment(content), position))
        }

        option.Some(#(rest, found)) ->
          advance(lexer, <<"]", rest:bits>>, found + 1)
          |> lex_long_comment(position, slice + found + 1, depth)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_long_comment(position, slice + 1, depth)
      }

    <<"\n", rest:bits>> ->
      advance_line(lexer, rest, 1)
      |> lex_long_comment(position, slice + 1, depth)

    <<>> -> {
      let assert Ok(content) =
        bit_array.slice(lexer.original, position.offset + depth + 4, slice)
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
  emit: fn(BitArray) -> Token,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    <<"'", rest:bits>> if closing_quote == Single ->
      consume_string(lexer, rest, position, slice, emit)
    <<"\"", rest:bits>> if closing_quote == Double ->
      consume_string(lexer, rest, position, slice, emit)

    <<"\n", rest:bits>> ->
      advance(lexer, rest, 2)
      |> lex_string(position, slice + 2, closing_quote, UnterminatedString)

    <<"\\z", rest:bits>> -> {
      let #(lexer, content) =
        lex_whitespace(advance(lexer, rest, 2), position, 2)

      lex_string(
        lexer,
        position,
        slice + bit_array.byte_size(content),
        closing_quote,
        emit,
      )
    }

    <<"\\", rest:bits>> ->
      case rest {
        <<_grapheme, rest:bits>> -> {
          // grapheme is always 1 byte + 1
          let to_move = 1 + 1
          let slice = slice + to_move

          advance(lexer, rest, to_move)
          |> lex_string(position, slice, closing_quote, emit)
        }
        <<>> ->
          advance(lexer, rest, 1)
          |> lex_string(position, slice + 1, closing_quote, emit)
        _ -> panic as { "lexer.source must be byte aligned" }
      }

    <<>> -> {
      let assert Ok(content) =
        bit_array.slice(lexer.original, position.offset + 1, slice)
      #(lexer, #(UnterminatedString(content), position))
    }

    _ ->
      advance(lexer, drop_byte(lexer.source), 1)
      |> lex_string(position, slice + 1, closing_quote, emit)
  }
}

fn consume_string(
  lexer: Lexer,
  rest: BitArray,
  position: Position,
  slice: Int,
  emit: fn(BitArray) -> Token,
) {
  let assert Ok(content) =
    bit_array.slice(lexer.original, position.offset + 1, slice)
  #(advance(lexer, rest, 1), #(emit(content), position))
}

fn lex_long_string(lexer: Lexer, position: Position, slice: Int, depth: Int) {
  case lexer.source {
    <<"]", rest:bits>> ->
      case find_closing_brace(rest, 0) {
        option.Some(#(rest, found)) if found == depth -> {
          let assert Ok(content) =
            bit_array.slice(lexer.original, position.offset + depth + 2, slice)

          #(advance(lexer, rest, depth + 2), #(LongString(content), position))
        }

        option.Some(#(rest, found)) ->
          advance(lexer, <<"]", rest:bits>>, found + 1)
          |> lex_long_string(position, slice + found + 1, depth)

        option.None ->
          advance(lexer, rest, 1)
          |> lex_long_string(position, slice + 1, depth)
      }

    <<"\n", rest:bits>> ->
      advance_line(lexer, rest, 1)
      |> lex_long_string(position, slice + 1, depth)

    <<>> -> {
      let assert Ok(content) =
        bit_array.slice(lexer.original, position.offset + depth + 2, slice)
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
  content: BitArray,
  current: Int,
) -> option.Option(#(BitArray, Int)) {
  case content {
    <<"[", rest:bits>> -> option.Some(#(rest, current))
    <<"=", rest:bits>> -> find_opening_brace(rest, current + 1)
    _ -> option.None
  }
}

fn find_closing_brace(
  content: BitArray,
  current: Int,
) -> option.Option(#(BitArray, Int)) {
  case content {
    <<"]", rest:bits>> -> option.Some(#(rest, current))
    <<"=", rest:bits>> -> find_closing_brace(rest, current + 1)
    _ -> option.None
  }
}

fn lex_lowercase_word(
  lexer: Lexer,
  start: Int,
  slice: Int,
) -> #(Lexer, String) {
  case lexer.source {
    <<"a", rest:bits>>
    | <<"b", rest:bits>>
    | <<"c", rest:bits>>
    | <<"d", rest:bits>>
    | <<"e", rest:bits>>
    | <<"f", rest:bits>>
    | <<"g", rest:bits>>
    | <<"h", rest:bits>>
    | <<"i", rest:bits>>
    | <<"j", rest:bits>>
    | <<"k", rest:bits>>
    | <<"l", rest:bits>>
    | <<"m", rest:bits>>
    | <<"n", rest:bits>>
    | <<"o", rest:bits>>
    | <<"p", rest:bits>>
    | <<"q", rest:bits>>
    | <<"r", rest:bits>>
    | <<"s", rest:bits>>
    | <<"t", rest:bits>>
    | <<"u", rest:bits>>
    | <<"v", rest:bits>>
    | <<"w", rest:bits>>
    | <<"x", rest:bits>>
    | <<"y", rest:bits>>
    | <<"z", rest:bits>>
    | <<"A", rest:bits>>
    | <<"B", rest:bits>>
    | <<"C", rest:bits>>
    | <<"D", rest:bits>>
    | <<"E", rest:bits>>
    | <<"F", rest:bits>>
    | <<"G", rest:bits>>
    | <<"H", rest:bits>>
    | <<"I", rest:bits>>
    | <<"J", rest:bits>>
    | <<"K", rest:bits>>
    | <<"L", rest:bits>>
    | <<"M", rest:bits>>
    | <<"N", rest:bits>>
    | <<"O", rest:bits>>
    | <<"P", rest:bits>>
    | <<"Q", rest:bits>>
    | <<"R", rest:bits>>
    | <<"S", rest:bits>>
    | <<"T", rest:bits>>
    | <<"U", rest:bits>>
    | <<"V", rest:bits>>
    | <<"W", rest:bits>>
    | <<"X", rest:bits>>
    | <<"Y", rest:bits>>
    | <<"Z", rest:bits>>
    | <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>>
    | <<"_", rest:bits>> ->
      advance(lexer, rest, 1)
      |> lex_lowercase_word(start, slice + 1)
    _ -> {
      let assert Ok(name) = bit_array.slice(lexer.original, start, slice)
      let assert Ok(name) = bit_array.to_string(name)
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
    <<"a", rest:bits>>
    | <<"b", rest:bits>>
    | <<"c", rest:bits>>
    | <<"d", rest:bits>>
    | <<"e", rest:bits>>
    | <<"f", rest:bits>>
    | <<"g", rest:bits>>
    | <<"h", rest:bits>>
    | <<"i", rest:bits>>
    | <<"j", rest:bits>>
    | <<"k", rest:bits>>
    | <<"l", rest:bits>>
    | <<"m", rest:bits>>
    | <<"n", rest:bits>>
    | <<"o", rest:bits>>
    | <<"p", rest:bits>>
    | <<"q", rest:bits>>
    | <<"r", rest:bits>>
    | <<"s", rest:bits>>
    | <<"t", rest:bits>>
    | <<"u", rest:bits>>
    | <<"v", rest:bits>>
    | <<"w", rest:bits>>
    | <<"x", rest:bits>>
    | <<"y", rest:bits>>
    | <<"z", rest:bits>>
    | <<"A", rest:bits>>
    | <<"B", rest:bits>>
    | <<"C", rest:bits>>
    | <<"D", rest:bits>>
    | <<"E", rest:bits>>
    | <<"F", rest:bits>>
    | <<"G", rest:bits>>
    | <<"H", rest:bits>>
    | <<"I", rest:bits>>
    | <<"J", rest:bits>>
    | <<"K", rest:bits>>
    | <<"L", rest:bits>>
    | <<"M", rest:bits>>
    | <<"N", rest:bits>>
    | <<"O", rest:bits>>
    | <<"P", rest:bits>>
    | <<"Q", rest:bits>>
    | <<"R", rest:bits>>
    | <<"S", rest:bits>>
    | <<"T", rest:bits>>
    | <<"U", rest:bits>>
    | <<"V", rest:bits>>
    | <<"W", rest:bits>>
    | <<"X", rest:bits>>
    | <<"Y", rest:bits>>
    | <<"Z", rest:bits>>
    | <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>>
    | <<"_", rest:bits>> ->
      advance(lexer, rest, 1)
      |> lex_uppercase_word(start, slice + 1)
    _ -> {
      let assert Ok(name) = bit_array.slice(lexer.original, start, slice)
      let assert Ok(name) = bit_array.to_string(name)
      #(lexer, name)
    }
  }
}

fn lex_discard_name(lexer: Lexer, start: Int, slice: Int) -> #(Lexer, String) {
  case lexer.source {
    <<"a", rest:bits>>
    | <<"b", rest:bits>>
    | <<"c", rest:bits>>
    | <<"d", rest:bits>>
    | <<"e", rest:bits>>
    | <<"f", rest:bits>>
    | <<"g", rest:bits>>
    | <<"h", rest:bits>>
    | <<"i", rest:bits>>
    | <<"j", rest:bits>>
    | <<"k", rest:bits>>
    | <<"l", rest:bits>>
    | <<"m", rest:bits>>
    | <<"n", rest:bits>>
    | <<"o", rest:bits>>
    | <<"p", rest:bits>>
    | <<"q", rest:bits>>
    | <<"r", rest:bits>>
    | <<"s", rest:bits>>
    | <<"t", rest:bits>>
    | <<"u", rest:bits>>
    | <<"v", rest:bits>>
    | <<"w", rest:bits>>
    | <<"x", rest:bits>>
    | <<"y", rest:bits>>
    | <<"z", rest:bits>>
    | <<"A", rest:bits>>
    | <<"B", rest:bits>>
    | <<"C", rest:bits>>
    | <<"D", rest:bits>>
    | <<"E", rest:bits>>
    | <<"F", rest:bits>>
    | <<"G", rest:bits>>
    | <<"H", rest:bits>>
    | <<"I", rest:bits>>
    | <<"J", rest:bits>>
    | <<"K", rest:bits>>
    | <<"L", rest:bits>>
    | <<"M", rest:bits>>
    | <<"N", rest:bits>>
    | <<"O", rest:bits>>
    | <<"P", rest:bits>>
    | <<"Q", rest:bits>>
    | <<"R", rest:bits>>
    | <<"S", rest:bits>>
    | <<"T", rest:bits>>
    | <<"U", rest:bits>>
    | <<"V", rest:bits>>
    | <<"W", rest:bits>>
    | <<"X", rest:bits>>
    | <<"Y", rest:bits>>
    | <<"Z", rest:bits>>
    | <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>>
    | <<"_", rest:bits>> ->
      advance(lexer, rest, 1) |> lex_discard_name(start, slice + 1)
    _ -> {
      let assert Ok(name) = bit_array.slice(lexer.original, start, slice)
      let assert Ok(name) = bit_array.to_string(name)
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
    <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>> ->
      advance(lexer, rest, 1)
      |> lex_int(start, slice + 1)

    // exponential notation
    <<"e+", rest:bits>> | <<"E+", rest:bits>> -> {
      let #(lexer, float) =
        advance(lexer, rest, 2) |> lex_float(start, slice + 2)

      #(lexer, float, option.None)
    }

    <<"e-", rest:bits>> | <<"E-", rest:bits>> -> {
      let #(lexer, float) =
        advance(lexer, rest, 2) |> lex_float(start, slice + 2)

      #(lexer, float, option.None)
    }

    <<"e", rest:bits>> | <<"E", rest:bits>> -> {
      let #(lexer, float) =
        advance(lexer, rest, 1) |> lex_float(start, slice + 1)

      #(lexer, float, option.None)
    }

    // prevent numbers like "0..4" to be mistakenly lexed as a float number
    <<"..", _rest:bits>> -> {
      let #(lexer, int) = consume_int(lexer, start, slice)
      #(lexer, int, option.None)
    }

    // float number
    <<".", rest:bits>> -> {
      let #(lexer, float) =
        advance(lexer, rest, 1) |> lex_float(start, slice + 1)

      #(lexer, float, option.None)
    }

    rest ->
      // we are done with lexing the integer, but we need to check
      // if the next token is a minus symbol and if it is lex it as a minus operator
      // instead of a negative number
      case rest {
        <<"-", rest:bits>> -> {
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

/// Precondition: slice is a Lua int 
fn consume_int(lexer: Lexer, position: Position, slice: Int) {
  let assert Ok(content) =
    bit_array.slice(lexer.original, position.offset, slice)
  let assert Ok(content) = bit_array.to_string(content)
  #(lexer, #(Int(content), position))
}

fn lex_float(
  lexer: Lexer,
  start: Position,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>> ->
      advance(lexer, rest, 1)
      |> lex_float(start, slice + 1)

    // float number in exponential notation
    <<"e+", rest:bits>> | <<"E+", rest:bits>> ->
      advance(lexer, rest, 2)
      |> lex_float(start, slice + 2)

    <<"e-", rest:bits>> | <<"E-", rest:bits>> ->
      advance(lexer, rest, 2)
      |> lex_float(start, slice + 2)

    <<"e", rest:bits>> | <<"E", rest:bits>> ->
      advance(lexer, rest, 1) |> lex_float(start, slice + 1)

    _ -> consume_float(lexer, start, slice)
  }
}

/// Precondition: slice is a Lua float 
fn consume_float(
  lexer: Lexer,
  position: Position,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  let assert Ok(content) =
    bit_array.slice(lexer.original, position.offset, slice)

  // handle float with trailing dot
  let number = case bit_array.slice(content, bit_array.byte_size(content), -1) {
    Ok(<<".">>) -> <<content:bits, "0">>
    Ok(_) -> content
    Error(_nil) -> panic as "unreachable"
  }

  let assert Ok(number) = bit_array.to_string(number)
  #(lexer, #(Float(number), position))
}

fn lex_hexadecimal_int(
  lexer: Lexer,
  start: Position,
  slice: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    // exponential
    <<"e+", rest:bits>> | <<"E+", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    <<"e-", rest:bits>> | <<"E-", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    // radix
    <<"p+", rest:bits>> | <<"P+", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    <<"p-", rest:bits>> | <<"P-", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    <<"p", rest:bits>> | <<"P", rest:bits>> ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>>
    | <<"a", rest:bits>>
    | <<"A", rest:bits>>
    | <<"b", rest:bits>>
    | <<"B", rest:bits>>
    | <<"c", rest:bits>>
    | <<"C", rest:bits>>
    | <<"d", rest:bits>>
    | <<"D", rest:bits>>
    | <<"e", rest:bits>>
    | <<"E", rest:bits>>
    | <<"f", rest:bits>>
    | <<"F", rest:bits>> ->
      advance(lexer, rest, 1) |> lex_hexadecimal_int(start, slice + 1)

    // prevent numbers like "0xA..4" to be mistakenly lexed as an hexadecimal float number
    <<"..", _rest:bits>> -> consume_int(lexer, start, slice)

    // hexadecimal float number
    <<".", rest:bits>> ->
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
    // exponential
    <<"e+", rest:bits>> | <<"E+", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    <<"e-", rest:bits>> | <<"E-", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    // radix
    <<"p+", rest:bits>> | <<"P+", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    <<"p-", rest:bits>> | <<"P-", rest:bits>> ->
      advance(lexer, rest, 2) |> lex_hexadecimal_float(start, slice + 2)

    <<"p", rest:bits>> | <<"P", rest:bits>> ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    <<"0", rest:bits>>
    | <<"1", rest:bits>>
    | <<"2", rest:bits>>
    | <<"3", rest:bits>>
    | <<"4", rest:bits>>
    | <<"5", rest:bits>>
    | <<"6", rest:bits>>
    | <<"7", rest:bits>>
    | <<"8", rest:bits>>
    | <<"9", rest:bits>>
    | <<"a", rest:bits>>
    | <<"A", rest:bits>>
    | <<"b", rest:bits>>
    | <<"B", rest:bits>>
    | <<"c", rest:bits>>
    | <<"C", rest:bits>>
    | <<"d", rest:bits>>
    | <<"D", rest:bits>>
    | <<"e", rest:bits>>
    | <<"E", rest:bits>>
    | <<"f", rest:bits>>
    | <<"F", rest:bits>> ->
      advance(lexer, rest, 1) |> lex_hexadecimal_float(start, slice + 1)

    _ -> consume_float(lexer, start, slice)
  }
}

fn advance(lexer: Lexer, source: BitArray, offset: Int) -> Lexer {
  Lexer(
    ..lexer,
    source:,
    offset: lexer.offset + offset,
    column: lexer.column + offset,
  )
}

fn advance_line(lexer: Lexer, source: BitArray, offset: Int) -> Lexer {
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

pub fn token_to_bits(token: Token) -> BitArray {
  case token {
    // literals
    Identifier(str) -> bit_array.from_string(str)
    String(str) -> str
    LongString(str) -> <<"[[", str:bits, "]]">>
    Int(str) -> bit_array.from_string(str)
    Float(str) -> bit_array.from_string(str)
    Comment(str) -> <<"--", str:bits>>
    LongComment(str) -> <<"--[[", str:bits, "]]">>

    // keywords
    And -> <<"and">>
    Break -> <<"break">>
    Do -> <<"do">>
    Else -> <<"else">>
    Elseif -> <<"elseif">>
    End -> <<"end">>
    BFalse -> <<"false">>
    For -> <<"for">>
    Function -> <<"function">>
    Goto -> <<"goto">>
    If -> <<"if">>
    In -> <<"in">>
    Local -> <<"local">>
    Nil -> <<"nil">>
    Not -> <<"not">>
    Or -> <<"or">>
    Repeat -> <<"repeat">>
    Return -> <<"return">>
    Then -> <<"then">>
    BTrue -> <<"true">>
    Until -> <<"until">>
    While -> <<"while">>

    // Arithmetic operators
    Plus -> <<"+">>
    Minus -> <<"-">>
    Star -> <<"*">>
    Slash -> <<"/">>
    SlashSlash -> <<"//">>
    Percent -> <<"%">>
    Circumflex -> <<"^">>

    // Relational operators
    EqualEqual -> <<"==">>
    NotEqual -> <<"~=">>
    Greater -> <<">">>
    GreaterEqual -> <<">=">>
    Less -> <<"<">>
    LessEqual -> <<"<=">>

    // Grouping
    LeftParen -> <<"(">>
    RightParen -> <<")">>
    LeftBrace -> <<"{">>
    RightBrace -> <<"}">>
    LeftSquare -> <<"[">>
    RightSquare -> <<"]">>

    // Bitwise operators
    Amper -> <<"&">>
    VBar -> <<"|">>
    Tilde -> <<"~">>
    GreaterGreater -> <<">>">>
    LessLess -> <<"<<">>

    // other operators
    DotDot -> <<"..">>
    Hash -> <<"#">>

    // Punctuation
    Equal -> <<"=">>
    Colon -> <<":">>
    ColonColon -> <<"::">>
    Semicolon -> <<";">>
    Comma -> <<",">>
    Dot -> <<".">>
    DotDotDot -> <<"...">>

    // whitespace
    Space(str) -> str

    EndOfFile -> <<>>

    // Invalid code
    UnterminatedString(str) -> <<"\"", str:bits>>
    UnterminatedLongString(str) -> <<"[[", str:bits>>
    UnterminatedLongComment(str) -> <<"--[[", str:bits>>
    UnexpectedGrapheme(str) -> str
  }
}

type BinaryPattern

@external(erlang, "lug_ffi", "compile_binary_pattern")
@external(javascript, "../lug_ffi.mjs", "compile_binary_pattern")
fn compile_binary_pattern(patterns: List(String)) -> BinaryPattern

@external(erlang, "lug_ffi", "split_before")
@external(javascript, "../lug_ffi.mjs", "split_before")
fn split_before(
  pattern: BinaryPattern,
  string: BitArray,
) -> #(BitArray, BitArray)

fn split_until_new_line(
  lexer: Lexer,
  string: BitArray,
) -> #(BitArray, BitArray) {
  split_before(lexer.new_lines, string)
}

fn drop_byte(string: BitArray) -> BitArray {
  case string {
    <<_byte, rest:bits>> -> rest
    _ -> string
  }
}

@external(erlang, "lug_ffi", "nil")
@external(javascript, "../lug_ffi.mjs", "nil")
