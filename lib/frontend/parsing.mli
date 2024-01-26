(** Module for parsing the source code and generating the corresponding ast *)

exception Syntax_error of Location.lexeme_pos * string

val parse : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.program

val string_of_tokens : Parser.token -> string