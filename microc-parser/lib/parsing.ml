exception Syntax_error of Location.lexeme_pos * string
let parse _scanner _lexbuf =
  try
    Parser.program _scanner _lexbuf
  with 
  | Scanner.Lexing_error(pos, str) -> raise (Scanner.Lexing_error(pos, str))
  | Parser.Error -> raise (Syntax_error(Location.to_lexeme_position(_lexbuf), "Syntax error"))

let createHashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let string_of_tokens_table = createHashtable 39 [
    (Parser.Plus, "+");
    (Parser.Minus, "-");
    (Parser.Divided, "/");
    (Parser.Modulo, "%");
    (Parser.Equals, "==");
    (Parser.Different, "!=");
    (Parser.LessThan, "<");
    (Parser.LessThanOrEq, "<=");
    (Parser.GreaterThan , ">");
    (Parser.GreaterThanOrEq , ">=");
    (Parser.AndT , "&&");
    (Parser.OrT , "||");
    (Parser.NotT , "!");
    (Parser.AddressOf, "&");
    (Parser.Assign , "=");
    (Parser.LeftParen , "(");
    (Parser.RightParen , ")");
    (Parser.BlockStart , "{");
    (Parser.BlockEnd , "}");
    (Parser.IndexStart , "[");
    (Parser.IndexEnd , "]");
    (Parser.ExprEnd , ";");
    (Parser.IfT , "if");
    (Parser.Else , "else");
    (Parser.ReturnT , "return");
    (Parser.While , "while");
    (Parser.For , "for");
    (Parser.IntType , "int");
    (Parser.CharType , "char");
    (Parser.VoidType , "void");
    (Parser.BoolType , "bool");
    (Parser.NULL , "null");
    (Parser.Star , "*");
    (Parser.Comma , ",");
    (Parser.EOF , "EOF");
  ]

let string_of_tokens token = 
  match token with
  | Parser.ID id -> String.concat " " ["ID:"; id]
  | Parser.ILit i -> String.concat  " " ["ILit:"; string_of_int i]
  | Parser.CLit c -> String.concat " " ["CLit:"; String.make 1 c]
  | Parser.BLit b -> String.concat " " ["BLit:"; string_of_bool b]
  | Parser.SLit s -> String.concat " " ["SLit:"; s]
  | Parser.FLit f -> String.concat " " ["FLit:"; string_of_float f]
  | _ -> Hashtbl.find string_of_tokens_table token