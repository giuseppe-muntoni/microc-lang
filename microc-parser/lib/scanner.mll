{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let createHashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
        tbl

    let keywordTable =
        createHashtable 10 [
            ("if", IfT);
            ("else", Else);
            ("return", ReturnT);
            ("while", While);
            ("for", For);
            ("int", IntType);
            ("char", CharType);
            ("void", VoidType);
            ("bool", BoolType);
            ("NULL", NULL)
        ]
}

(* Scanner specification *)
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let singleLineComment = '/''/'[^'\n']*'\n'
let multilineComment = '/''*'('*'?)[^'*''/']*'*''/'
let intLitBase10 = '-'?['1'-'9']['0'-'9']* | '0'
let intLitHex = '0''x'['0'-'9']*

rule next_token = parse
    | [' ' '\t' '\n']
    | singleLineComment
    | multilineComment {next_token lexbuf}
    | id as word {
        try
            let token = Hashtbl.find keywordTable word in
            token
        with Not_found ->
            ID word
     }
    | '+' { Plus }
    | '-' { Minus }
    | '*' { Star }
    | '/' { Divided }
    | '%' { Modulo }
    | '&''&' { AndT }
    | '|''|' { OrT }
    | '=''=' { Equals}
    | '!''=' { Different}
    | '<''=' { LessThanOrEq}
    | '<' { LessThan}
    | '>''=' { GreaterThanOrEq}
    | '>' { GreaterThan}
    | '&' { AddressOf }
    | '!' { NotT }
    | '\''[^'\'']'\'' as charLit {CLit charLit.[1]}
    | intLitBase10 as intLit {ILit (int_of_string intLit)}
    | intLitHex as intLitHex {ILit (int_of_string intLitHex)}
    | 't''r''u''e' {BLit true}
    | 'f''a''l''s''e' {BLit false}
    | '=' {Assign}
    | '(' {LeftParen}
    | ')' {RightParen}
    | '[' {IndexStart}
    | ']' {IndexEnd}
    | '{' {BlockStart}
    | '}' {BlockEnd}
    | ';' {ExprEnd}
    | ',' {Comma}
    | eof {EOF}