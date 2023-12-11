{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let createHashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
        tbl

    let keywordTable =
        createHashtable 11 [
            ("if", IfT);
            ("else", Else);
            ("return", ReturnT);
            ("while", While);
            ("for", For);
            ("int", IntType);
            ("char", CharType);
            ("void", VoidType);
            ("bool", BoolType);
            ("float", FloatType);
            ("NULL", NULL);
            ("extern", Extern)
        ]
}

(* Scanner specification *)
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let intLitBase10 = ['1'-'9']['0'-'9']* | '0'
let intLitHex = '0''x'['0'-'9']* (*WRONG*)
let floatLit = (intLitBase10)'.'['0'-'9']*

rule next_token = parse
    | ['\n']                        {Lexing.new_line lexbuf; next_token lexbuf}
    | [' ' '\t']+                   {next_token lexbuf}
    | "//"                          {single_line_comment lexbuf} 
    | "/*"                          {multi_line_comment lexbuf} 
    | id as word {
        try
            let token = Hashtbl.find keywordTable word in
            token
        with Not_found ->
            ID word
     }
    | '+'                           {Plus}
    | '-'                           {Minus}
    | '*'                           {Star}
    | '/'                           {Divided}
    | '%'                           {Modulo}
    | '&''&'                        {AndT}
    | '|''|'                        {OrT}
    | '=''='                        {Equals}
    | '!''='                        {Different}
    | '<''='                        {LessThanOrEq}
    | '<'                           {LessThan}
    | '>''='                        {GreaterThanOrEq}
    | '>'                           {GreaterThan}
    | '&'                           {AddressOf}
    | '!'                           {NotT}
    | '\''[^'\'']'\'' as charLit    {CLit charLit.[1]}
    | intLitBase10 as intLit        {ILit (int_of_string intLit)}
    | intLitHex as intLitHex        {ILit (int_of_string intLitHex)}
    | floatLit as floatLit          {FLit (float_of_string floatLit)}
    | '\"'[^'\"']*'\"' as stringLit {SLit (String.sub stringLit 1 ((String.length stringLit) - 2))}
    | 't''r''u''e'                  {BLit true}
    | 'f''a''l''s''e'               {BLit false}
    | '='                           {Assign}
    | '('                           {LeftParen}
    | ')'                           {RightParen}
    | '['                           {IndexStart}
    | ']'                           {IndexEnd}
    | '{'                           {BlockStart}
    | '}'                           {BlockEnd}
    | ';'                           {ExprEnd}
    | ','                           {Comma}
    | eof                           {EOF}

and multi_line_comment = parse    
        | "*/"  {next_token lexbuf}
        | '\n'  {Lexing.new_line lexbuf; multi_line_comment lexbuf}
        | _     {multi_line_comment lexbuf}

and single_line_comment = parse 
        | "\n"  {Lexing.new_line lexbuf; next_token lexbuf}
        | _     {single_line_comment lexbuf}