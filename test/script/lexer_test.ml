open Microc

let load_file filename =
  let ic = open_in filename in 
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let rec print_tokens lexbuf = 
    match Scanner.next_token lexbuf with
    | Parser.EOF -> print_endline ""
    | x -> print_endline (Parsing.string_of_tokens x); print_tokens lexbuf
  
let test_lexer filename = 
    let source = load_file filename in
    let lexbuf = Lexing.from_string ~with_positions:true source in
    print_tokens lexbuf

let () = 
  let usage_msg = Printf.sprintf "%s <file>" (Sys.argv.(0)) in 
  let filename = ref "" in 
  Arg.parse [] (fun fname -> filename := fname) usage_msg;
  if String.equal !filename "" then
    Arg.usage [] usage_msg
  else
    test_lexer !filename;