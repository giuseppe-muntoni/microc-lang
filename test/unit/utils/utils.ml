open Microc

let make_path sample = 
  if (String.ends_with ~suffix: ".mc" sample) then
    String.concat "" ["../../../../../test/samples/"; sample]
  else
    String.concat "" ["../../../../../test/samples/"; sample; ".mc"]

let create_ast sample =
  (* TODO: How to resolve this? *)
  let sample_path = make_path sample in
  let ic = open_in sample_path in 
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  let sample = Bytes.to_string s in 
  let lexbuf = Lexing.from_string ~with_positions:true sample in 
  Parsing.parse Scanner.next_token lexbuf

let eq_symbol (id1, symbol1) (id2, symbol2) = id1 = id2 &&
  match (symbol1, symbol2) with 
  | (Symbol.LocalVar(typ1, _), Symbol.LocalVar(typ2, _)) -> 
    typ1 = typ2
  | _ ->
    symbol1 = symbol2 
