open Microc

let create_ast sample =
  (* TODO: How to resolve this? *)
  let sample_path = String.concat "" ["../../../../../test/samples/"; sample] in
  let ic = open_in sample_path in 
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  let sample = Bytes.to_string s in 
  let lexbuf = Lexing.from_string ~with_positions:true sample in 
  Parsing.parse Scanner.next_token lexbuf
