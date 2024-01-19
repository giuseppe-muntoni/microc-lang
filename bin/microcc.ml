open Microc

type action = Parse | Type_check | Dump_llvm_ir | Compile

let[@inline] ( >> ) f g x = g (f x)

let execute_action source_name optimize verify_module = function
  | Parse ->
    Parsing.parse Scanner.next_token
    >> Ast.show_program
    >> Printf.printf "Parsing succeded!\n\n%s\n"
    >> fun _ -> None
  | Type_check ->
    Parsing.parse Scanner.next_token
    >> Semantic_analysis.check_semantic 
    >> Ast.show_program
    >> Printf.printf "Type-check succeded!\n\n%s\n"
    >> fun _ -> None
  | Dump_llvm_ir ->
    Parsing.parse Scanner.next_token
    >> Semantic_analysis.check_semantic 
    >> Codegen.to_llvm_module source_name
    >> (fun llmodule -> 
          if verify_module then 
            Llvm_analysis.assert_valid_module llmodule;
            llmodule
        )
    >> (if optimize then 
          Optimizer.optimize_module 
        else Fun.id)
    >> Llvm.dump_module
    >> fun _ -> None
  | Compile -> 
    (Parsing.parse Scanner.next_token
    >> Semantic_analysis.check_semantic 
    >> Codegen.to_llvm_module source_name
    >> (fun llmodule ->
          if verify_module then 
            Llvm_analysis.assert_valid_module llmodule;
            llmodule
        )
    >> if optimize then Optimizer.optimize_module else Fun.id)
    >> fun llmodule -> Some llmodule

let link_modules modules = 
  match (List.find_opt (fun llmodule -> llmodule = None) modules) with 
  | Some(_) -> None 
  | None -> 
    let modules = List.map Option.get modules in
    let final_module = List.fold_left (
      fun dst src -> (
        Llvm_linker.link_modules' dst src;
        dst
    )) (List.hd modules) (List.tl modules) in 
    Some final_module

let handle_syntatic_error source lexeme_pos msg =
  let lines = String.split_on_char '\n' source in
  let line = List.nth lines (lexeme_pos.Location.line - 1) in
  let prefix = String.make (lexeme_pos.Location.start_column - 1) ' ' in
  let middle =
    String.make
      (lexeme_pos.Location.end_column - lexeme_pos.Location.start_column + 1)
      '^'
  in
  Printf.eprintf "\n*** Error at line %d.\n%s\n%s%s\n*** %s\n\n"
    lexeme_pos.Location.line line prefix middle msg

let handle_semantic_error source code_pos msg =
  let lines =
    String.split_on_char '\n' source
    |> List.filteri (fun line _ ->
           code_pos.Location.start_line - 1 <= line
           && line <= code_pos.Location.end_line - 1)
  in
  let length = List.length lines in
  if length = 1 then
    let line = List.hd lines in
    let prefix = String.make (code_pos.Location.start_column - 1) ' ' in
    let middle =
      String.make
        (code_pos.Location.end_column - code_pos.Location.start_column + 1)
        '^'
    in
    Printf.eprintf "\n*** Error at line %d.\n%s\n%s%s\n*** %s\n\n"
      code_pos.Location.start_line line prefix middle msg
  else
    let text = lines |> List.filteri (fun i _ -> i < 5) |> String.concat "\n" in
    Printf.eprintf "\n*** Error at lines %d-%d.\n%s\n*** %s\n\n"
      code_pos.Location.start_line
      (code_pos.Location.start_line + 5)
      text msg

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let () =
  try
    let action = ref Compile in
    let sources = Array.make 64 "" in
    let n_sources = ref 0 in 
    let outputfile = ref "a.bc" in
    let optimize = ref false in
    let verify = ref false in
    let rts_path = ref "" in
    let spec_list =
      [
        ("-p", 
          Arg.Unit (fun () -> action := Parse), "Parse and print AST");
        ( "-t",
          Arg.Unit (fun () -> action := Type_check),
          "Type checks and print the result" );
        ( "-d",
          Arg.Unit (fun () -> action := Dump_llvm_ir),
          "Compile and print the generated LLVM IR" );
        ( "-c",
          Arg.Unit (fun () -> action := Compile),
          "Compile the sources file and link them together (default)" );
        ( "-o",
          Arg.Set_string outputfile,
          "Place the output into file (default: a.bc)" );
        ( "-O",
          Arg.Set optimize,
          "Optimize the generated LLVM IR (default: false)" );
        ( "-verify",
          Arg.Set verify,
          "Verify the generated LLVM module (default: false)" );
        ( "-rts",
          Arg.Set_string rts_path,
          "The path of the rts bitcode; in combination with -cl is linked with other files" );
      ]
    in
    let usage =
      Printf.sprintf "Usage:\t%s [options] <source_file_1> ...\n" Sys.argv.(0)
    in
    Arg.parse spec_list (fun file -> 
      Array.set sources !n_sources file; 
      n_sources := !n_sources + 1
    ) usage;
    match (!n_sources, !action) with 
    | (0, _) -> 
      Arg.usage spec_list usage
    | (n, Compile) -> (
      let sources = Array.sub sources 0 n in 
      let modules = Array.map (fun filename -> (
        let source = load_file filename in
        let lexbuf = Lexing.from_string ~with_positions:true source in
        try execute_action filename !optimize !verify !action lexbuf with
        | Scanner.Lexing_error (pos, msg) 
        | Parsing.Syntax_error (pos, msg) ->
            print_endline (String.concat " " ["Error in file"; filename]);
            handle_syntatic_error source pos msg;
            None
        | Semantic_analysis.Semantic_errors errors  ->
            print_endline (String.concat " " ["Error in file"; filename]);
            List.iter (fun (pos, msg) -> (
              handle_semantic_error source pos msg;
            )) errors;
            None
      )) sources in 
      let final_module = link_modules (Array.to_list modules) in 
      match final_module with
      | Some final_module -> 
        assert(Llvm_bitwriter.write_bitcode_file final_module "./tmp.bc");
        let args = (
          if (!rts_path = "") then 
            [|"clang"; "./tmp.bc"; "-o"; !outputfile|]
          else 
            [|"clang"; "./tmp.bc"; !rts_path; "-o"; !outputfile|]
        ) in 
        let in_ch = Unix.open_process_args_in "clang" args in 
        let pid = Unix.process_in_pid in_ch in 
        let _ = Unix.waitpid [] pid in
        let _ = Unix.open_process_args_in "rm" [|"rm"; "./tmp.bc"|] in ()
      | None -> ())
    | (1, action) -> (
      let source = load_file (Array.get sources 0) in
      let lexbuf = Lexing.from_string ~with_positions:true source in
      try 
        let _ = execute_action (Array.get sources 0) !optimize !verify action lexbuf in
        ()  
      with
      | Scanner.Lexing_error (pos, msg) 
      | Parsing.Syntax_error (pos, msg) ->
          print_endline (String.concat " " ["Error in file"; (Array.get sources 0)]);
          handle_syntatic_error source pos msg;
      | Semantic_analysis.Semantic_errors errors ->
          print_endline (String.concat " " ["Error in file"; (Array.get sources 0)]);
          List.iter (fun (pos, msg) -> (
              handle_semantic_error source pos msg;
            )) errors;)
    | _ -> 
      Arg.usage spec_list usage
  with Sys_error msg -> Printf.eprintf "*** Error %s ***\n" msg
