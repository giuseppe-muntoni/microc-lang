exception Semantic_errors of (Location.code_pos * string) list

let check_semantic ast = 
  let open Base.Result.Let_syntax in
  let res = 
    let%bind _ = Symbol_tables_repository.create ast in
    let%bind _ = Type_checker.check_types ast in
    let%bind _ = Return_analyzer.check_return_presence ast in 
    return ast
  in match res with
  | Ok(ast) -> (
    (* I also check if there is some deadcode *)
    let deadcode = Deadcode_analyzer.detect_deadcode ast in 
    match deadcode with 
    | [] -> 
      ast
    | deadcode -> 
      let errors = List.map (fun (loc, dead_typ) -> (loc, Semantic_errors.to_string(Semantic_errors.DeadcodeFound(dead_typ)))) deadcode in 
      raise (Semantic_errors(errors)))
  | Error(loc, error) -> 
    raise (Semantic_errors([(loc, (Semantic_errors.to_string error))]))
