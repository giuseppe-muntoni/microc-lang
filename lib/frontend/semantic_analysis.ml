exception Semantic_error of Location.code_pos * string

let check_semantic ast = 
  let open Base.Result.Let_syntax in
  let res = 
    let%bind _ = Symbol_table_repository.create ast in
    let%bind _ = Type_checker.check_types ast in
    let%bind ast = Deadcode_analyzer.delete_deadcode ast in
    return ast
  in match res with
  | Ok(ast) -> 
    ast
  | Error(loc, error) -> 
    raise (Semantic_error(loc, Errors.to_string error))
