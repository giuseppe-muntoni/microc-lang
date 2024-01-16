open Base.Result.Let_syntax

let rec check_return_presence_stmt stmt =
  let open Ast in 
  match stmt.node with 
  | Ast.Expr _ -> false
  | Ast.Return _ -> true
  | Ast.If(_, then_stmt, else_stmt) -> check_return_presence_stmt then_stmt && check_return_presence_stmt else_stmt
  | Ast.While(_, _) -> false
  | Ast.Block stmtordecs -> 
    match (List.find_opt (fun stmtordec -> (
      let open Ast in
      match stmtordec.node with 
      | Ast.Dec _ -> false
      | Ast.Stmt stmt -> check_return_presence_stmt stmt 
    ))) stmtordecs with 
    | None -> false
    | Some _ -> true

let check_return_presence program = match program with 
| Ast.Prog topdecls -> 
  let fundecls = List.filter_map (fun topdecl -> (
    let open Ast in 
    match topdecl.node with
    | Vardec _ -> None
    | Fundecl fun_decl -> 
      match (fun_decl.typ, fun_decl.body, fun_decl.fname) with
      | (_, None, _) -> None 
      | (Ast.TypV, _, _) -> None
      | (_, _, "main") -> None
      | _ -> Some (topdecl.loc, fun_decl)
  )) topdecls in 
  List.fold_left (fun res (location, fundecl) -> (
    let open Ast in
    let%bind _ = res in 
    if (check_return_presence_stmt (Option.get fundecl.body)) then
      return ()
    else
      let open Semantic_errors in 
      Error(location, ReturnAnalyzerErr(NoReturn fundecl.fname))
  )) (Ok()) fundecls