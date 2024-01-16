type deadcode_info = {
    unreachable_code : Location.code_pos list;
    unused_vars : unused_var list;
}

and unused_var = {
    id : Ast.identifier;
    typ : unused_typ;
    location : Location.code_pos;
}

and unused_typ = Param | Local

exception Deadcode_found of deadcode_info 

module Unreachable_code_detector : sig
  val detect: Ast.program -> Location.code_pos list
end
= 
struct
  let rec detect_in_stmt stmt = 
    let open Ast in 
    match stmt.node with 
    | Ast.Expr _ -> 
      (false, [])
    | Ast.Return _ -> 
      (true, [])
    | Ast.If(_, then_stmt, else_stmt) -> 
      let (is_then_terminator, then_deadcode_positions) = detect_in_stmt then_stmt in 
      let (is_else_terminator, else_deadcode_positions) = detect_in_stmt else_stmt in 
      (is_then_terminator && is_else_terminator, else_deadcode_positions @ then_deadcode_positions)
    | Ast.While(_, body) -> 
      (false, snd(detect_in_stmt body))
    | Ast.Block stmtordecs -> 
      let (is_terminator, deadcode_positions) = List.fold_left (fun (terminator_found, deadcode_positions) stmtordec -> (
        match terminator_found with
        | true -> 
          (true, stmtordec.loc :: deadcode_positions)
        | false -> 
          match stmtordec.node with 
          | Dec _ -> 
            (false, deadcode_positions)
          | Stmt stmt -> 
            let (is_stmt_terminator, deadcode_stmt) = detect_in_stmt stmt in 
            (is_stmt_terminator, deadcode_stmt @ deadcode_positions)
      )) (false, []) stmtordecs in
      (is_terminator, List.rev deadcode_positions)

  let detect_in_topdecl topdecl = 
    let open Ast in 
    match topdecl.node with 
    | Ast.Vardec _ -> []
    | Ast.Fundecl fun_decl -> 
      match fun_decl.body with 
      | None -> []
      | Some body -> snd(detect_in_stmt body)

  let detect program = match program with 
  | Ast.Prog topdecls -> 
    List.fold_left (fun deadcode topdecl -> (
      let topdecl_deadcode = detect_in_topdecl topdecl in 
      match topdecl_deadcode with 
      | [] -> 
        deadcode
      | topdecl_deadcode -> 
        deadcode @ topdecl_deadcode
    )) [] topdecls
end


module Unused_vars_detector : sig 
  val detect: Ast.program -> unused_var list
end
= 
struct
  let detect _program = []
end 

let detect_deadcode ast = 
  let unreachable_code = Unreachable_code_detector.detect ast in 
  let unused_vars = Unused_vars_detector.detect ast in 
  match (unreachable_code, unused_vars) with 
  | ([], []) -> 
    ast
  | _ -> 
    raise (Deadcode_found {
      unreachable_code;
      unused_vars;
    })