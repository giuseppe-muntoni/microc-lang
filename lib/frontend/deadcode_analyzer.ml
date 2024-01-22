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
  val detect: Ast.program -> (Location.code_pos * Semantic_errors.deadcode_type) list
end
= 
struct
  module AccessedSet = Set.Make(String)

  let rec get_accesses access = 
    let open Ast in 
    match access.node with 
    | Ast.AccVar id -> 
      AccessedSet.singleton id 
    | Ast.AccDeref expr -> 
      get_accesses_expr expr
    | Ast.AccIndex(access, expr) -> 
      AccessedSet.union (get_accesses access) (get_accesses_expr expr)

  and get_accesses_expr expr = 
    let open Ast in 
    match expr.node with 
    | Ast.Access access -> 
      get_accesses access 
    | Ast.Assign(access, expr) -> 
      AccessedSet.union (get_accesses access) (get_accesses_expr expr)
    | Ast.Addr access -> 
      get_accesses access
    | Ast.UnaryOp(_, expr) -> 
      get_accesses_expr expr 
    | Ast.BinaryOp(_, expr1, expr2) -> 
      AccessedSet.union (get_accesses_expr expr1) (get_accesses_expr expr2)
    | Ast.Call(_, actuals) -> 
      List.fold_left (fun accessed actual -> (
        AccessedSet.union accessed (get_accesses_expr actual)
      )) AccessedSet.empty actuals
    | _ -> 
      AccessedSet.empty

  and detect_in_stmt stmt fundecl_loc = 
    let open Ast in
    match stmt.node with 
    | Ast.Expr e -> 
      (get_accesses_expr e, [])
    | Ast.Return(Some e) -> 
      (get_accesses_expr e, [])
    | Ast.Return(None) -> 
      (AccessedSet.empty, [])
    | Ast.If(guard, then_stmt, else_stmt) -> 
      let accessed_guard = get_accesses_expr guard in 
      let (accessed_then, dead_then) = detect_in_stmt then_stmt fundecl_loc in 
      let (accessed_else, dead_else) = detect_in_stmt else_stmt fundecl_loc in 
      (AccessedSet.union (AccessedSet.union accessed_guard accessed_then) accessed_else, dead_then @ dead_else)
    | Ast.While(guard, body) -> 
      let accessed_guard = get_accesses_expr guard in 
      let (accessed_body, dead_body) = detect_in_stmt body fundecl_loc in 
      (AccessedSet.union accessed_guard accessed_body, dead_body)
    | Ast.Block stmtordecs -> 
      detect_in_block stmtordecs stmt.loc  fundecl_loc

  and detect_in_block stmtordecs block_loc fundecl_loc = 
    let open Ast in
    let current_scope = Result.get_ok(Symbol_tables_repository.read block_loc) in 
    let result = List.map (fun stmtordec -> (
      let open Ast in 
      match stmtordec.node with 
      | Ast.Stmt stmt -> 
        let (accessed_stmt, dead_stmt) = detect_in_stmt stmt fundecl_loc in 
        (accessed_stmt, dead_stmt, stmtordec.loc)
      | Ast.Dec _ -> 
        (AccessedSet.empty, [], stmtordec.loc)
    )) stmtordecs in 
    let (non_local_accessed, local_accessed, non_local_dead) = List.fold_left (fun (non_local_accessed, local_accessed, dead) (accessed_stmt, dead_stmt, stmt_loc) -> 
      let (non_local_accessed, local_accessed) = AccessedSet.fold (fun var_id (non_local_accessed, local_accessed) -> (
        let var_symbol = Symbol_table.lookup var_id current_scope in
        match var_symbol with 
        | Symbol.GlobalVar _ -> 
          (non_local_accessed, local_accessed)
        | Symbol.LocalVar(_, decl_loc) -> 
          if (Location.compare_code_pos (List.hd stmtordecs).loc decl_loc <= 0 &&
              Location.compare_code_pos stmt_loc decl_loc >= 0) then 
            (non_local_accessed, AccessedSet.add var_id local_accessed)
          else 
            (AccessedSet.add var_id non_local_accessed, local_accessed)
        | Symbol.Fun _ -> 
          failwith "Not possible!"
      )) accessed_stmt (non_local_accessed, local_accessed) in
      (non_local_accessed, local_accessed, dead @ dead_stmt)
    ) (AccessedSet.empty, AccessedSet.empty, []) result in 
    let local_decls = List.map (fun (id, symbol) -> (
      match symbol with 
      | Symbol.LocalVar(_, loc) -> (id, loc)
      | _ -> failwith "Not possible!"
    )) (Symbol_table.to_list current_scope) in 
    let local_decls = List.filter (fun (_, loc) -> loc <> fundecl_loc) local_decls in
    let dead = List.fold_left (fun dead (id_decl, loc) -> (
      if (AccessedSet.exists (fun id_acc -> id_acc = id_decl) local_accessed) then
        dead
      else
        (id_decl, loc) :: dead
    )) non_local_dead local_decls in 
    (non_local_accessed, dead)

  let detect_in_topdecl topdecl = 
    let open Ast in 
    match topdecl.node with
    | Vardec _ -> []
    | Fundecl fundecl -> 
      match fundecl.body with 
      | None -> []
      | Some body -> 
        (* The globals are ignored, so the only accessed vars remained here are function params *)
        let (params_accessed, dead_body) = detect_in_stmt body topdecl.loc in
        let params_ids = List.map snd fundecl.formals in 
        let params_ids = AccessedSet.of_list params_ids in 
        let dead_params = AccessedSet.elements (AccessedSet.diff params_ids params_accessed) in 
        let dead_params = List.map (fun dead_param -> (
          let open Semantic_errors in
          (topdecl.loc, UnusedVar(Param, dead_param))
        )) dead_params in 
        let dead_fun_locals = List.map (fun (id, loc) -> (
          let open Semantic_errors in
          (loc, UnusedVar(Local, id))
        )) dead_body in 
        dead_params @ dead_fun_locals

  let detect program = 
    let open Ast in
    match program with 
    | Prog topdecls -> 
      List.fold_left (fun dead topdecl -> (
        dead @ (detect_in_topdecl topdecl)
      )) [] topdecls
      
end

let detect_deadcode ast = 
  let unreachable_code = Unreachable_code_detector.detect ast in 
  let unreachable_code = List.map (fun pos -> (pos, Semantic_errors.StmtUnreachable)) unreachable_code in 
  let unused_vars = Unused_vars_detector.detect ast in 
  unreachable_code @ unused_vars
    