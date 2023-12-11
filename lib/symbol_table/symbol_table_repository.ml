open Errors
open Base.Result.Let_syntax

module Repository = Map.Make(String)

let repository = ref (Repository.empty);;

let read location = 
  try Ok(Repository.find (Location.show_code_pos location) !repository)
  with Not_found -> Error(location, SymbolTableRepositoryErr ScopeNotFound)

let update location update_fun = 
  let%bind scope = read location in 
  try
    let _ = update_fun scope in
    return ()
  with Symbol_table.NotFoundEntry _ ->
    Error(location, SymbolTableRepositoryErr Errors)

let rec add_from_stmt stmt current_scope_loc = match stmt with
| { Ast.loc = stmt_location; Ast.node = stmt } ->
  match stmt with
  | Ast.If(_, then_stmt, else_stmt) ->
    let%bind _ = add_from_stmt then_stmt current_scope_loc in
    add_from_stmt else_stmt current_scope_loc
  | Ast.While(_, body) -> 
    add_from_stmt body current_scope_loc
  | Ast.Block (stmtordecs) -> (
    let%bind upper_scope = read current_scope_loc in 
    let block_scope = Symbol_table.begin_block upper_scope in 
    repository := Repository.add (Location.show_code_pos stmt_location) block_scope !repository;
    add_from_stmtordecs stmtordecs stmt_location)
  | _ -> Ok()

and add_from_stmtordecs stmtordecs current_scope_loc = 
  let open Ast in 
  List.map (fun stmtordec -> stmtordec.node) stmtordecs 
  |> List.fold_left (fun res stmtordec -> (
    let%bind _ = res in
    match stmtordec with
    | Ast.Dec(typ, id) -> update current_scope_loc (Symbol.build_var id typ)
    | Ast.Stmt(stmt) -> add_from_stmt stmt current_scope_loc
  )) (Ok())

let add_from_fundecl fun_decl = 
  let open Ast in 
  let global_scope_loc = Location.dummy_code_pos in 
  let%bind _ = update global_scope_loc (Symbol.build_fun fun_decl) in 
  match fun_decl.body with
  | Some body -> (
    match body.node with
    | Block stmtordecs -> (
      let%bind global_scope = read global_scope_loc in 
      let fun_scope = Symbol_table.begin_block global_scope in 
      repository := Repository.add (Location.show_code_pos body.loc) fun_scope !repository;
      let%bind _ = update body.loc (Symbol.build_vars fun_decl.formals) in
      add_from_stmtordecs stmtordecs body.loc) 
    | _ -> Error(body.loc, SymbolTableRepositoryErr FunBodyIllFormed)
    )
  | None -> Ok()

let add_from_topdecl topdecl = 
  let open Ast in
  let global_scope_loc = Location.dummy_code_pos in 
  match topdecl.node with 
  | Ast.Vardec (typ, id, true) -> 
    update global_scope_loc (Symbol.build_extern_var id typ)
  | Ast.Vardec (typ, id, false) -> 
    update global_scope_loc (Symbol.build_var id typ)
  | Ast.Fundecl fun_decl -> 
    add_from_fundecl fun_decl

let add_from_program program = match program with
| Ast.Prog topdecls -> 
  repository := Repository.add (Location.show_code_pos Location.dummy_code_pos) (Symbol_table.empty_table) !repository;
  List.fold_left (fun res topdecl -> 
    let%bind _ = res in 
    add_from_topdecl topdecl
  ) (Ok()) topdecls

let create program = match Repository.is_empty !repository with 
| false -> Ok()
| true -> add_from_program program