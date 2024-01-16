open Semantic_errors
open Base.Result.Let_syntax

module Repository = Map.Make(String)

let actual_program = ref None;;
let repository = ref (Repository.empty);;

let check_array_size id symbol location = match symbol with 
  | Symbol.Var(Types.CompoundType(Types.Array array_info), _) -> 
    let open Types in 
    if (snd(List.hd array_info.sizes) = None) then
      Error(location, SymbolErr(ArrayVarWithoutSize id))
    else 
      Ok()
  | _ -> Ok()


let read_all () = 
  let bindings = Repository.bindings !repository in
  List.map snd bindings

let read location = 
  try Ok(Repository.find (Location.show_code_pos location) !repository)
  with Not_found -> Error(location, SymbolTablesRepositoryErr ScopeNotFound)

let update location update_fun = 
  let%bind scope = read location in 
  match update_fun scope with
  | Ok(updated_scope) -> 
    repository := Repository.add (Location.show_code_pos location) updated_scope !repository;
    Ok()
  | Error(err) -> 
    Error(location, err)

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
  List.fold_left (fun res stmtordec -> (
    let%bind _ = res in
    match stmtordec.node with
    | Ast.Stmt(stmt) -> 
      add_from_stmt stmt current_scope_loc
    | Ast.Dec(typ, id) -> 
      let%bind _ = update current_scope_loc (Symbol_builder.build_var id typ) in 
      let%bind current_scope = read current_scope_loc in 
      let symbol = Symbol_table.lookup id current_scope in 
      check_array_size id symbol stmtordec.loc
  )) (Ok()) stmtordecs

let add_from_fundecl fun_decl = 
  let open Ast in 
  match fun_decl.body with
  | Some body -> (
    match body.node with
    | Block stmtordecs -> (
      let global_scope_loc = Location.dummy_code_pos in
      let%bind global_scope = read global_scope_loc in 
      let fun_scope = Symbol_table.begin_block global_scope in 
      repository := Repository.add (Location.show_code_pos body.loc) fun_scope !repository;
      let%bind _ = update body.loc (Symbol_builder.build_vars fun_decl.formals) in
      add_from_stmtordecs stmtordecs body.loc) 
    | _ -> failwith "Unexpected error: the body of a function must be a block, syntactically"
    )
  | None -> Ok()

let add_global_definition topdecl = 
  let open Ast in
  let global_scope_loc = Location.dummy_code_pos in 
  match topdecl.node with 
  | Ast.Vardec (typ, id, true) -> 
    let%bind _ = update global_scope_loc (Symbol_builder.build_extern_var id typ) in
    let%bind global_scope = read global_scope_loc in 
    let symbol = Symbol_table.lookup id global_scope in 
    check_array_size id symbol topdecl.loc
  | Ast.Vardec (typ, id, false) -> 
    let%bind _ = update global_scope_loc (Symbol_builder.build_var id typ) in 
    let%bind global_scope = read global_scope_loc in 
    let symbol = Symbol_table.lookup id global_scope in 
    check_array_size id symbol topdecl.loc
  | Ast.Fundecl fun_decl -> 
    update global_scope_loc (Symbol_builder.build_fun fun_decl)

let add_rts_functions () = 
  let open Ast in
  let%bind _ = update Location.dummy_code_pos (Symbol_builder.build_fun {
    typ = Ast.TypV;
    fname = "print";
    formals = [
      (Ast.TypI, "x")
    ];
    body = None
  }) in 
  let%bind _ = update Location.dummy_code_pos (Symbol_builder.build_fun {
    typ = Ast.TypI;
    fname = "getint";
    formals = [];
    body = None
  }) in 
  let%bind _ = update Location.dummy_code_pos (Symbol_builder.build_fun {
    typ = Ast.TypV;
    fname = "print_endline";
    formals = [(Ast.TypA(Ast.TypC, None), "str")];
    body = None
  }) in 
  update Location.dummy_code_pos (Symbol_builder.build_fun {
    typ = Ast.TypV;
    fname = "print_float";
    formals = [(Ast.TypF, "x")];
    body = None
  })

let add_from_program program = match program with
| Ast.Prog topdecls -> 
  repository := Repository.add (Location.show_code_pos Location.dummy_code_pos) (Symbol_table.empty_table) !repository;
  let%bind _ = add_rts_functions () in 
  let%bind _ = List.fold_left (fun res topdecl -> 
    let%bind _ = res in 
    add_global_definition topdecl
  ) (Ok()) topdecls in 
  List.fold_left (fun res topdecl -> 
    let open Ast in
    let%bind _ = res in 
    match topdecl.node with
    | Ast.Fundecl fun_decl -> add_from_fundecl fun_decl
    | _ -> res
  ) (Ok()) topdecls

let create program = 
  match (Repository.is_empty !repository || !actual_program <> Some(program)) with 
  | false -> 
    Ok()
  | true -> 
    repository := Repository.empty;
    actual_program := Some(program);
    add_from_program program