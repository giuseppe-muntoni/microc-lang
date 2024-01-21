open Base.Result.Let_syntax
open Semantic_errors

type var_scope = LocalScope | GlobalScope

let is_multidim_array data_type = 
  let open Types in 
  match data_type with 
  | CompoundType(Array(array_info)) ->
    array_info.dimensions > 1 
  | _ -> 
    false

let build_fun fun_decl scope =
  let open Symbol in
  let open Ast in 
  let ret_typ = Frontend_types_adapter.adapt_ast_type fun_decl.typ in 
  match ret_typ with
  | Types.CompoundType _ -> 
    failwith "Unexpected error: a function cannot return syntactically a compound data type"
  | Types.PrimitiveType ret_typ ->
    let formals = List.map (fun (typ, id) -> (Frontend_types_adapter.adapt_ast_type typ, id)) fun_decl.formals in
    let symbol = Fun(ret_typ, formals) in 
    try
      Ok(Symbol_table.add_entry fun_decl.fname symbol scope)
    with Symbol_table.DuplicateEntry _ ->
      Error(SymbolErr(DuplicateEntry(fun_decl.fname, symbol)))

let _build_var is_extern location var_scope id typ scope  = 
  let open Symbol in
  let open Types in
  let typ = Frontend_types_adapter.adapt_ast_type typ in 
  match (is_multidim_array typ, typ = PrimitiveType VoidType) with 
  | (true, false) -> 
    Error(SymbolErr(MultiDimArray id))
  | (false, true) -> 
    Error(SymbolErr(VoidVarDecl id))
  | (false, false) -> (
    let symbol = (match var_scope with 
    | LocalScope -> 
      LocalVar(typ, location)
    | GlobalScope -> 
      GlobalVar(typ, is_extern)) in 
    try
      Ok(Symbol_table.add_entry id symbol scope)
    with Symbol_table.DuplicateEntry _ -> 
      Error(SymbolErr(DuplicateEntry(id, symbol))))
  | (true, true) -> 
    failwith "Not possible!"

let build_local_var id typ location scope = 
  _build_var false location LocalScope id typ scope

let build_global_extern_var = 
  _build_var true Location.dummy_code_pos GlobalScope

let build_global_var = 
  _build_var false Location.dummy_code_pos GlobalScope

let build_local_vars types location scope = 
  List.fold_left (fun scope (typ, id) -> (
  let%bind scope = scope in 
  build_local_var id typ location scope
)) (Ok(scope)) types