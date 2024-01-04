open Base.Result.Let_syntax
open Errors

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
  let ret_typ = Types.convert_to_data_type fun_decl.typ in 
  match ret_typ with
  | Types.CompoundType _ -> 
    failwith "Unexpected error: a function cannot return syntactically a compound data type"
  | Types.PrimitiveType ret_typ ->
    let formals = List.map (fun (typ, id) -> (Types.convert_to_data_type typ, id)) fun_decl.formals in
    let symbol = Fun(ret_typ, formals) in 
    try
      Ok(Symbol_table.add_entry fun_decl.fname symbol scope)
    with Symbol_table.DuplicateEntry _ ->
      Error(SymbolErr(DuplicateEntry(fun_decl.fname, symbol)))

let _build_var is_extern id typ scope  = 
  let open Symbol in
  let open Types in
  let typ = Types.convert_to_data_type typ in 
  match (is_multidim_array typ, typ = PrimitiveType VoidType) with 
  | (true, false) -> 
    Error(SymbolErr(MultiDimArray id))
  | (false, true) -> 
    Error(SymbolErr(VoidVarDecl id))
  | (false, false) -> (
    let symbol = Var(typ, is_extern) in 
    try
      Ok(Symbol_table.add_entry id symbol scope)
    with Symbol_table.DuplicateEntry _ -> 
      Error(SymbolErr(DuplicateEntry(id, symbol))))
  | (true, true) -> 
    failwith "Not possible!"

let build_var = 
  _build_var false

let build_extern_var = 
  _build_var true

let build_vars types scope = 
  List.fold_left (fun scope (typ, id) -> (
  let%bind scope = scope in 
  build_var id typ scope
)) (Ok(scope)) types