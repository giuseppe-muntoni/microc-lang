open Base.Result.Let_syntax
open Errors

let build_fun fun_decl scope =
  let open Symbol in
  let open Ast in 
  let ret_typ = Types.convert_to_data_type fun_decl.typ in 
  match ret_typ with
  | Types.CompoundType _ -> 
    failwith "Unexpected error: a function cannot return syntactically a compound data type"
  | Types.PrimitiveType ret_typ ->
    let formals = List.map fst fun_decl.formals
    |> List.map (fun formal -> Types.convert_to_data_type formal) in
    let symbol = Fun(ret_typ, formals) in 
    try
      Ok(Symbol_table.add_entry fun_decl.fname symbol scope)
    with Symbol_table.DuplicateEntry _ ->
      Error(SymbolErr(DuplicateEntry(fun_decl.fname, symbol)))

let build_var id typ scope =
  let open Symbol in
  let typ = Types.convert_to_data_type typ in 
  let symbol = Var(typ, false) in 
  try
    Ok(Symbol_table.add_entry id symbol scope)
  with Symbol_table.DuplicateEntry _ -> 
    Error(SymbolErr(DuplicateEntry(id, symbol)))

let build_extern_var id typ scope =
  let open Symbol in
  let typ = Types.convert_to_data_type typ in 
  let symbol = Var(typ, true) in 
  try
    Ok(Symbol_table.add_entry id symbol scope)
  with Symbol_table.DuplicateEntry _ -> 
    Error(SymbolErr(DuplicateEntry(id, symbol)))

let build_vars types scope = 
  List.fold_left (fun scope (typ, id) -> (
  let%bind scope = scope in 
  build_var id typ scope
)) (Ok(scope)) types