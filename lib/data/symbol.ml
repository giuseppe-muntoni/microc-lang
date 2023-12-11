type is_extern = bool
and t = 
  | Var of Types.data_type * is_extern
  | Fun of Types.primitive_type * Types.data_type list 

let build_fun fun_decl scope =
  let open Ast in 
  let ret_typ = Types.convert_to_data_type fun_decl.typ in 
  match ret_typ with
  | Types.CompoundType _ -> 
    failwith "Unexpected error: a function cannot return syntactically a compound data type"
  | Types.PrimitiveType ret_typ ->
    let formals = List.map fst fun_decl.formals
    |> List.map (fun formal -> Types.convert_to_data_type formal) in
    let symbol = Fun(ret_typ, formals) in 
    Symbol_table.add_entry fun_decl.fname symbol scope

let build_var id typ scope =
  let typ = Types.convert_to_data_type typ in 
  let symbol = Var(typ, false) in 
  Symbol_table.add_entry id symbol scope

let build_extern_var id typ scope =
  let typ = Types.convert_to_data_type typ in 
  let symbol = Var(typ, true) in 
  Symbol_table.add_entry id symbol scope

let build_vars types scope = List.fold_left (fun scope (typ, id) -> (
  build_var id typ scope
)) scope types