open Semantic_errors
open Base.Result.Let_syntax

let check_array_size id symbol location = match symbol with
  | Symbol.GlobalVar(Types.CompoundType(Types.Array array_info), _)
  | Symbol.LocalVar(Types.CompoundType(Types.Array array_info), _) -> 
    let open Types in 
    if (snd(List.hd array_info.sizes) = None) then
      Error(location, DeclarationsErr(ArrayVarWithoutSize id))
    else 
      Ok()
  | _ -> Ok()

let rec check_accesses_acc access current_scope = 
  let open Ast in 
  match access.node with 
  | Ast.AccVar id -> (
    try
      let _ = Symbol_table.lookup id current_scope in 
      Ok()
    with Symbol_table.NotFoundEntry _ -> 
      Error(access.loc, DeclarationsErr(NotDeclaredVar(id))))
  | Ast.AccDeref expr -> 
    check_accesses expr current_scope
  | Ast.AccIndex(access, expr) -> 
    let%bind _ = check_accesses_acc access current_scope in 
    check_accesses expr current_scope

and check_accesses expr current_scope = 
  let open Ast in 
  match expr.node with 
  | Ast.Access(access) -> 
    check_accesses_acc access current_scope
  | Ast.Assign(access, expr) -> 
    let%bind _ = check_accesses_acc access current_scope in 
    check_accesses expr current_scope
  | Ast.Addr(access) ->
    check_accesses_acc access current_scope
  | Ast.UnaryOp(_, expr) ->
    check_accesses expr current_scope
  | Ast.BinaryOp(_, expr1, expr2) -> 
    let%bind _ = check_accesses expr1 current_scope in 
    check_accesses expr2 current_scope
  | Ast.Call(id, actuals) -> (
    try
      let symbol = Symbol_table.lookup id current_scope in 
      match symbol with 
      | Symbol.Fun _ -> (
        List.fold_left (fun res actual -> (
          let%bind _ = res in 
          check_accesses actual current_scope
        )) (Ok()) actuals)
      | _ -> 
        Error(expr.loc, DeclarationsErr(CalledVar(id)))
    with Symbol_table.NotFoundEntry _ ->
      Error(expr.loc, DeclarationsErr(NotDeclaredFun(id))))
  | _ -> Ok()