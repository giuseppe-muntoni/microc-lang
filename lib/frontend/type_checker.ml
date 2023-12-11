open Errors
open Base.Result.Let_syntax

let rec check_types_access current_scope access =
  let open Ast in
  match access.node with 
  | Ast.AccVar id -> 
    check_types_accvar current_scope id access.loc
  | Ast.AccDeref expr -> 
    check_types_accderef current_scope expr access.loc
  | Ast.AccIndex(access, expr) ->
    check_types_accindex current_scope (access, expr) access.loc

and check_types_accindex current_scope (access, expr) location = 
  let open Types in
  let%bind access_type = check_types_access current_scope access in
  let%bind idx_type = check_types_expr current_scope expr in
  match (access_type, idx_type) with 
  | (CompoundType(Array arr_info), PrimitiveType Number(IntType)) -> 
    if arr_info.ptr_indirection = 0 then
      return (PrimitiveType arr_info.primitive_type)
    else 
      return (CompoundType(Pointer{
        primitive_type = arr_info.primitive_type;
        ptr_indirection = arr_info.ptr_indirection
      }))
  | (_, PrimitiveType Number(IntType)) ->
    Error(location, TypeCheckerErr AccIdxToNotArr)
  | (CompoundType(Array _), _) -> 
    Error(location, TypeCheckerErr IdxNotInt)
  | _ -> 
    Error(location, TypeCheckerErr AccIdxToNotArr)

and check_types_accderef current_scope expr location = 
  let open Types in
  let%bind expr_typ = check_types_expr current_scope expr in 
  match expr_typ with
  | CompoundType(Pointer{ primitive_type; ptr_indirection = 1 }) -> 
    return (PrimitiveType primitive_type)
  | CompoundType(Pointer{primitive_type; ptr_indirection = n}) -> 
    return (CompoundType(Pointer{primitive_type: primitive_type; ptr_indirection = n-1}))
  | _ -> 
    Error(location, TypeCheckerErr DerefNotPtr)

and check_types_accvar current_scope id location = 
  try
    let symbol = Symbol_table.lookup id current_scope in 
    match symbol with 
    | Symbol.Var(typ, _) -> 
      return typ
    | _ -> 
      Error(location, TypeCheckerErr AccessToFun)
  with Not_found -> 
    Error(location, TypeCheckerErr SymbolNotFound)

and check_types_expr current_scope expr = 
  let open Ast in
  let open Types in
  match expr.node with
  | Ast.Access(access) -> 
    check_types_access current_scope access
  | Ast.Assign(access, expr) ->
    check_types_assign current_scope (access, expr) expr.loc
  | Ast.Addr(access) ->
    check_types_addr current_scope access expr.loc
  | Ast.ILiteral(_) ->
    return (PrimitiveType (Number IntType))
  | Ast.CLiteral(_) -> 
    return (PrimitiveType CharType)
  | Ast.BLiteral(_) -> 
    return (PrimitiveType BoolType)
  | Ast.SLiteral(s) -> 
    return (CompoundType(Array {
      primitive_type = CharType;
      ptr_indirection = 0;
      dimensions = 1; 
      sizes = [(1, Some((String.length s) + 1))]
    }))
  | Ast.FLiteral(_) -> 
    return (PrimitiveType (Number FloatType))
  | Ast.UnaryOp(uop, e) -> 
    check_types_unaryop current_scope (uop, e) expr.loc
  | Ast.BinaryOp(binop, e1, e2) -> 
    check_types_binop current_scope (binop, e1, e2) expr.loc
  | Ast.Call(id, actual_params) -> 
    check_types_call current_scope (id, actual_params) expr.loc

and check_types_assign current_scope (access, expr) location = 
  let open Types in 
  let%bind access_type = check_types_access current_scope access in 
  let%bind expr_type = check_types_expr current_scope expr in 
  match access_type = expr_type with
  | true -> 
    return (PrimitiveType VoidType)
  | false ->
    Error(location, TypeCheckerErr AssignTypeMismatch)

and check_types_addr current_scope access location = 
  let open Types in 
  match%bind (check_types_access current_scope access) with
  | CompoundType(Pointer ptr_info) -> 
    return (CompoundType(Pointer {
      primitive_type = ptr_info.primitive_type;
      ptr_indirection = ptr_info.ptr_indirection + 1
    }))
  | PrimitiveType(t) -> 
    return (CompoundType(Pointer {
      primitive_type = t;
      ptr_indirection = 1
    }))
  | CompoundType(Array(_)) ->
    Error(location, TypeCheckerErr ArrayPtrNotIntercheangeable)

and check_types_unaryop current_scope (uop, e) location = 
  let open Types in
  let%bind expr_type = check_types_expr current_scope e in
  match (uop, expr_type) with
  | (Ast.Neg, PrimitiveType(Number _)) -> 
    return expr_type
  | (Ast.Neg, _) -> 
    Error(location, TypeCheckerErr NegToNonNumeric)
  | (Ast.Not, PrimitiveType BoolType) -> 
    return expr_type
  | (Ast.Not, _) ->
    Error(location, TypeCheckerErr NotNonBool)

and check_types_binop current_scope (binop, e1, e2) location = 
  let%bind e1_type = check_types_expr current_scope e1 in 
  let%bind e2_type = check_types_expr current_scope e2 in 
  let open Types in
  match (binop, e1_type, e2_type) with 
  | (Ast.Add, PrimitiveType (Number t1), PrimitiveType (Number t2))
  | (Ast.Sub, PrimitiveType (Number t1), PrimitiveType (Number t2))
  | (Ast.Mult, PrimitiveType (Number t1), PrimitiveType (Number t2))
  | (Ast.Div, PrimitiveType (Number t1), PrimitiveType (Number t2)) -> 
    return (PrimitiveType (Number (Types.max t1 t2)))
  | (Ast.Mod, PrimitiveType (Number IntType), PrimitiveType (Number IntType)) -> 
    return (PrimitiveType (Number IntType))
  | (Ast.Equal, PrimitiveType (Number _), PrimitiveType (Number _))
  | (Ast.Neq, PrimitiveType (Number _), PrimitiveType (Number _))
  | (Ast.Less, PrimitiveType (Number _), PrimitiveType (Number _))
  | (Ast.Leq, PrimitiveType (Number _), PrimitiveType (Number _))
  | (Ast.Greater, PrimitiveType (Number _), PrimitiveType (Number _))
  | (Ast.Geq, PrimitiveType (Number _), PrimitiveType (Number _)) -> 
    return (PrimitiveType BoolType)
  | (Ast.And, PrimitiveType BoolType, PrimitiveType BoolType)
  | (Ast.Or, PrimitiveType BoolType, PrimitiveType BoolType) -> 
    return (PrimitiveType BoolType)
  | _ -> 
    Error(location, TypeCheckerErr WrongBinOpType)

and check_types_call current_scope (fname, actual_params) location = 
  let open Types in
  let%bind actual_types = (List.fold_left (
    fun actual_types actual_param -> 
      let%bind actual_types = actual_types in
      let%bind actual_type = (check_types_expr current_scope actual_param) in
      return (actual_type::actual_types)
    ) (Ok([])) actual_params) in 
  let actual_types = List.rev actual_types in
  try
    let symbol = Symbol_table.lookup fname current_scope in
    match symbol with
    | Symbol.Fun(return_type, params) -> 
      if (List.equal (=) params actual_types) then
        return(PrimitiveType return_type)
      else 
        Error(location, TypeCheckerErr WrongActualParamsType)
    | _ -> 
      Error(location, TypeCheckerErr CalledVar)
  with Symbol_table.NotFoundEntry _ -> Error(location, TypeCheckerErr SymbolNotFound)

let rec check_types_stmt current_fun current_scope stmt = 
  let open Ast in
  let open Types in
  match stmt.node with
  | Ast.If(guard, then_stmt, else_stmt) -> 
    check_types_if current_fun current_scope (guard, then_stmt, else_stmt)
  | Ast.While(guard, body) -> 
    check_types_while current_fun current_scope (guard, body)
  | Ast.Expr(expr) -> 
    let%bind _ = check_types_expr current_scope expr in 
    return (PrimitiveType VoidType)
  | Ast.Return return -> 
    check_types_return current_fun current_scope return stmt.loc
  | Ast.Block stmtordecs -> 
    check_types_stmtordecs current_fun current_scope stmtordecs

and check_types_return current_fun current_scope return_stmt return_loc = 
  let open Symbol in
  let open Types in
  let ret_type = (
    match current_fun with
    | Fun(ret_type, _) -> PrimitiveType ret_type
    | _ -> failwith "Unexpected error"
  ) in match return_stmt with
  | None -> 
    if ret_type = PrimitiveType VoidType then
      return (PrimitiveType VoidType)
    else 
      Error(return_loc, TypeCheckerErr WrongReturnType) 
  | Some expr -> 
    let%bind expr_type = check_types_expr current_scope expr in 
    if ret_type = expr_type then
      return (PrimitiveType VoidType)
    else 
      Error(return_loc, TypeCheckerErr WrongReturnType)

and check_types_if current_fun current_scope (guard, then_stmt, else_stmt) = 
  let open Ast in
  let open Types in
  match%bind (check_types_expr current_scope guard) with
  | PrimitiveType BoolType -> (
    let%bind then_type = check_types_stmt current_fun current_scope then_stmt in
    let%bind else_type = check_types_stmt current_fun current_scope else_stmt in 
    match (then_type, else_type) with 
    | (PrimitiveType VoidType, PrimitiveType VoidType) -> 
      return (PrimitiveType VoidType)
    | (PrimitiveType VoidType, _) -> 
      Error (else_stmt.loc, TypeCheckerErr StmtNotVoid)
    | _ -> 
      Error (then_stmt.loc, TypeCheckerErr StmtNotVoid))
  | _ -> 
    Error (guard.loc, TypeCheckerErr GuardNotBool)

and check_types_while current_fun current_scope (guard, body) =
  let open Ast in
  let open Types in
  match%bind (check_types_expr current_scope guard) with
  | PrimitiveType BoolType -> (
    match%bind (check_types_stmt current_fun current_scope body) with
    | PrimitiveType VoidType -> 
      return (PrimitiveType VoidType)
    | _ -> 
      Error(body.loc, TypeCheckerErr StmtNotVoid))
  | _ -> 
    Error (guard.loc, TypeCheckerErr GuardNotBool)

and check_types_stmtordecs current_fun current_scope stmtordecs = 
  let open Types in
  List.fold_left (fun res stmtordec -> 
    let open Ast in
    let%bind _ = res in 
    match stmtordec.node with
    | Ast.Dec _ -> 
      return (PrimitiveType VoidType)
    | Ast.Stmt(stmt) -> 
      check_types_stmt current_fun current_scope stmt
  ) (Ok(PrimitiveType VoidType)) stmtordecs

let check_types_fundecl fundecl =
  let open Ast in
  let open Types in
  match fundecl.body with 
  | None ->
    return (PrimitiveType VoidType)
  | Some stmt ->
    match stmt.node with 
    | Block stmtordecs ->
      let%bind global_scope = Symbol_table_repository.read (Location.dummy_code_pos) in 
      let fun_symbol = Symbol_table.lookup fundecl.fname global_scope in 
      let%bind fun_scope = Symbol_table_repository.read stmt.loc in 
      check_types_stmtordecs fun_symbol fun_scope stmtordecs
    | _ -> 
      Error(stmt.loc, TypeCheckerErr IllFormedFunctionBody)

let check_types_topdecl topdecl =
  let open Ast in
  let open Types in
  match topdecl.node with
  | Ast.Fundecl fundecl -> 
    check_types_fundecl fundecl
  | Ast.Vardec _ ->
    return (PrimitiveType VoidType)

let rec check_types program = 
  let open Types in
  match program with
  | Ast.Prog [] -> 
    return ()
  | Ast.Prog (topdecl::topdecls) -> 
    match%bind (check_types_topdecl topdecl) with 
    | PrimitiveType VoidType -> 
      check_types (Ast.Prog topdecls)
    | _ -> 
      let open Ast in Error(topdecl.loc, TypeCheckerErr StmtNotVoid)