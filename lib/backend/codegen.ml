exception Codegen_error of Location.code_pos * string

open Base.Result.Let_syntax

let is_float_op op1 op2 llvm_context = 
  ((Llvm.type_of op1) = (Llvm.float_type llvm_context)) 
  || ((Llvm.type_of op2) = (Llvm.float_type llvm_context))

let adapt_source_primitive_type_to_lltype typ llvm_context = 
  let open Types in
  match typ with
  | Number IntType -> 
    Llvm.i32_type llvm_context
  | Number FloatType -> 
    Llvm.float_type llvm_context
  | BoolType -> 
    Llvm.i1_type llvm_context
  | CharType -> 
    Llvm.i8_type llvm_context 
  | VoidType -> 
    Llvm.void_type llvm_context 

let rec adapt_source_pointer_to_lltype info llvm_context = 
  let open Types in 
  match info.indirection with 
  | 1 -> Llvm.pointer_type (adapt_source_primitive_type_to_lltype info.pointed_type llvm_context)
  | n -> Llvm.pointer_type (adapt_source_pointer_to_lltype ({pointed_type = info.pointed_type; indirection = n - 1}) llvm_context)

let rec adapt_source_array_to_lltype info llvm_context = 
  let open Types in 
  let array_size = (
    match (List.hd info.sizes) with 
    | (_, Some n) -> n
    | (_, None) -> failwith "Unexpected error: size of the array not present!" 
  ) in 
  let elements_type = (
    match info.indirection with 
    | 0 -> 
      adapt_source_primitive_type_to_lltype info.elements_type llvm_context
    | n -> 
      let elements_type = {pointed_type = info.elements_type; indirection = n} in 
      adapt_source_pointer_to_lltype elements_type llvm_context
  ) in
  Llvm.array_type elements_type array_size 

let adapt_source_type_to_lltype typ llvm_context = 
  let open Types in
  match typ with
  | PrimitiveType(typ) -> 
    adapt_source_primitive_type_to_lltype typ llvm_context
  | CompoundType(Pointer ptr_info) -> 
    adapt_source_pointer_to_lltype ptr_info llvm_context
  | CompoundType(Array array_info) -> 
    adapt_source_array_to_lltype array_info llvm_context

let adapt_source_symbol_to_lltype symbol llvm_context = 
  let open Types in 
  match symbol with 
  | Symbol.Var(typ, _) ->
    adapt_source_type_to_lltype typ llvm_context
  | Symbol.Fun(ret_type, params) -> 
    let ret_type = adapt_source_primitive_type_to_lltype ret_type llvm_context in 
    let params_type = List.map (fun param -> 
      adapt_source_type_to_lltype (fst param) llvm_context
    ) params in 
    Llvm.function_type ret_type (Array.of_list params_type)

let rec generate_access_code access llvm_context builder llvalues_table symbol_table = 
  let open Ast in 
  match access.node with 
  | Ast.AccVar id ->
    let address = Symbol_table.lookup id llvalues_table in 
    let value = Llvm.build_load address id builder in 
    return (address, value)
  | Ast.AccDeref expr -> 
    let%bind address = generate_expr_code expr llvm_context builder llvalues_table symbol_table in 
    let value = Llvm.build_load address "derefvalue" builder in 
    return (address, value)
  | Ast.AccIndex(access, expr) -> 
    let%bind (address_of_arr, _) = generate_access_code access llvm_context builder llvalues_table symbol_table in 
    let%bind index = generate_expr_code expr llvm_context builder llvalues_table symbol_table in 
    let index = Llvm.build_sext index (Llvm.i64_type llvm_context) "index" builder in 
    let address = Llvm.build_gep address_of_arr [|( Llvm.const_int (Llvm.i32_type llvm_context) 0) ; index |] "arrayaccess" builder in 
    let value = Llvm.build_load address "load" builder in
    return (address, value)

and generate_expr_code expr llvm_context builder llvalues_table symbol_table = 
  let open Ast in 
  match expr.node with 
  | Ast.Access(access) -> 
    let%bind (address, value) = generate_access_code access llvm_context builder llvalues_table symbol_table in 
    return value
  | Ast.Assign(access, expr) -> 
    let%bind (address, _) = generate_access_code access llvm_context builder llvalues_table symbol_table in 
    let%bind new_value = generate_expr_code expr llvm_context builder llvalues_table symbol_table in 
    let _ = Llvm.build_store new_value address builder in 
    return new_value
  | Ast.Addr(access) -> 
    let%bind (address, _) = generate_access_code access llvm_context builder llvalues_table symbol_table in 
    return address 
  | Ast.ILiteral(i) ->
    return (Llvm.const_int (Llvm.i32_type llvm_context) i)
  | Ast.CLiteral(c) -> 
    return (Llvm.const_int (Llvm.i8_type llvm_context) (Char.code c))
  | Ast.BLiteral(b) -> 
    return (Llvm.const_int (Llvm.i1_type llvm_context) (Bool.to_int b))
  | Ast.SLiteral(s) -> 
    return (Llvm.const_stringz llvm_context s)
  | Ast.FLiteral(f) -> 
    return (Llvm.const_float (Llvm.float_type llvm_context) f)
  | Ast.UnaryOp(Ast.Not, expr) -> 
    let%bind value = generate_expr_code expr llvm_context builder llvalues_table symbol_table in 
    return (Llvm.build_not value "not" builder)
  | Ast.UnaryOp(Ast.Neg, expr) -> 
    let%bind value = generate_expr_code expr llvm_context builder llvalues_table symbol_table in 
    return (Llvm.build_neg value "neg" builder)
  | Ast.BinaryOp(op, expr1, expr2) -> (
    let%bind op1 = generate_expr_code expr1 llvm_context builder llvalues_table symbol_table in 
    let%bind op2 = generate_expr_code expr2 llvm_context builder llvalues_table symbol_table in 
    match op with 
    | Ast.Add -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fadd op1 op2 "fadd" builder)
      else
        return (Llvm.build_add op1 op2 "add" builder))
    | Ast.Sub -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fsub op1 op2 "fsub" builder)
      else
        return (Llvm.build_sub op1 op2 "sub" builder))
    | Ast.Mult -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fmul op1 op2 "fmult" builder)
      else
        return (Llvm.build_mul op1 op2 "mult" builder))
    | Ast.Div -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fdiv op1 op2 "fdiv" builder)
      else
        return (Llvm.build_udiv op1 op2 "div" builder))
    | Ast.Mod -> 
      return (Llvm.build_urem op1 op2 "mod" builder)
    | Ast.Equal -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fcmp Llvm.Fcmp.Oeq op1 op2 "oeq" builder)
      else
        return (Llvm.build_icmp Llvm.Icmp.Eq op1 op2 "eq" builder))
    | Ast.Neq -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fcmp Llvm.Fcmp.One op1 op2 "one" builder)
      else
        return (Llvm.build_icmp Llvm.Icmp.Ne op1 op2 "ne" builder))
    | Ast.Less -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fcmp Llvm.Fcmp.Olt op1 op2 "olt" builder)
      else
        return (Llvm.build_icmp Llvm.Icmp.Slt op1 op2 "slt" builder))
    | Ast.Leq -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fcmp Llvm.Fcmp.Ole op1 op2 "ole" builder)
      else
        return (Llvm.build_icmp Llvm.Icmp.Sle op1 op2 "sle" builder))
    | Ast.Greater -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fcmp Llvm.Fcmp.Ogt op1 op2 "ogt" builder)
      else
        return (Llvm.build_icmp Llvm.Icmp.Sgt op1 op2 "sgt" builder))
    | Ast.Geq -> (
      if (is_float_op op1 op2 llvm_context) then 
        return (Llvm.build_fcmp Llvm.Fcmp.Oge op1 op2 "oge" builder)
      else
        return (Llvm.build_icmp Llvm.Icmp.Sge op1 op2 "sge" builder))
    | Ast.And -> 
      return (Llvm.build_and op1 op2 "and" builder)
    | Ast.Or ->
      return (Llvm.build_or op1 op2 "or" builder)
    )
  | _ -> failwith ""

let rec generate_stmt_code stmt llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block =
  let open Ast in 
  match stmt.node with
  | Ast.If(guard, then_stmt, else_stmt) -> 
    generate_if_code (guard, then_stmt, else_stmt) llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block
  | Ast.While(guard, body) -> 
    generate_while_code (guard, body) llvm_context llvalues_table symbol_table fun_llvalue return_basic_block
  | Ast.Expr expr ->
    let%bind _ = generate_expr_code expr llvm_context builder llvalues_table symbol_table in 
    return builder
  | Ast.Return(Some(return_expr)) ->
    let ret_val_addr = Symbol_table.lookup "ret" llvalues_table in 
    let%bind ret_val = generate_expr_code return_expr llvm_context builder llvalues_table symbol_table in 
    let _ = Llvm.build_store ret_val ret_val_addr builder in 
    let _ = Llvm.build_br return_basic_block builder in 
    return builder
  | Ast.Return(None) -> 
    let _ = Llvm.build_br return_basic_block builder in 
    return builder
  | Ast.Block stmtordecs -> 
    let%bind block_scope = Symbol_tables_repository.read stmt.loc in
    let block_llvalues_table = Symbol_table.begin_block llvalues_table in 
    let%bind builder = generate_stmtordecs_code stmtordecs llvm_context builder block_llvalues_table block_scope fun_llvalue return_basic_block in 
    return builder

and generate_if_code (guard, then_stmt, else_stmt) llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block =
  let%bind guard_val = generate_expr_code guard llvm_context builder llvalues_table symbol_table in 
  let if_end_block = Llvm.append_block llvm_context "if.end" fun_llvalue in
  let if_end_builder = Llvm.builder_at_end llvm_context if_end_block in 
  let then_block = Llvm.append_block llvm_context "if.then" fun_llvalue in 
  let then_builder = Llvm.builder_at_end llvm_context then_block in 
  let%bind then_builder = generate_stmt_code then_stmt llvm_context then_builder llvalues_table symbol_table fun_llvalue return_basic_block in 
  let _ = Llvm.build_br if_end_block then_builder in
  let else_block = Llvm.append_block llvm_context "if.else" fun_llvalue in 
  let else_builder = Llvm.builder_at_end llvm_context else_block in 
  let%bind else_builder = generate_stmt_code else_stmt llvm_context else_builder llvalues_table symbol_table fun_llvalue return_basic_block in 
  let _ = Llvm.build_br if_end_block else_builder in
  let _ = Llvm.build_cond_br guard_val then_block else_block builder in
  return if_end_builder 

and generate_while_code (guard, body) llvm_context llvalues_table symbol_table fun_llvalue return_basic_block = 
  let while_guard_block = Llvm.append_block llvm_context "while.guard" fun_llvalue in 
  let while_guard_builder = Llvm.builder_at_end llvm_context while_guard_block in 
  let while_body_block = Llvm.append_block llvm_context "while.body" fun_llvalue in 
  let while_body_builder = Llvm.builder_at_end llvm_context while_body_block in
  let while_end_block = Llvm.append_block llvm_context "while.end" fun_llvalue in 
  let while_end_builder = Llvm.builder_at_end llvm_context while_end_block in
  let%bind guard_val = generate_expr_code guard llvm_context while_guard_builder llvalues_table symbol_table in 
  let _ = Llvm.build_cond_br guard_val while_body_block while_end_block while_guard_builder in 
  let%bind while_body_builder = generate_stmt_code body llvm_context while_body_builder llvalues_table symbol_table fun_llvalue return_basic_block in
  let _ = Llvm.build_br while_guard_block while_body_builder in 
  return while_end_builder

and generate_stmtordecs_code stmtordecs llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block = 
  let open Ast in
  let%bind (_, builder) = List.fold_left(fun res stmtordec -> 
    let%bind (llvalues_table, builder) = res in 
    match stmtordec.node with 
    | Dec(_, id) -> 
      let local_var = Symbol_table.lookup id symbol_table in 
      let lltype = adapt_source_symbol_to_lltype local_var llvm_context in 
      let llvalue = Llvm.build_alloca lltype id builder in 
      return (Symbol_table.add_entry id llvalue llvalues_table, builder)
    | Stmt stmt -> 
      let%bind builder = generate_stmt_code stmt llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block in
      return (llvalues_table, builder)
  ) (Ok(llvalues_table, builder)) stmtordecs in 
  return builder

let generate_fundecl_code fundecl global_symbol_table global_llvalues_table llvm_module llvm_context = 
  let open Ast in
  match fundecl.body with 
  | None -> 
    return ()
  | Some body -> 
    match body.node with
    | Block stmtordecs -> 
      (let fun_symbol = Symbol_table.lookup fundecl.fname global_symbol_table in 
      match fun_symbol with 
      | Symbol.Var _ ->
        failwith "Unexpected error"
      | Symbol.Fun(ret_typ, formals) -> 
        let fun_llvalue = Symbol_table.lookup fundecl.fname global_llvalues_table in 
        let%bind fun_scope = Symbol_tables_repository.read body.loc in 
        let fun_llvalues_table = Symbol_table.begin_block global_llvalues_table in 
        let fun_basic_block = Llvm.entry_block fun_llvalue in 
        let fun_bb_builder = Llvm.builder_at_end llvm_context fun_basic_block in
        let fun_llvalues_table = List.fold_left (fun fun_llvalues_table (typ, id) -> (
          let lltype = adapt_source_type_to_lltype typ llvm_context in 
          let llvalue = Llvm.build_alloca lltype id fun_bb_builder in 
          Symbol_table.add_entry id llvalue fun_llvalues_table
        )) fun_llvalues_table formals in 
        let ret_val_typ = adapt_source_type_to_lltype (PrimitiveType ret_typ) llvm_context in
        let ret_val_addr = Llvm.build_alloca ret_val_typ "ret_val_addr" fun_bb_builder in 
        let fun_llvalues_table = Symbol_table.add_entry "ret" ret_val_addr fun_llvalues_table in 
        let return_basic_block = Llvm.append_block llvm_context "return" fun_llvalue in 
        let return_bb_builder = Llvm.builder_at_end llvm_context return_basic_block in 
        let ret_val = Llvm.build_load ret_val_addr "ret_val" return_bb_builder in 
        let _ = Llvm.build_ret ret_val return_bb_builder in  
        let%bind _ = generate_stmtordecs_code stmtordecs llvm_context fun_bb_builder fun_llvalues_table fun_scope fun_llvalue return_basic_block in 
        return ())
    | _ -> 
      failwith "Unexpected error"

let generate_global_definitions topdecl global_symbol_table global_llvalues_table llvm_module llvm_context = 
  let open Ast in
  match topdecl.node with 
  | Vardec(_, id, _) ->
    let var_symbol = Symbol_table.lookup id global_symbol_table in 
    let var_lltype = adapt_source_symbol_to_lltype var_symbol llvm_context in 
    let var_llvalue = Llvm.declare_global var_lltype id llvm_module in 
    return (Symbol_table.add_entry id var_llvalue global_llvalues_table)
  | Fundecl fundecl -> 
    let fun_symbol = Symbol_table.lookup fundecl.fname global_symbol_table in 
    let fun_lltype = adapt_source_symbol_to_lltype fun_symbol llvm_context in 
    let fun_llvalue = Llvm.define_function fundecl.fname fun_lltype llvm_module in 
    return (Symbol_table.add_entry fundecl.fname fun_llvalue global_llvalues_table)

let generate_rts_functions global_symbol_table global_llvalues_table llvm_module llvm_context = 
  (* print function *)
  let print_fun = Symbol_table.lookup "print" global_symbol_table in  
  let fun_lltype = adapt_source_symbol_to_lltype print_fun llvm_context in 
  let fun_llvalue = Llvm.define_function "print" fun_lltype llvm_module in 
  let llvalues_table = Symbol_table.add_entry "print" fun_llvalue global_llvalues_table in 
  (* getint function *)
  let getint_fun = Symbol_table.lookup "getint" global_symbol_table in  
  let fun_lltype = adapt_source_symbol_to_lltype getint_fun llvm_context in 
  let fun_llvalue = Llvm.define_function "getint" fun_lltype llvm_module in 
  return (Symbol_table.add_entry "getint" fun_llvalue llvalues_table)

let generate_program_code program llvm_module llvm_context = match program with
| Ast.Prog topdecls -> 
  let%bind global_symbol_table = Symbol_tables_repository.read Location.dummy_code_pos in 
  let%bind global_llvalues_table = generate_rts_functions global_symbol_table (Symbol_table.empty_table) llvm_module llvm_context in 
  let%bind global_llvalues_table = List.fold_left (fun llvalues_table topdecl -> 
    let%bind llvalues_table = llvalues_table in 
    generate_global_definitions topdecl global_symbol_table llvalues_table llvm_module llvm_context
  ) (Ok(global_llvalues_table)) topdecls in 
  List.fold_left (fun res topdecl -> (
    let open Ast in 
    let%bind _ = res in 
    match topdecl.node with 
    | Ast.Vardec _ -> 
      return ()
    | Ast.Fundecl fundecl -> 
      generate_fundecl_code fundecl global_symbol_table global_llvalues_table llvm_module llvm_context
  )) (Ok()) topdecls

let to_llvm_module program =
  let llvm_context = Llvm.global_context() in
  let llvm_module = Llvm.create_module llvm_context "source-code-module" in 
  let res = generate_program_code program llvm_module llvm_context in 
  match res with
  | Error(loc, err) -> 
    raise (Codegen_error(loc, Errors.to_string err))
  | Ok(_) -> 
    llvm_module