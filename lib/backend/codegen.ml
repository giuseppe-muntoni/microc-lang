exception Codegen_error of Location.code_pos * string

let rec generate_access_code access llvm_context bblock builder fun_llvalue llvalues_table symbol_table = 
  let open Ast in 
  match access.node with 
  | Ast.AccVar id ->
    let address = Symbol_table.lookup id llvalues_table in
    let value = Llvm.build_load address id builder in 
    let value = 
      (if (Llvm.classify_type (Llvm.type_of value) = Llvm.TypeKind.Array) then 
        let zero_idx = Llvm.const_int (Llvm.i64_type llvm_context) 0 in
        Llvm.build_gep address [| zero_idx; zero_idx |] "arrayaccess" builder
      else
        value) in 
    (address, value)
  | Ast.AccDeref expr -> 
    let (address, _, builder) = generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let value = Llvm.build_load address "derefvalue" builder in 
    (address, value)
  | Ast.AccIndex(access, expr) -> 
    let (address_of_arr, value_of_arr) = generate_access_code access llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let (index, _, builder) = generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let index = Llvm.build_sext index (Llvm.i64_type llvm_context) "index" builder in
    let typ = Llvm.type_of value_of_arr in 
    let address = 
      (if (Llvm.classify_type typ = Llvm.TypeKind.Array) then 
        Llvm.build_gep address_of_arr [|( Llvm.const_int (Llvm.i64_type llvm_context) 0) ; index |] "arrayaccess" builder
      else 
        Llvm.build_gep value_of_arr [| index |] "arrayaccess" builder) in 
    let value = Llvm.build_load address "load" builder in
    (address, value)

and generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table = 
  let open Ast in 
  match expr.node with 
  | Ast.Access(access) -> 
    let (_, value) = generate_access_code access llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    (value, bblock, builder)
  | Ast.Assign(access, expr) -> 
    let (address, _) = generate_access_code access llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let (new_value, bblock, builder) = generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let _ = Llvm.build_store new_value address builder in 
    (new_value, bblock, builder)
  | Ast.Addr(access) -> 
    let (address, _) = generate_access_code access llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    (address, bblock, builder) 
  | Ast.ILiteral(i) ->
    (Llvm.const_int (Llvm.i32_type llvm_context) i, bblock, builder)
  | Ast.CLiteral(c) -> 
    (Llvm.const_int (Llvm.i8_type llvm_context) (Char.code c), bblock, builder)
  | Ast.BLiteral(b) -> 
    (Llvm.const_int (Llvm.i1_type llvm_context) (Bool.to_int b), bblock, builder)
  | Ast.SLiteral(s) ->
    (Llvm.build_global_stringptr s "str" builder, bblock, builder)
  | Ast.FLiteral(f) -> 
    (Llvm.const_float (Llvm.float_type llvm_context) f, bblock, builder)
  | Ast.UnaryOp(Ast.Not, expr) -> 
    let (value, bblock, builder) = generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    (Llvm.build_not value "not" builder, bblock, builder)
  | Ast.UnaryOp(Ast.Neg, expr) -> 
    let (value, bblock, builder) = generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    (Llvm.build_neg value "neg" builder, bblock, builder)
  | Ast.BinaryOp(Ast.And, e1, e2) -> 
    generate_short_circuit_and e1 e2 llvm_context bblock builder fun_llvalue llvalues_table symbol_table
  | Ast.BinaryOp(Ast.Or, e1, e2) -> 
    generate_short_circuit_or e1 e2 llvm_context bblock builder fun_llvalue llvalues_table symbol_table
  | Ast.BinaryOp(op, expr1, expr2) -> (
    let (op1, bblock, builder) = generate_expr_code expr1 llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let (op2, bblock, builder) = generate_expr_code expr2 llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    match op with 
    | Ast.Add -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fadd op1 op2 "fadd" builder, bblock, builder)
      else
        (Llvm.build_add op1 op2 "add" builder, bblock, builder))
    | Ast.Sub -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fsub op1 op2 "fsub" builder, bblock, builder)
      else
        (Llvm.build_sub op1 op2 "sub" builder, bblock, builder))
    | Ast.Mult -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fmul op1 op2 "fmult" builder, bblock, builder)
      else
        (Llvm.build_mul op1 op2 "mult" builder, bblock, builder))
    | Ast.Div -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fdiv op1 op2 "fdiv" builder, bblock, builder)
      else
        (Llvm.build_sdiv op1 op2 "div" builder, bblock, builder))
    | Ast.Mod -> 
      (Llvm.build_srem op1 op2 "mod" builder, bblock, builder)
    | Ast.Equal -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fcmp Llvm.Fcmp.Oeq op1 op2 "oeq" builder, bblock, builder)
      else
        (Llvm.build_icmp Llvm.Icmp.Eq op1 op2 "eq" builder, bblock, builder))
    | Ast.Neq -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fcmp Llvm.Fcmp.One op1 op2 "one" builder, bblock, builder)
      else
        (Llvm.build_icmp Llvm.Icmp.Ne op1 op2 "ne" builder, bblock, builder))
    | Ast.Less -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fcmp Llvm.Fcmp.Olt op1 op2 "olt" builder, bblock, builder)
      else
        (Llvm.build_icmp Llvm.Icmp.Slt op1 op2 "slt" builder, bblock, builder))
    | Ast.Leq -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fcmp Llvm.Fcmp.Ole op1 op2 "ole" builder, bblock, builder)
      else
        (Llvm.build_icmp Llvm.Icmp.Sle op1 op2 "sle" builder, bblock, builder))
    | Ast.Greater -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fcmp Llvm.Fcmp.Ogt op1 op2 "ogt" builder, bblock, builder)
      else
        (Llvm.build_icmp Llvm.Icmp.Sgt op1 op2 "sgt" builder, bblock, builder))
    | Ast.Geq -> (
      if (Llvm_utils.is_float_op op1 op2 llvm_context) then 
        let op1 = Llvm_utils.convert_to_float op1 llvm_context builder in 
        let op2 = Llvm_utils.convert_to_float op2 llvm_context builder in
        (Llvm.build_fcmp Llvm.Fcmp.Oge op1 op2 "oge" builder, bblock, builder)
      else
        (Llvm.build_icmp Llvm.Icmp.Sge op1 op2 "sge" builder, bblock, builder))
    | _ -> 
      failwith "Not possible!"
    )
  | Ast.Call(id, actuals) -> 
    let called_llvalue = Symbol_table.lookup id llvalues_table in 
    let (actuals, bblock, builder) = List.fold_left (fun (others, bblock, builder) actual ->
      let (actual, bblock, builder) = generate_expr_code actual llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
      (actual :: others, bblock, builder)
    ) ([], bblock, builder) actuals in 
    let actuals = List.rev actuals in 
    (Llvm.build_call called_llvalue (Array.of_list actuals) "" builder, bblock, builder)

and generate_short_circuit_and e1 e2 llvm_context and_lhs_bb and_lhs_builder fun_llvalue llvalues_table symbol_table = 
  let and_rhs_bb = Llvm.append_block llvm_context "and.rhs" fun_llvalue in 
  let and_rhs_builder = Llvm.builder_at_end llvm_context and_rhs_bb in
  let and_end_bb = Llvm.append_block llvm_context "and.end" fun_llvalue in 
  let and_end_builder = Llvm.builder_at_end llvm_context and_end_bb in 
  let (op1, and_lhs_bb, and_lhs_builder) = generate_expr_code e1 llvm_context and_lhs_bb and_lhs_builder fun_llvalue llvalues_table symbol_table in 
  let _ = Llvm.build_cond_br op1 and_rhs_bb and_end_bb and_lhs_builder in
  let (op2, and_rhs_bb, and_rhs_builder) = generate_expr_code e2 llvm_context and_rhs_bb and_rhs_builder fun_llvalue llvalues_table symbol_table in 
  let _ = Llvm.build_br and_end_bb and_rhs_builder in 
  let and_res = Llvm.build_phi [
    ((Llvm.const_int (Llvm.i1_type llvm_context) 0), and_lhs_bb);
    (op2, and_rhs_bb);
  ] "and" and_end_builder in 
  (and_res, and_end_bb, and_end_builder)

and generate_short_circuit_or e1 e2 llvm_context or_lhs_bb or_lhs_builder fun_llvalue llvalues_table symbol_table = 
  let or_rhs_bb = Llvm.append_block llvm_context "or.rhs" fun_llvalue in 
  let or_rhs_builder = Llvm.builder_at_end llvm_context or_rhs_bb in
  let or_end_bb = Llvm.append_block llvm_context "or.end" fun_llvalue in 
  let or_end_builder = Llvm.builder_at_end llvm_context or_end_bb in 
  let (op1, or_lhs_bb, or_lhs_builder) = generate_expr_code e1 llvm_context or_lhs_bb or_lhs_builder fun_llvalue llvalues_table symbol_table in 
  let _ = Llvm.build_cond_br op1 or_end_bb or_rhs_bb or_lhs_builder in
  let (op2, or_rhs_bb, or_rhs_builder) = generate_expr_code e2 llvm_context or_rhs_bb or_rhs_builder fun_llvalue llvalues_table symbol_table in 
  let _ = Llvm.build_br or_end_bb or_rhs_builder in 
  let or_res = Llvm.build_phi [
    ((Llvm.const_int (Llvm.i1_type llvm_context) 1), or_lhs_bb);
    (op2, or_rhs_bb);
  ] "or" or_end_builder in 
  (or_res, or_end_bb, or_end_builder)

let rec generate_stmt_code stmt llvm_context bblock builder llvalues_table symbol_table fun_llvalue return_basic_block =
  let open Ast in 
  match stmt.node with
  | Ast.If(guard, then_stmt, else_stmt) -> 
    generate_if_code (guard, then_stmt, else_stmt) llvm_context bblock builder llvalues_table symbol_table fun_llvalue return_basic_block
  | Ast.While(guard, body) -> 
    generate_while_code (guard, body) llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block
  | Ast.Expr expr ->
    let (_, bblock, builder) = generate_expr_code expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    (bblock, builder)
  | Ast.Return(Some(return_expr)) ->
    let ret_val_addr = Symbol_table.lookup "ret" llvalues_table in 
    let (ret_val, bblock, builder) = generate_expr_code return_expr llvm_context bblock builder fun_llvalue llvalues_table symbol_table in 
    let _ = Llvm.build_store ret_val ret_val_addr builder in 
    let _ = Llvm.build_br return_basic_block builder in 
    (bblock, builder)
  | Ast.Return(None) -> 
    let _ = Llvm.build_br return_basic_block builder in 
    (bblock, builder)
  | Ast.Block [] -> 
    (bblock, builder)
  | Ast.Block stmtordecs -> 
    let block_scope = Result.get_ok(Symbol_tables_repository.read stmt.loc) in
    let block_llvalues_table = Symbol_table.begin_block llvalues_table in 
    let (bblock, builder) = generate_stmtordecs_code stmtordecs llvm_context bblock builder block_llvalues_table block_scope fun_llvalue return_basic_block in 
    (bblock, builder)

and generate_if_code (guard, then_stmt, else_stmt) llvm_context guard_block guard_builder llvalues_table symbol_table fun_llvalue return_basic_block =
  let (guard_val, _, guard_builder)  = generate_expr_code guard llvm_context guard_block guard_builder fun_llvalue llvalues_table symbol_table in 
  let then_block = Llvm.append_block llvm_context "if.then" fun_llvalue in 
  let then_builder = Llvm.builder_at_end llvm_context then_block in 
  let (_, then_builder) = generate_stmt_code then_stmt llvm_context then_block then_builder llvalues_table symbol_table fun_llvalue return_basic_block in 
  let else_block = Llvm.append_block llvm_context "if.else" fun_llvalue in 
  let else_builder = Llvm.builder_at_end llvm_context else_block in 
  let (_, else_builder) = generate_stmt_code else_stmt llvm_context else_block else_builder llvalues_table symbol_table fun_llvalue return_basic_block in 
  let _ = Llvm.build_cond_br guard_val then_block else_block guard_builder in
  match (Llvm_utils.is_bblock_terminated then_stmt, Llvm_utils.is_bblock_terminated else_stmt) with 
  | (true, true) -> 
    (* Due to deadcode analysis I expect that there's no code in the block that contains the actual if then else *)
    (else_block, else_builder)
  | (true, false) -> 
    let if_end_block = Llvm.append_block llvm_context "if.end" fun_llvalue in
    let if_end_builder = Llvm.builder_at_end llvm_context if_end_block in 
    let _ = Llvm.build_br if_end_block else_builder in
    (if_end_block, if_end_builder)
  | (false, true) -> 
    let if_end_block = Llvm.append_block llvm_context "if.end" fun_llvalue in
    let if_end_builder = Llvm.builder_at_end llvm_context if_end_block in 
    let _ = Llvm.build_br if_end_block then_builder in
    (if_end_block, if_end_builder)
  | (false, false) ->
    let if_end_block = Llvm.append_block llvm_context "if.end" fun_llvalue in
    let if_end_builder = Llvm.builder_at_end llvm_context if_end_block in 
    let _ = Llvm.build_br if_end_block then_builder in
    let _ = Llvm.build_br if_end_block else_builder in
    (if_end_block, if_end_builder)

and generate_while_code (guard, body) llvm_context builder llvalues_table symbol_table fun_llvalue return_basic_block = 
  let while_guard_block = Llvm.append_block llvm_context "while.guard" fun_llvalue in 
  let while_guard_builder = Llvm.builder_at_end llvm_context while_guard_block in 
  let while_body_block = Llvm.append_block llvm_context "while.body" fun_llvalue in 
  let while_body_builder = Llvm.builder_at_end llvm_context while_body_block in
  let while_end_block = Llvm.append_block llvm_context "while.end" fun_llvalue in 
  let while_end_builder = Llvm.builder_at_end llvm_context while_end_block in
  let _ = Llvm.build_br while_guard_block builder in 
  let (guard_val, _, guard_end_builder) = generate_expr_code guard llvm_context while_guard_block while_guard_builder fun_llvalue llvalues_table symbol_table in 
  let _ = Llvm.build_cond_br guard_val while_body_block while_end_block guard_end_builder in 
  let (_, while_body_builder) = generate_stmt_code body llvm_context while_body_block while_body_builder llvalues_table symbol_table fun_llvalue return_basic_block in
  match Llvm_utils.is_bblock_terminated body with 
  | true -> 
    (while_end_block, while_end_builder)
  | false -> 
    let _ = Llvm.build_br while_guard_block while_body_builder in 
    (while_end_block, while_end_builder)

and generate_stmtordecs_code stmtordecs llvm_context bblock builder llvalues_table symbol_table fun_llvalue return_basic_block = 
  let open Ast in
  let (_, bblock, builder) = List.fold_left(fun res stmtordec -> 
    let (llvalues_table, bblock, builder) = res in 
    match stmtordec.node with 
    | Dec(_, id) -> 
      let local_var = Symbol_table.lookup id symbol_table in 
      let lltype = Llvm_types_adapter.adapt_symbol_to_lltype local_var llvm_context in 
      let llvalue = Llvm.build_alloca lltype id builder in 
      ((Symbol_table.add_entry id llvalue llvalues_table), bblock, builder)
    | Stmt stmt -> 
      let (bblock, builder) = generate_stmt_code stmt llvm_context bblock builder llvalues_table symbol_table fun_llvalue return_basic_block in
      (llvalues_table, bblock, builder)
  ) (llvalues_table, bblock, builder) stmtordecs in 
  (bblock, builder)

let generate_fundecl_code fundecl global_symbol_table global_llvalues_table llvm_context = 
  let open Ast in
  match fundecl.body with 
  | None -> 
    ()
  | Some body -> 
    match body.node with
    | Block stmtordecs -> 
      (let fun_symbol = Symbol_table.lookup fundecl.fname global_symbol_table in 
      match fun_symbol with 
      | Symbol.GlobalVar _ 
      | Symbol.LocalVar _ ->
        failwith "Unexpected error"
      | Symbol.Fun(ret_typ, formals) -> 
        let fun_llvalue = Symbol_table.lookup fundecl.fname global_llvalues_table in 
        let fun_scope = Result.get_ok(Symbol_tables_repository.read body.loc) in 
        let fun_llvalues_table = Symbol_table.begin_block global_llvalues_table in 
        let fun_basic_block = Llvm.entry_block fun_llvalue in 
        let fun_bb_builder = Llvm.builder_at_end llvm_context fun_basic_block in
        let (_, fun_llvalues_table) = List.fold_left (fun (i, fun_llvalues_table) (typ, id) -> (
          let lltype = Llvm_types_adapter.adapt_param_to_lltype typ llvm_context in 
          let param_addr = Llvm.build_alloca lltype id fun_bb_builder in 
          let _ = Llvm.build_store (Llvm.param fun_llvalue i) param_addr fun_bb_builder in
          (i + 1, Symbol_table.add_entry id param_addr fun_llvalues_table)
        )) (0, fun_llvalues_table) formals in  
        match ret_typ with 
        | Types.VoidType -> (
          let return_basic_block = Llvm.append_block llvm_context "return" fun_llvalue in 
          let return_bb_builder = Llvm.builder_at_end llvm_context return_basic_block in 
          let _ = Llvm.build_ret_void return_bb_builder in 
          let (bblock, builder) = generate_stmtordecs_code stmtordecs llvm_context fun_basic_block fun_bb_builder fun_llvalues_table fun_scope fun_llvalue return_basic_block in
          if (Llvm.block_terminator bblock = None) then 
            let _ = Llvm.build_br return_basic_block builder in 
            ()
          else 
            ())
        | _ -> (
          let ret_val_typ = Llvm_types_adapter.adapt_type_to_lltype (Types.PrimitiveType ret_typ) llvm_context in
          let ret_val_addr = Llvm.build_alloca ret_val_typ "ret_val_addr" fun_bb_builder in 
          let fun_llvalues_table = Symbol_table.add_entry "ret" ret_val_addr fun_llvalues_table in 
          let return_basic_block = Llvm.append_block llvm_context "return" fun_llvalue in 
          let return_bb_builder = Llvm.builder_at_end llvm_context return_basic_block in 
          let ret_val = Llvm.build_load ret_val_addr "ret_val" return_bb_builder in 
          let _ = Llvm.build_ret ret_val return_bb_builder in  
          let (bblock, builder) = generate_stmtordecs_code stmtordecs llvm_context fun_basic_block fun_bb_builder fun_llvalues_table fun_scope fun_llvalue return_basic_block in 
          if (fundecl.fname = "main" && Llvm.block_terminator bblock = None) then
            let _ = Llvm.build_store (Llvm.const_int (Llvm.i32_type llvm_context) 0) ret_val_addr builder in 
            let _ = Llvm.build_br return_basic_block builder in 
            ()
          else
            ()))
    | _ -> 
      failwith "Unexpected error"

let generate_global_definitions topdecl global_symbol_table global_llvalues_table llvm_module llvm_context = 
  let open Ast in
  match topdecl.node with 
  | Vardec(_, id, extern) ->
    let var_symbol = Symbol_table.lookup id global_symbol_table in 
    let var_lltype = Llvm_types_adapter.adapt_symbol_to_lltype var_symbol llvm_context in 
    let var_llvalue = (if extern then
      Llvm.declare_global var_lltype id llvm_module
    else 
      (match var_symbol with 
      | Symbol.Fun _ 
      | Symbol.LocalVar _ -> failwith "Not possible!"
      | Symbol.GlobalVar(t, _) -> 
        let global_init = Llvm_utils.get_global_init t llvm_context in 
        Llvm.define_global id global_init llvm_module)
    ) in Symbol_table.add_entry id var_llvalue global_llvalues_table
  | Fundecl fundecl -> 
    let fun_symbol = Symbol_table.lookup fundecl.fname global_symbol_table in 
    let fun_lltype = Llvm_types_adapter.adapt_symbol_to_lltype fun_symbol llvm_context in 
    let fun_llvalue = (match fundecl.body with 
    | Some _ -> Llvm.define_function fundecl.fname fun_lltype llvm_module
    | None -> Llvm.declare_function fundecl.fname fun_lltype llvm_module) in 
    Symbol_table.add_entry fundecl.fname fun_llvalue global_llvalues_table

let generate_rts_functions global_symbol_table global_llvalues_table llvm_module llvm_context =
  (* print function *)
  let print_fun = Symbol_table.lookup "print" global_symbol_table in  
  let fun_lltype = Llvm_types_adapter.adapt_symbol_to_lltype print_fun llvm_context in 
  let fun_llvalue = Llvm.declare_function "print" fun_lltype llvm_module in 
  let llvalues_table = Symbol_table.add_entry "print" fun_llvalue global_llvalues_table in 
  (* getint function *)
  let getint_fun = Symbol_table.lookup "getint" global_symbol_table in  
  let fun_lltype = Llvm_types_adapter.adapt_symbol_to_lltype getint_fun llvm_context in 
  let fun_llvalue = Llvm.declare_function "getint" fun_lltype llvm_module in 
  let llvalues_table = Symbol_table.add_entry "getint" fun_llvalue llvalues_table in 
  (* print_endline function *)
  let print_endline_fun = Symbol_table.lookup "print_endline" global_symbol_table in 
  let fun_lltype = Llvm_types_adapter.adapt_symbol_to_lltype print_endline_fun llvm_context in 
  let fun_llvalue = Llvm.declare_function "print_endline" fun_lltype llvm_module in 
  let llvalues_table = Symbol_table.add_entry "print_endline" fun_llvalue llvalues_table in
  (* print_float function *)
  let print_float_fun = Symbol_table.lookup "print_float" global_symbol_table in 
  let fun_lltype = Llvm_types_adapter.adapt_symbol_to_lltype print_float_fun llvm_context in 
  let fun_llvalue = Llvm.declare_function "print_float" fun_lltype llvm_module in 
  Symbol_table.add_entry "print_float" fun_llvalue llvalues_table

let generate_program_code program llvm_module llvm_context = 
  match program with
  | Ast.Prog topdecls -> 
    let global_symbol_table = Result.get_ok(Symbol_tables_repository.read Location.dummy_code_pos) in 
    let global_llvalues_table = generate_rts_functions global_symbol_table (Symbol_table.empty_table) llvm_module llvm_context in 
    let global_llvalues_table = List.fold_left (fun llvalues_table topdecl -> 
      generate_global_definitions topdecl global_symbol_table llvalues_table llvm_module llvm_context
    ) global_llvalues_table topdecls in 
    List.iter (fun topdecl -> (
      let open Ast in  
      match topdecl.node with 
      | Ast.Vardec _ -> 
        ()
      | Ast.Fundecl fundecl -> 
        generate_fundecl_code fundecl global_symbol_table global_llvalues_table llvm_context
    )) topdecls

let to_llvm_module name program =
  let llvm_context = Llvm.global_context() in
  let llvm_module = Llvm.create_module llvm_context name in 
  let _ = generate_program_code program llvm_module llvm_context in 
  llvm_module