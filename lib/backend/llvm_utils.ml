let is_float_op op1 op2 llvm_context = 
  ((Llvm.type_of op1) = (Llvm.float_type llvm_context)) 
  || ((Llvm.type_of op2) = (Llvm.float_type llvm_context))

let convert_to_float value llvm_context builder = 
  if (Llvm.type_of value) = (Llvm.i32_type llvm_context) then 
    Llvm.build_sitofp value (Llvm.float_type llvm_context) "float" builder
  else
    value 

let rec is_bblock_terminated stmt =
  let open Ast in 
  match stmt.node with 
  | Ast.Expr _ -> false
  | Ast.Return _ -> true
  | Ast.If(_, then_stmt, else_stmt) -> is_bblock_terminated then_stmt && is_bblock_terminated else_stmt
  | Ast.While(_, _) -> false
  | Ast.Block stmtordecs -> 
    match (List.find_opt (fun stmtordec -> (
      let open Ast in
      match stmtordec.node with 
      | Ast.Dec _ -> false
      | Ast.Stmt stmt -> is_bblock_terminated stmt 
    ))) stmtordecs with 
    | None -> false
    | Some _ -> true

let rec get_global_init typ llcontext = 
  let open Types in 
  match typ with 
  | PrimitiveType VoidType -> failwith "Not possible!"
  | PrimitiveType BoolType -> Llvm.const_int (Llvm.i1_type llcontext) 0
  | PrimitiveType CharType -> Llvm.const_int (Llvm.i8_type llcontext) 0
  | PrimitiveType Number IntType -> Llvm.const_int (Llvm.i32_type llcontext) 0
  | PrimitiveType Number FloatType -> Llvm.const_float (Llvm.float_type llcontext) 0.0
  | CompoundType Pointer _ -> Llvm.const_pointer_null (Llvm_types_adapter.adapt_type_to_lltype typ llcontext)
  | CompoundType Array array_info -> 
    let array_size = Option.get (snd(List.hd(array_info.sizes))) in 
    match array_info.indirection with 
    | 0 -> 
      let elements_type = Llvm_types_adapter.adapt_type_to_lltype (PrimitiveType array_info.elements_type) llcontext in
      let el_init = get_global_init (PrimitiveType array_info.elements_type) llcontext in 
      let init = Array.make array_size el_init in 
      Llvm.const_array elements_type init
    | n -> 
      let pointer_info = {
        pointed_type = array_info.elements_type;
        indirection = n;
      } in 
      let pointer_lltype = Llvm_types_adapter.adapt_type_to_lltype (CompoundType(Pointer pointer_info)) llcontext in 
      let el_init = get_global_init (CompoundType(Pointer pointer_info)) llcontext in 
      let init = Array.make array_size el_init in 
      Llvm.const_array pointer_lltype init