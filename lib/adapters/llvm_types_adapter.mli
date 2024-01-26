(** This module is intended to adapt types and symbols of the source code to llvm types *)

(** [lltype] = adapt_symbol_to_lltype [symbol] [llcontext] takes a symbol and convert it to a lltype. 
    A possible symbol can be a function declaration that is converted to the lltype corresponding to the function *)
val adapt_symbol_to_lltype: Symbol.t -> Llvm.llcontext -> Llvm.lltype

(** [lltype] = adapt_param_to_lltype [param_type] [llcontext] generates the corresponding lltype of the function parameter type [param_type] *)
val adapt_param_to_lltype: Types.data_type -> Llvm.llcontext -> Llvm.lltype

(** [lltype] = adapt_type_to_lltype [data_type] [llcontext] generates the corresponding lltype of the type [data_type] *)
val adapt_type_to_lltype: Types.data_type -> Llvm.llcontext -> Llvm.lltype