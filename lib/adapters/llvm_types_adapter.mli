val adapt_symbol_to_lltype: Symbol.t -> Llvm.llcontext -> Llvm.lltype
val adapt_param_to_lltype: Types.data_type -> Llvm.llcontext -> Llvm.lltype
val adapt_type_to_lltype: Types.data_type -> Llvm.llcontext -> Llvm.lltype