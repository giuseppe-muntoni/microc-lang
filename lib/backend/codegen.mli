(** Module intended for the generation of the LLVM IR of the source code *)

exception Codegen_error of Location.code_pos * string

(** [llmodule] = to_llvm_module [name] [program] creates the llvm module [llmodule] with name [name] for the program [program] *)
val to_llvm_module : string -> Ast.program -> Llvm.llmodule
