(** Module intended to the definition of the abstract data type symbol table *)

exception DuplicateEntry of Ast.identifier
exception NotFoundEntry of Ast.identifier

type 'a t 

val empty_table : 'a t 
val begin_block : 'a t -> 'a t 
val end_block : 'a t -> 'a t
val add_entry : Ast.identifier -> 'a -> 'a t -> 'a t 
val lookup : Ast.identifier -> 'a t -> 'a
val of_alist : (Ast.identifier * 'a) list -> 'a t 
val to_list: 'a t -> (Ast.identifier * 'a) list