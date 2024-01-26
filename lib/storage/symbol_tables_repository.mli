(** Repository for the symbol tables of the program. Defines C.R.U.D. operations to create/read/update the symbol tables *)

(** Given a program create the symbol tables associated with each possible block. 
    After the first call with the same program this function has no effect *)
val create: Ast.program -> (unit, Location.code_pos * Semantic_errors.t) result

(** Given a location of a block returns the corresponding symbol table *)
val read: Location.code_pos -> (Symbol.t Symbol_table.t, Location.code_pos * Semantic_errors.t) result

(** Returns every symbol table of the program in the order of the blocks in the source code *)
val read_all: unit -> (Symbol.t Symbol_table.t) list

(** Given a location of a block, a location of a declaration, and an update function, updates the symbol table at the given location applying the update function 
    and returning an error with the given location of declaration if the update function returns an error *)
val update: Location.code_pos -> Location.code_pos -> (Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result) -> (unit, Location.code_pos * Semantic_errors.t) result
