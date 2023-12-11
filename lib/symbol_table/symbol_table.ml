exception DuplicateEntry of Ast.identifier
exception NotFoundEntry of Ast.identifier

module SymbolsMap = Map.Make(String);;

type 'a t = Scope of 'a SymbolsMap.t * 'a t option

let empty_table = Scope(SymbolsMap.empty, None)

let begin_block upper_scope  = Scope(SymbolsMap.empty, Some(upper_scope))

let end_block current_scope = match current_scope with
  | Scope(_, Some(upper_scope)) -> upper_scope
  | Scope(_, None) -> failwith "This is the toplevel scope"

let add_entry ide symbol current_scope = match current_scope with
  | Scope(symbol_table, upper_scope) -> match (SymbolsMap.find_opt ide symbol_table) with
    | Some(_) -> raise (DuplicateEntry ide)
    | None -> Scope(SymbolsMap.add ide symbol symbol_table, upper_scope)

let rec lookup ide current_scope = match current_scope with
  | Scope(symbol_table, Some upper_scope) -> (match (SymbolsMap.find_opt ide symbol_table) with 
    | Some(symbol) -> symbol 
    | None -> lookup ide upper_scope
  )
  | Scope(symbol_table, None) -> (match (SymbolsMap.find_opt ide symbol_table) with 
    | Some(symbol) -> symbol 
    | None -> raise (NotFoundEntry ide)
  )

let rec of_alist_aux (symbols, scope_acc) =  match (symbols, scope_acc) with
  | ([], _) -> scope_acc
  | ((ide, symbol) :: xs, Scope(symbol_table, upper_scope)) -> of_alist_aux (xs, Scope(SymbolsMap.add ide symbol symbol_table, upper_scope))

let of_alist symbols = of_alist_aux (symbols, Scope(SymbolsMap.empty, None))