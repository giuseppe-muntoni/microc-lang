(** Module intended to the definition of the various types of errors that can arise during the semantic analysis *)

type t = 
  | SymbolErr of symbol_err
  | SymbolTablesRepositoryErr of repository_err 
  | DeclarationsErr of decl_err
  | TypeCheckerErr of type_checker_err
  | ReturnAnalyzerErr of return_analyzer_err 
  | DeadcodeFound of deadcode_type
[@@deriving show]

and symbol_err = 
  | DuplicateEntry of Ast.identifier * Symbol.t
  | MultiDimArray of Ast.identifier
  | VoidVarDecl of Ast.identifier
  [@@deriving show]

and repository_err = 
  | ScopeNotFound
  [@@deriving show]

and decl_err = 
  | ArrayVarWithoutSize of Ast.identifier
  | NotDeclaredVar of Ast.identifier
  | NotDeclaredFun of Ast.identifier
  | CalledVar of Ast.identifier

and type_checker_err =
  | StmtNotVoid 
  | GuardNotBool 
  | WrongReturnType of Ast.identifier * Types.data_type * Types.data_type
  | AssignTypeMismatch of Types.data_type * Types.data_type
  | ArrNotAssignable of Types.data_type
  | ArrayPtrNotIntercheangeable
  | NegToNonNumeric of Types.data_type
  | NotNonBool of Types.data_type
  | WrongBinOpType of Ast.binop
  | WrongActualParamsType of Ast.identifier * Types.data_type list * Types.data_type list
  | AccessToFun of Ast.identifier
  | DerefNotPtr
  | IdxNotInt
  | AccIdxToNotArr
  [@@deriving show]

and return_analyzer_err = 
  | NoReturn of Ast.identifier
  [@@deriving show] 

and deadcode_type = 
  | StmtUnreachable
  | UnusedVar of unused_type * Ast.identifier
[@@deriving ord, eq]

and unused_type = 
  | Local
  | Param 

(** Generates a string version of the error [error] *)
  let to_string error = match error with 
  | SymbolErr error ->
    (match error with 
    | DuplicateEntry(id, _) -> String.concat " " ["You are trying to define a symbol with name"; id; "that is already used in the current scope"]
    | MultiDimArray id -> String.concat " " ["The variable"; id; "is a multi-dimensional array that is not supported"]
    | VoidVarDecl id -> String.concat " " ["You are trying to define"; id; "as a void variable. It is forbidden."])
  | SymbolTablesRepositoryErr error -> 
    (match error with
    | ScopeNotFound -> "Internal error: the scope requested does not exists")
  | DeclarationsErr error -> 
    (match error with
    | ArrayVarWithoutSize id -> String.concat " " ["The array"; id; "does not specify the size, but is mandatory"]
    | NotDeclaredVar id -> String.concat " " ["You are trying to access a variable called"; id; "that is not declared"]
    | NotDeclaredFun id -> String.concat " " ["You are trying to call a function called"; id; "that is not declared"]
    | CalledVar id -> String.concat " " ["You are trying to call"; id; "that is not a function, but a variable"])
  | TypeCheckerErr error -> 
    (match error with
    | StmtNotVoid -> "A statement must have type void"
    | GuardNotBool -> "The guard is expected to have type bool"
    | WrongReturnType(id, expected_t, actual_t) -> String.concat " " ["The function"; id; "is expected to return the type"; Types.to_string expected_t; "but is returning a value of type"; Types.to_string actual_t]
    | AssignTypeMismatch(var_t, value_t) -> String.concat " " ["You are trying to assign a value of type"; Types.to_string value_t; "to a variable of type"; Types.to_string var_t]
    | ArrNotAssignable(_) -> "You are trying to assign a value to an array, that is not possible"
    | ArrayPtrNotIntercheangeable -> "It is forbidden to use an array as a pointer"
    | NegToNonNumeric t -> String.concat " " ["The - unary operator can be applied only to int and float but here is applied to a"; Types.to_string t]
    | NotNonBool t -> String.concat " " ["The ! unary operator can be applied only to bool but here is applied to a"; Types.to_string t]
    | WrongBinOpType binop -> String.concat " " ["The type of the operands of"; Ast.show_binop binop; "is incorrect"]
    | WrongActualParamsType(id, formals_t, actuals_t) -> 
      let formals_str = String.concat ", " (List.map (Types.to_string) formals_t) in 
      let actuals_str = String.concat ", " (List.map (Types.to_string) actuals_t) in 
      String.concat " " ["The function"; id; "expects the parameters types ("; formals_str; ")"; "but here you passed the parameters types ("; actuals_str; ")"]
    | AccessToFun id -> String.concat " " ["You are trying to read"; id; "that is not a variable, but a function"]
    | DerefNotPtr -> "You are trying to dereferencing a symbol that is not a pointer"
    | IdxNotInt -> "You are trying to indexing an array with a symbol that is not an int"
    | AccIdxToNotArr -> "You are trying to indexing a symbol that is not an array")
  | ReturnAnalyzerErr error -> (
    match error with 
    | NoReturn id -> String.concat " " ["In the function"; id; "exists some execution path for which the return statement is missing"])
  | DeadcodeFound deadcode_type -> 
    match deadcode_type with 
    | StmtUnreachable -> "The statement is unreachable"
    | UnusedVar(Local, id) -> String.concat " " ["The local variable"; id; "is unused"]
    | UnusedVar(Param, id) -> String.concat " " ["The function parameter"; id; "is unused"]

