type t = 
  | SymbolTableRepositoryErr of repository_err 
  | TypeCheckerErr of type_checker_err 
  | DeadcodeAnalyzerErr of deadcode_err
  | SymbolErr of symbol_err
  [@@deriving show]

and symbol_err = 
  | DuplicateEntry of Ast.identifier * Symbol.t
  [@@deriving show]

and repository_err = 
  | ScopeNotFound
  [@@deriving show]

and type_checker_err =
  | StmtNotVoid 
  | GuardNotBool 
  | WrongReturnType of Ast.identifier * Types.data_type * Types.data_type
  | AssignTypeMismatch of Types.data_type * Types.data_type
  | ArrayPtrNotIntercheangeable 
  | NegToNonNumeric of Types.data_type
  | NotNonBool of Types.data_type
  | WrongBinOpType of Ast.binop
  | NotDeclaredVar of Ast.identifier
  | NotDeclaredFun of Ast.identifier
  | CalledVar of Ast.identifier
  | WrongActualParamsType of Ast.identifier * Types.data_type list * Types.data_type list
  | AccessToFun of Ast.identifier
  | DerefNotPtr
  | IdxNotInt
  | AccIdxToNotArr
  [@@deriving show]

and deadcode_err = Prova

let to_string _error = ""