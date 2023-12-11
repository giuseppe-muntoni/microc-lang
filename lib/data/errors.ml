type t = 
  | SymbolTableRepositoryErr of repository_err 
  | TypeCheckerErr of type_checker_err 
  | DeadcodeAnalyzerErr of deadcode_err 

and repository_err = 
  | ScopeNotFound
  | FunBodyIllFormed
  | Errors

and type_checker_err =
  | StmtNotVoid 
  | IllFormedFunctionBody 
  | GuardNotBool 
  | WrongReturnType 
  | AssignTypeMismatch 
  | ArrayPtrNotIntercheangeable 
  | NegToNonNumeric
  | NotNonBool
  | WrongBinOpType
  | SymbolNotFound
  | CalledVar
  | WrongActualParamsType
  | AccessToFun
  | DerefNotPtr
  | IdxNotInt
  | AccIdxToNotArr

and deadcode_err = Prova

let pretty_print _error = ""