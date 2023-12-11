/*
* MicroC Parser specification
*/

%{
    (* Auxiliary definitions *)
    open Location
    open Ast

    let (|@|) node loc = { Ast.node = node; Ast.loc = loc }
    
    type varDeclaration = {
      tipo: typ;
      id: identifier;
    }

    type declInfo =
    | Pointer
    | Array of int option
    | IDInfo of string

    let rec getDeclType declInfoList _typ = 
      match declInfoList with
        | [] -> _typ
        | (Array dim) :: xs -> TypA ((getDeclType xs _typ), dim)
        | Pointer :: xs -> TypP (getDeclType xs _typ)
        | (IDInfo _) :: xs -> getDeclType xs _typ

    let rec getDeclID declInfoList = 
      match declInfoList with
        | [] -> ""
        | (IDInfo name) :: _ -> name
        | _ :: xs -> getDeclID xs
%}

/* Tokens declarations */
%token Plus Minus Divided Modulo Equals Different LessThan LessThanOrEq GreaterThan GreaterThanOrEq AndT OrT
%token NotT AddressOf
%token <string>ID
%token <int>ILit
%token <char>CLit
%token <bool>BLit
%token <string>SLit
%token <float>FLit
%token Assign LeftParen RightParen BlockStart BlockEnd IndexStart IndexEnd ExprEnd
%token IfT Else ReturnT While For
%token IntType CharType VoidType BoolType FloatType NULL
%token Star
%token Comma
%token Extern
%token EOF

/* Precedence and associativity specification */
%nonassoc ThenPrec                    /* lowest precedence */
%nonassoc Else
%right    Assign                        
%left     OrT
%left     AndT
%left     Equals  Different 
%nonassoc GreaterThan LessThan  GreaterThanOrEq LessThanOrEq
%left     Plus  Minus
%left     Star  Divided  Modulo
%nonassoc NotT AddressOf
%nonassoc IndexStart Uminus           /* highest precedence */


/* Starting symbol */
%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  | _topdecl = list(topdecl) EOF {Prog _topdecl}

topdecl:
  | _vardec = vardecl ExprEnd {Vardec (_vardec.tipo, _vardec.id, false) |@| to_code_position ($symbolstartpos, $endpos)}
  | Extern _vardec = vardecl ExprEnd {Vardec (_vardec.tipo, _vardec.id, true) |@| to_code_position ($symbolstartpos, $endpos)}
  | _fundecl = fundecl {Fundecl _fundecl |@| to_code_position ($symbolstartpos, $endpos)}

vardecl:
  | _typ = typ _vardesc = vardesc { 
    {
      tipo = getDeclType (List.rev _vardesc) _typ;
      id = getDeclID _vardesc;
    }
  }

vardesc:
  | _id = ID {[IDInfo _id]}
  | Star _vardesc = vardesc { Pointer :: _vardesc }
  | LeftParen _vardesc = vardesc RightParen { _vardesc }
  | _vardesc = vardesc IndexStart IndexEnd { Array None :: _vardesc }
  | _vardesc = vardesc IndexStart dim = ILit IndexEnd { Array (Some dim) :: _vardesc }

fundecl:
  | _typ = typ _id = ID LeftParen _params = separated_list(Comma, vardecl) RightParen _body = block {
    {
      typ = _typ;
      fname = _id;
      formals = List.map (fun x -> (x.tipo, x.id)) _params;
      body = Some _body;
    }
  }
  | _typ = typ _id = ID LeftParen _params = separated_list(Comma, vardecl) RightParen ExprEnd {
    {
      typ = _typ;
      fname = _id;
      formals = List.map (fun x -> (x.tipo, x.id)) _params;
      body = None;
    }
  }

block:
  | BlockStart _blockBody = list(stmtordec) BlockEnd {Block _blockBody |@| to_code_position ($symbolstartpos, $endpos)}

stmtordec:
  | _stmt = stmt {Stmt _stmt |@| to_code_position ($symbolstartpos, $endpos)}
  | _vardec = vardecl ExprEnd {Dec (_vardec.tipo, _vardec.id) |@| to_code_position ($symbolstartpos, $endpos)}

typ:
  | IntType {TypI}
  | FloatType {TypF}
  | CharType {TypC}
  | VoidType {TypV}
  | BoolType {TypB}

stmt:
  | ReturnT _expr = option(expr) ExprEnd {Return _expr |@| to_code_position ($symbolstartpos, $endpos)}
  | _expr = option(expr) ExprEnd {
      match _expr with 
      |  Some expression -> Expr expression |@| to_code_position ($symbolstartpos, $endpos)
      |  None -> Block [] |@| to_code_position ($symbolstartpos, $endpos)
    }
  | _block = block {_block}
  | While LeftParen _expr = expr RightParen _stmt = stmt {Ast.While (_expr, _stmt) |@| to_code_position ($symbolstartpos, $endpos)}
  | For 
      LeftParen 
      _maybeinit = option(expr) ExprEnd _maybeguard = option(expr) ExprEnd _maybeinc = option(expr) 
      RightParen 
      _body = stmt {
        let _init = match _maybeinit with
        |  Some x -> Expr x |@| to_code_position ($symbolstartpos, $endpos)
        |  None -> Block [] |@| to_code_position ($symbolstartpos, $endpos)
        in let _guard = match _maybeguard with
        |  Some x -> x
        |  None -> (BLiteral true) |@| to_code_position ($symbolstartpos, $endpos)
        in let _inc = match _maybeinc with
        |  Some x -> Expr x |@| to_code_position ($symbolstartpos, $endpos)
        |  None -> Block [] |@| to_code_position ($symbolstartpos, $endpos)
      in Block [
        Stmt _init |@| to_code_position ($symbolstartpos, $endpos);
        Stmt (
          Ast.While (
            _guard,
            Block [
              Stmt _body |@| to_code_position ($symbolstartpos, $endpos);
              Stmt _inc |@| to_code_position ($symbolstartpos, $endpos)
            ] |@| to_code_position ($symbolstartpos, $endpos)
          ) |@| to_code_position ($symbolstartpos, $endpos)
        ) |@| to_code_position ($symbolstartpos, $endpos)
      ] |@| to_code_position ($symbolstartpos, $endpos)
    }
  | IfT LeftParen _expr = expr RightParen _then = stmt Else _else = stmt {If (_expr, _then, _else) |@| to_code_position ($symbolstartpos, $endpos)}
  | IfT LeftParen _expr = expr RightParen _then = stmt %prec ThenPrec {If (_expr, _then, Block [] |@| to_code_position ($symbolstartpos, $endpos)) |@| to_code_position ($symbolstartpos, $endpos)}

expr:
  | _rexpr = rexpr {_rexpr}
  | _lexpr = lexpr {Access _lexpr |@| to_code_position ($symbolstartpos, $endpos)}

lexpr:
  | _id = ID {AccVar _id |@| to_code_position ($symbolstartpos, $endpos)}
  | LeftParen _lexpr = lexpr RightParen {_lexpr}
  | Star _lexpr = lexpr {AccDeref (Access _lexpr |@| to_code_position ($symbolstartpos, $endpos)) |@| to_code_position ($symbolstartpos, $endpos)}
  | Star _aexpr = aexpr {AccDeref _aexpr |@| to_code_position ($symbolstartpos, $endpos)}
  | _lexpr = lexpr IndexStart _expr = expr IndexEnd {AccIndex (_lexpr, _expr) |@| to_code_position ($symbolstartpos, $endpos)}

rexpr:
  | _aexpr = aexpr {_aexpr}
  | _id = ID LeftParen _actuals = separated_list(Comma, expr) RightParen {Call (_id, _actuals) |@| to_code_position ($symbolstartpos, $endpos)}
  | _lexpr = lexpr Assign _expr = expr {Ast.Assign (_lexpr, _expr) |@| to_code_position ($symbolstartpos, $endpos)}
  | NotT _expr = expr {UnaryOp (Not, _expr) |@| to_code_position ($symbolstartpos, $endpos)}
  | Minus _expr = expr %prec Uminus {UnaryOp (Neg, _expr) |@| to_code_position ($symbolstartpos, $endpos)}
  | _opl = expr _binop = binop _opr = expr {BinaryOp (_binop, _opl, _opr) |@| to_code_position ($symbolstartpos, $endpos)}

%inline binop:
  | Plus {Add}
  | Minus {Sub}
  | Star {Mult}
  | Modulo {Mod}
  | Divided {Div}
  | AndT {And}
  | OrT {Or}
  | LessThan {Less}
  | GreaterThan {Greater}
  | LessThanOrEq {Leq}
  | GreaterThanOrEq {Geq}
  | Equals {Equal}
  | Different {Neq}

aexpr:
  | _ilit = ILit {ILiteral _ilit |@| to_code_position ($symbolstartpos, $endpos)}
  | _clit = CLit {CLiteral _clit |@| to_code_position ($symbolstartpos, $endpos)}
  | _blit = BLit {BLiteral _blit |@| to_code_position ($symbolstartpos, $endpos)}
  | _slit = SLit {SLiteral _slit |@| to_code_position ($symbolstartpos, $endpos)}
  | _flit = FLit {FLiteral _flit |@| to_code_position ($symbolstartpos, $endpos)}
  | NULL {ILiteral 0 |@| to_code_position ($symbolstartpos, $endpos)}
  | LeftParen _rexpr = rexpr RightParen {_rexpr}
  | AddressOf _lexpr = lexpr {Addr _lexpr |@| to_code_position ($symbolstartpos, $endpos)}
;
