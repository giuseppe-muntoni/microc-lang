type deadcode_info = {
    unreachable_code : Location.code_pos list;
    unused_vars : unused_var list;
}

and unused_var = {
    id : Ast.identifier;
    typ : unused_typ;
    location : Location.code_pos;
}

and unused_typ = Param | Local

exception Deadcode_found of deadcode_info 

val detect_deadcode : Ast.program -> Ast.program