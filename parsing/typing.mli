exception Error of Ast.location * string
exception Anomaly of string

val unfold_1 : Tast.typ -> Tast.typ
val sizeof : ?loc:Ast.location -> Tast.typ -> int
val tvoid : Tast.typ
val file : debug:bool -> bool * Ast.pdecl list -> Tast.tdecl list
