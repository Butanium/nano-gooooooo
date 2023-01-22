exception Error of Ast.location * string
exception Anomaly of string
(*
   val error : Ast.location -> string -> 'a
   val string_of_type : Tast.typ -> string
   val throw_expected_type : Ast.location -> expected:Tast.typ -> Tast.typ -> 'a
   val check_type : Ast.location -> expected:Tast.typ -> Tast.typ -> unit

   module Henv : sig
     type ('a, 'b) hashtable = ('a, 'b) Hashtbl.t
     type 'b t = { dummy_init : string -> 'b; henv : (string, 'b) hashtable }

     val add : Ast.location -> 'a t -> string -> 'a -> unit
     val add_name : Ast.location -> 'a t -> string -> unit
     val h_create : ?random:bool -> int -> ('a, 'b) Hashtbl.t
     val create : (string -> 'a) -> 'a t
     val find : 'a t -> string -> 'a
     val find_opt : 'a t -> string -> 'a option
     val find_exn : loc:Ast.location -> 'a t -> string -> 'a
   end

   val type_type : Tast.structure Henv.t -> Ast.ptyp -> Tast.typ
   val eq_type : Tast.typ -> Tast.typ -> bool
   val fmt_used : bool ref
   val fmt_imported : bool ref
   val evar : Tast.var -> Tast.expr
   val new_var : string -> Ast.location -> ?used:bool -> Tast.typ -> Tast.var *)
(*
   module Env : sig
     module M : sig
       type key = string
       type 'a t = 'a Lib.M.t

       val empty : 'a t
       val is_empty : 'a t -> bool
       val mem : key -> 'a t -> bool
       val add : key -> 'a -> 'a t -> 'a t
       val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
       val singleton : key -> 'a -> 'a t
       val remove : key -> 'a t -> 'a t

       val merge :
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

       val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
       val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
       val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
       val iter : (key -> 'a -> unit) -> 'a t -> unit
       val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
       val for_all : (key -> 'a -> bool) -> 'a t -> bool
       val exists : (key -> 'a -> bool) -> 'a t -> bool
       val filter : (key -> 'a -> bool) -> 'a t -> 'a t
       val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
       val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
       val cardinal : 'a t -> int
       val bindings : 'a t -> (key * 'a) list
       val min_binding : 'a t -> key * 'a
       val min_binding_opt : 'a t -> (key * 'a) option
       val max_binding : 'a t -> key * 'a
       val max_binding_opt : 'a t -> (key * 'a) option
       val choose : 'a t -> key * 'a
       val choose_opt : 'a t -> (key * 'a) option
       val split : key -> 'a t -> 'a t * 'a option * 'a t
       val find : key -> 'a t -> 'a
       val find_opt : key -> 'a t -> 'a option
       val find_first : (key -> bool) -> 'a t -> key * 'a
       val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
       val find_last : (key -> bool) -> 'a t -> key * 'a
       val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
       val map : ('a -> 'b) -> 'a t -> 'b t
       val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
       val to_seq : 'a t -> (key * 'a) Seq.t
       val to_rev_seq : 'a t -> (key * 'a) Seq.t
       val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
       val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
       val of_seq : (key * 'a) Seq.t -> 'a t
     end

     type t = Tast.var M.t

     val empty : 'a M.t
     val find : M.key -> 'a M.t -> 'a
     val add : Tast.var M.t -> Tast.var -> Tast.var M.t
     val all_vars : Tast.var list ref
     val check_unused : unit -> unit

     val var :
     string ->
       Ast.location ->
         ?used:bool ->
           Tast.typ ->
             Tast.var M.t ->
               Tast.var M.t * Tast.var
             end

             val make : Tast.expr_desc -> Tast.typ -> Tast.expr
             val stmt : Tast.expr_desc -> Tast.expr
             val type_of_const : Ast.constant -> Tast.typ
             val get_unop_type : Ast.location -> Ast.unop -> Tast.expr -> Tast.typ
             val type_expr : Tast.var Env.M.t -> Ast.pexpr -> Tast.expr * bool

             val expr_desc :
             Tast.var Env.M.t ->
               Ast.location ->
                 Ast.pexpr_desc ->
                   Tast.expr_desc * Tast.typ * bool

                   val dummy_function : 'a -> 'b
                   val dummy_structure : string -> Tast.structure
                   val phase1 : 'a Henv.t -> Ast.pdecl -> unit
                   val sizeof : ?loc:Ast.location -> Tast.typ -> int
                   val phase2 : Tast.structure Henv.t -> Tast.function_ Henv.t -> Ast.pdecl -> unit
                   val decl : Tast.structure Henv.t -> 'a -> Ast.pdecl -> Tast.tdecl *)
val unfold_1 : Tast.typ -> Tast.typ
val sizeof : ?loc:Ast.location -> Tast.typ -> int
val tvoid : Tast.typ
val file : debug:bool -> bool * Ast.pdecl list -> Tast.tdecl list
