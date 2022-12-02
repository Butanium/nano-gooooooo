open Format
open Lib
open Ast
open Tast

let debug = ref false
let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Error of Ast.location * string
exception ErrorExpr of Ast.location * string * pexpr_desc
exception Anomaly of string

let error loc e = raise (Error (loc, e))
let error_expr loc e expr = raise (ErrorExpr (loc, e, expr))
(* TODO environnement pour les types structure *)

(* TODO environnement pour les fonctions *)

let rec string_of_type = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tptr t -> "*" ^ string_of_type t
  | Twild -> "_"
  | Tstruct { s_name; _ } -> "struct " ^ s_name
  | Tmany t ->
      List.fold_left (fun acc t -> acc ^ string_of_type t ^ " ") "[" t ^ "]"

let throw_expected_type loc ~expected typ =
  error loc
    (Printf.sprintf "expected type %s, got %s instead" (string_of_type expected)
       (string_of_type typ))

let check_type loc ~expected typ =
  if expected <> typ then throw_expected_type loc ~expected typ

module Henv = struct
  open Hashtbl

  type ('a, 'b) hashtable = ('a, 'b) t
  type 'b t = { dummy_init : string -> 'b; henv : (string, 'b) hashtable }

  let add loc env name value =
    if mem env.henv name then
      error loc (Printf.sprintf "%s has been declared twice " name);
    add env.henv name value

  let add_name loc env name = add loc env name (env.dummy_init name)
  let h_create = create
  let create dummy_init = { dummy_init; henv = h_create 10 }
  let find env = find env.henv
  let find_opt env = find_opt env.henv

  let find_exn loc env k =
    match find_opt env k with
    | None -> error loc (Printf.sprintf "undeclared name: %s" k)
    | Some v -> v
end

let rec type_type env_struct = function
  | PTident { id = "int"; _ } -> Tint
  | PTident { id = "bool"; _ } -> Tbool
  | PTident { id = "string"; _ } -> Tstring
  | PTident { id; loc } -> Tstruct (Henv.find_exn loc env_struct id)
  | PTptr ty -> Tptr (type_type env_struct ty)
(* | _ -> error dummy_loc "unknown struct " *)
(* TODO type structure *)

let rec eq_type ty1 ty2 =
  match (ty1, ty2) with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
(* TODO autres types *)

let fmt_used = ref false
let fmt_imported = ref false
let evar v = { expr_desc = TEident v; expr_type = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used = false) ty ->
    incr id;
    {
      v_name = x;
      v_id = !id;
      v_loc = loc;
      v_typ = ty;
      v_used = used;
      v_addr = 0;
      v_depth = 0;
    }

module Env = struct
  module M = Map.Make (String)

  type t = var M.t

  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env
  let all_vars = ref []

  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then
        error v.v_loc "unused variable"
    in
    List.iter check !all_vars

  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    (add env v, v)

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_type = ty }
let stmt d = make d tvoid

let type_of_const = function
  | Cint _ -> Tint
  | Cbool _ -> Tbool
  | Cstring _ -> Tstring

let get_unop_type loc unop expr =
  match unop with
  | Uneg ->
      check_type loc ~expected:Tint expr.expr_type;
      Tint
  | Unot ->
      check_type loc ~expected:Tbool expr.expr_type;
      Tbool
  | Ustar -> (
      match expr.expr_type with
      | Tptr t -> t
      | _ -> throw_expected_type loc ~expected:(Tptr Twild) expr.expr_type)
  | Uamp -> Tptr expr.expr_type

let rec type_expr env e =
  let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  ({ expr_desc = e; expr_type = ty }, rt)

(**  returns the expr, the type of the expr and if a format import is needed
*)
and expr_desc env loc expr_p =
  match expr_p with
  | PEskip -> (TEskip, tvoid, false)
  | PEconstant c -> (TEconstant c, type_of_const c, false)
  | PEbinop (op, e1, e2) ->
      let ({ expr_type = typ1; _ } as e1), need_f1 = type_expr env e1 in
      let ({ expr_type = typ2; _ } as e2), need_f2 = type_expr env e2 in
      if typ1 = typ2 && typ1 = Tint then
        (TEbinop (op, e1, e2), Tint, need_f1 || need_f2)
      else
        error_expr loc
          (Printf.sprintf
             "type error in binary operation, expected type Int, got %s and %s"
             (string_of_type typ1) (string_of_type typ2))
          expr_p
  | PEunop (op, e1) ->
      let expr, fmt = type_expr env e1 in
      (TEunop (op, expr), get_unop_type loc op expr, fmt)
  | PEcall ({ id = "fmt.Print"; _ }, el) ->
      let el = List.map (fun e -> type_expr env e |> fst) el in
      (TEprint el, tvoid, true)
      (* TODO : check si je suis sensé mettre true ou false *)
  | PEcall ({ id = "new"; _ }, [ { pexpr_desc = PEident { id; _ }; _ } ]) ->
      let ty =
        match id with
        | "int" -> Tint
        | "bool" -> Tbool
        | "string" -> Tstring
        | _ ->
            (* TODO que faire si c'est un type custom *)
            error loc ("no such type " ^ id)
      in
      (TEnew ty, Tptr ty, false)
      (* declaration d'un pointeur ty *)
  | PEcall ({ id = "new"; _ }, _) -> error loc "new expects a type"
  | PEcall (id, el) -> (* TODO *) assert false
  | PEfor (e, b) ->
      let b, fmt = type_expr env b in
      check_type loc ~expected:Tbool b.expr_type;
      let e, fmt2 = type_expr env e in
      (TEfor (e, b), tvoid, fmt || fmt2)
  | PEif (e1, e2, e3) ->
      let f = type_expr env in
      let (e1, fmt1), (e2, fmt2), (e3, fmt3) = (f e1, f e2, f e3) in
      check_type loc ~expected:Tbool e1.expr_type;
      (TEif (e1, e2, e3), tvoid, fmt1 || fmt2 || fmt3)
  | PEnil -> (TEnil, tvoid, false)
  | PEident { id; _ } -> (
      (* TODO *)
      try
        let v = Env.find id env in
        (TEident v, v.v_typ, false)
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) -> (* TODO *) assert false
  | PEassign (lvl, el) -> (* TODO *) (TEassign ([], []), tvoid, false)
  | PEreturn el -> (* TODO *) (TEreturn [], tvoid, true)
  | PEblock el -> (* TODO *) (TEblock [], tvoid, false)
  | PEincdec (e, op) ->
      let e, fmt = type_expr env e in
      check_type loc ~expected:Tint e.expr_type;
      (TEincdec (e, op), tvoid, fmt)
  | PEvars _ -> (* TODO *) assert false

let found_main = ref false
let dummy_function _ = assert false

let dummy_structure s_name =
  {
    s_name;
    s_fields = Hashtbl.create 10;
    s_size = -1;
    (* taille calculee en octets *)
  }

(* 1. declare structures *)
let phase1 env = function
  | PDstruct { ps_name = { id; loc }; _ } -> Henv.add_name loc env id
  | PDfunction _ -> ()

let rec sizeof ?(loc = dummy_loc) = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tstruct ({ s_size; s_name; s_fields } as strukt) ->
      if s_size = -2 then
        error loc
          (Printf.sprintf "Error : cyclic dependancy of types in type %s" s_name)
      else if s_size = -1 then (
        strukt.s_size <- -2;
        let size =
          Hashtbl.fold
            (fun _ field size -> size + sizeof field.f_typ)
            s_fields 0
        in
        strukt.s_size <- size;
        size)
      else s_size
        (* TODO : ensure offset has no influence here
           (we saw that in C you have offset when a field size is not a 8 mutiple )*)
  | _ -> (* TODO *) failwith "size of a non implemented type"

(* 2. declare functions and type fields *)
let phase2 structs funs = function
  | PDfunction { pf_name = { id; loc }; pf_params; pf_return_types; _ } ->
      let fn_params =
        List.fold_left
          (fun (v_id, vars) ({ id = v_name; loc = v_loc }, typ) ->
            ( v_id + 1,
              {
                v_name;
                v_id;
                v_loc;
                v_typ = type_type structs typ;
                v_depth = -1;
                v_used = false;
                v_addr = 0;
                (* dummy init *)
              }
              :: vars ))
          (1, []) pf_params
        |> snd |> List.rev
      in
      let return_types = List.map (type_type structs) pf_return_types in
      (* todo what is v_depth*)
      Henv.add loc funs id { fn_name = id; fn_params; return_types }
  | PDstruct { ps_name = { id; _ }; ps_fields } ->
      let structure = Henv.find structs id in
      List.iter
        (fun (ident, ptyp) ->
          let f_typ = type_type structs ptyp in
          Hashtbl.add structure.s_fields ident.id
            { f_name = ident.id; f_typ; f_ofs = 0 })
        ps_fields

(* 3. type check function bodies and compute size and offsets for sturct, also compute v_addr *)
let decl = function
  | PDfunction
      { pf_name = { id; loc; _ }; pf_body = e; pf_return_types = tyl; _ } ->
      (* TODO check name and type *)
      let f = { fn_name = id; fn_params = []; return_types = [] } in
      let e, rt = type_expr Env.empty e in
      TDfunction (f, e)
  | PDstruct { ps_name = { id; _ }; _ } ->
      (* TODO *)
      let s = { s_name = id; s_fields = Hashtbl.create 5; s_size = 0 } in
      TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)
  let struct_env = Henv.create dummy_structure in
  let function_env = Henv.create dummy_function in
  List.iter (phase1 struct_env) dl;
  List.iter (phase2 struct_env function_env) dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused ();
  (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
