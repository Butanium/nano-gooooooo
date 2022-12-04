open Format
open Lib
open Ast
open Tast

(* type boolean = bool *)

let debug = ref false
let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

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
    (sprintf "expected type %s, got %s instead" (string_of_type expected)
       (string_of_type typ))

let throw_undeclared loc k = error loc (sprintf "undeclared name: %s" k)

let check_type loc ~expected typ =
  if expected <> typ then throw_expected_type loc ~expected typ

let throw_if_not loc ~expected typ =
  match (expected, typ) with
  | `Struct, Tstruct _ | `Ptr, Tptr _ -> ()
  | `Struct, err ->
      error loc
        (sprintf "expected a struct, got %s instead" (string_of_type err))
  | `Ptr, err ->
      error loc
        (sprintf "expected a pointer, got %s instead" (string_of_type err))

module Henv = struct
  open Hashtbl

  type ('a, 'b) hashtable = ('a, 'b) t
  type 'b t = { dummy_init : string -> 'b; henv : (string, 'b) hashtable }

  let mem env = mem env.henv

  let add loc env name value =
    if mem env name then error loc (sprintf "%s has been declared twice " name);
    add env.henv name value

  let add_name loc env name = add loc env name (env.dummy_init name)
  let h_create = create
  let create dummy_init = { dummy_init; henv = h_create 10 }
  let find env = find env.henv
  let find_opt env = find_opt env.henv

  let find_exn ~loc env k =
    match find_opt env k with None -> throw_undeclared loc k | Some v -> v
end

let rec type_type env_struct = function
  | PTident { id; _ } when Henv.mem env_struct id ->
      Tstruct (Henv.find env_struct id)
  | PTident { id = "int"; _ } -> Tint
  | PTident { id = "bool"; _ } -> Tbool
  | PTident { id = "string"; _ } -> Tstring
  | PTptr ty -> Tptr (type_type env_struct ty)
  | PTident { id; loc } -> throw_undeclared loc id

let rec eq_type ty1 ty2 =
  match (ty1, ty2) with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany t1, Tmany t2 ->
      List.length t1 = List.length t2 && List.for_all2 eq_type t1 t2
  | _ -> false
(* myTODO check que c'est bien utilser *)

let ( === ) = eq_type
let ( ==! ) a b = a === b |> not
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

module Env : sig
  type t = var M.t

  val empty : 'a M.t
  val find : M.key -> 'a M.t -> 'a
  val find_exn : location -> 'a M.t -> M.key -> 'a
  val all_vars : var list ref
  val check_unused : unit -> unit
  val add_var : var M.t -> var -> var M.t

  val add_new_var :
    tag -> location -> ?used:bool -> typ -> var M.t -> var M.t * var
end = struct
  module M = Map.Make (String)

  type t = var M.t

  let empty = M.empty
  let find = M.find

  let find_exn loc env k =
    match M.find_opt k env with Some r -> r | None -> throw_undeclared loc k

  let add_var env v = M.add v.v_name v env
  let all_vars = ref []

  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO not used *) false then
        error v.v_loc "unused variable"
    in
    List.iter check !all_vars

  let add_new_var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    (add_var env v, v)

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

let unfold_many =
  let rec aux = function
    | Tmany [ t ] :: ts | t :: ts -> t :: aux ts
    | [] -> []
  in
  function [ Tmany l ] | l -> aux l

let type_function_body (structs : structure Henv.t) funs fun_ =
  let rec type_expr env e : expr * bool =
    let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
    ({ expr_desc = e; expr_type = ty }, rt)
  and just_expr env e = type_expr env e |> fst
  and type_in_block env (e : pexpr) =
    match e.pexpr_desc with
    | PEvars (ids, opt_t, exprs) ->
        if opt_t = None && exprs = [] then
          error e.pexpr_loc "missing variable type initialization";
        let t_exprs = List.map (fun e -> type_expr env e |> fst) exprs in
        (match opt_t with
        | None -> ()
        | Some t -> (
            let t = type_type structs t in
            match List.find_opt (fun e -> e.expr_type <> t) t_exprs with
            | None -> ()
            | Some err ->
                error e.pexpr_loc
                  (sprintf
                     "expected all values to be of type %s, got %s instead"
                     (string_of_type t)
                     (string_of_type err.expr_type))));
        let types = List.map (fun e -> e.expr_type) t_exprs |> unfold_many in
        let env, vars =
          List.fold_left2
            (fun (env, vars) name typ ->
              if name.id <> "_" then
                let env, var = Env.add_new_var name.id name.loc typ env in
                (env, var :: vars)
              else (env, vars))
            (env, []) ids types
        in
        ( env,
          { expr_desc = TEvars (List.rev vars, t_exprs); expr_type = tvoid },
          false )
    | _ ->
        let e, rt = type_expr env e in
        (env, e, rt)
  (*  returns the expr, the type of the expr and if there will be a return statement in any case *)
  and expr_desc env loc expr_p =
    match expr_p with
    | PEskip -> (TEskip, tvoid, false)
    | PEconstant c -> (TEconstant c, type_of_const c, false)
    | PEbinop (op, e1, e2) ->
        let ({ expr_type = typ1; _ } as e1), _ = type_expr env e1 in
        let ({ expr_type = typ2; _ } as e2), _ = type_expr env e2 in
        if typ1 = typ2 && typ1 = Tint then (TEbinop (op, e1, e2), Tint, false)
        else
          error loc
            (sprintf
               "type error in binary operation, expected type Int, got %s and \
                %s"
               (string_of_type typ1) (string_of_type typ2))
    | PEunop (op, e1) ->
        let expr, rt = type_expr env e1 in
        (TEunop (op, expr), get_unop_type loc op expr, false)
    | PEcall ({ id = "fmt.Print"; _ }, el) ->
        let el = List.map (just_expr env) el in
        fmt_used := true;
        if not !fmt_imported then
          error loc "package fmt is used but not imported";
        (TEprint el, tvoid, false)
    | PEcall ({ id = "new"; _ }, [ { pexpr_desc = PEident { id; _ }; _ } ]) ->
        let ty =
          match id with
          | id when Henv.mem structs id -> Tstruct (Henv.find structs id)
          | "int" -> Tint
          | "bool" -> Tbool
          | "string" -> Tstring
          | id -> throw_undeclared loc id
        in
        (TEnew ty, Tptr ty, false)
        (* declaration d'un pointeur ty *)
    | PEcall ({ id = "new"; loc }, _) -> error loc "new expects a type"
    | PEcall (id, el) ->
        let t_els = List.map (just_expr env) el in
        let types = List.map (fun e -> e.expr_type) t_els |> unfold_many in
        let func = Henv.find_exn ~loc funs id.id in
        let n_arg = List.length types in
        let exp_n = List.length func.fn_params in
        if n_arg <> exp_n then
          error loc
            (Printf.sprintf
               "Expected %d arguement for function %s, got %d instead" exp_n
               func.fn_name n_arg);
        List.iter2
          (fun typ param ->
            if typ ==! param.v_typ then
              error loc
                (Printf.sprintf
                   "Parameter %s is expected to have type %s, not %s"
                   param.v_name
                   (string_of_type param.v_typ)
                   (string_of_type typ)))
          types func.fn_params;
        (TEcall (func, t_els), Tmany func.return_types, false)
    | PEfor (e, b) ->
        let b, _ = type_expr env b in
        check_type loc ~expected:Tbool b.expr_type;
        let e, _ = type_expr env e in
        (TEfor (e, b), tvoid, false)
    | PEif (e1, e2, e3) ->
        let f = type_expr env in
        let (e1, _), (e2, rtThen), (e3, rtElse) = (f e1, f e2, f e3) in
        check_type loc ~expected:Tbool e1.expr_type;
        (TEif (e1, e2, e3), tvoid, rtThen && rtElse)
    | PEnil -> (TEnil, Tptr Twild, false)
    | PEident { id; loc } ->
        let v = Env.find_exn loc env id in
        (TEident v, v.v_typ, false)
    | PEdot (e, id) ->
        let e, _ = type_expr env e in
        let field =
          match e.expr_type with
          | Tstruct structure | Tptr (Tstruct structure) -> (
              match Hashtbl.find_opt structure.s_fields id.id with
              | None ->
                  error loc
                    (sprintf "type %s has no field %s"
                       (string_of_type e.expr_type)
                       id.id)
              | Some field -> field)
          | err ->
              error loc
              @@ sprintf "expected a struct, got %s instead"
                   (string_of_type err)
        in
        (TEdot (e, field), field.f_typ, false)
    | PEassign (lvl, el) ->
        let tlvls = List.map (just_expr env) lvl in
        let tels = List.map (just_expr env) el in
        (* TODO add a check for left values*)
        (TEassign (tlvls, tels), tvoid, false)
    | PEreturn el ->
        let typed_els = List.map (fun e -> type_expr env e |> fst) el in
        if
          List.map (fun e -> e.expr_type) typed_els
          |> unfold_many = fun_.return_types
        then (TEreturn typed_els, tvoid, true)
        else error loc "Return values don't match function return type"
    | PEblock el ->
        let (_, rt), t_els =
          List.fold_left_map
            (fun (env, acc_rt) expr ->
              let env, e, rt = type_in_block env expr in
              ((env, acc_rt || rt), e))
            (env, false) el
        in
        (TEblock t_els, tvoid, rt)
    | PEincdec (e, op) ->
        let e, fmt = type_expr env e in
        check_type loc ~expected:Tint e.expr_type;
        (* todo : add check for left value *)
        (TEincdec (e, op), tvoid, fmt)
    | PEvars _ -> error loc "Unexpected variable declaration"
  in

  List.fold_left
    (fun env param -> Env.add_var env param)
    Env.empty fun_.fn_params
  |> type_expr

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
          (sprintf "Error : cyclic dependancy of types in type %s" s_name)
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
  | Tmany list ->
      List.fold_left (fun acc x -> acc + sizeof x) 0 list
      (* TODO : ensure offset has no influence here
         (we saw that in C you have offset when a field size is not a 8 mutiple )*)
  | Twild -> error loc "size of Twild is undefined"

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
let decl (structs : structure Henv.t) (functions : function_ Henv.t) = function
  | PDfunction { pf_name = { id; loc }; pf_body; pf_return_types; _ } ->
      let func = Henv.find functions id in
      let e, rt = type_function_body structs functions func pf_body in
      if (not rt) && func.return_types <> [] then
        error loc (sprintf "No return statement in function %s" id);
      List.fold_left
        (fun address var ->
          var.v_addr <- address;
          address + sizeof var.v_typ)
        0 func.fn_params
      |> ignore;
      TDfunction (func, e)
  | PDstruct { ps_name = { id; _ }; _ } ->
      let structure = Henv.find structs id in
      let size =
        Hashtbl.fold
          (fun _ field offset ->
            field.f_ofs <- offset;
            offset + sizeof field.f_typ)
          structure.s_fields 0
      in
      structure.s_size <- size;
      TDstruct structure

let file ~debug:b (imp, dl) =
  debug := b;
  fmt_imported := imp;
  fmt_used := false;
  let struct_env = Henv.create dummy_structure in
  let function_env = Henv.create dummy_function in
  List.iter (phase1 struct_env) dl;
  List.iter (phase2 struct_env function_env) dl;
  Henv.find_exn ~loc:dummy_loc function_env "main" |> ignore;
  let dl = List.map (decl struct_env function_env) dl in
  Env.check_unused ();
  (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
