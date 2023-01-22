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
let unfold_1 = function Tmany [ x ] | x -> x

let rec eq_type ty1 ty2 =
  match (ty1, ty2) with
  | Tptr Twild, Tptr _ | Tptr _, Tptr Twild -> true
  | Twild, _ | _, Twild -> true
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany t1, Tmany t2 ->
      List.length t1 = List.length t2 && List.for_all2 eq_type t1 t2
  | _ -> false

let ( === ) = eq_type
let ( ==! ) a b = a === b |> not

let check_type loc ~expected typ =
  if expected ==! unfold_1 typ then throw_expected_type loc ~expected typ

(* unused
    let throw_if_not loc ~expected typ =
   match (expected, typ) with
   | `Struct, Tstruct _ | `Ptr, Tptr _ -> ()
   | `Struct, err ->
       error loc
         (sprintf "expected a struct, got %s instead" (string_of_type err))
   | `Ptr, err ->
       error loc
         (sprintf "expected a pointer, got %s instead" (string_of_type err))*)

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

let fmt_used = ref false
let fmt_imported = ref false
let evar v = { expr_desc = TEident v; expr_type = v.v_typ }

let new_var ?(v_depth = 0) =
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
      v_depth;
    }

module Env : sig
  val empty : 'a M.t
  val find_exn : location -> 'a M.t -> M.key -> 'a
  val all_vars : var list ref
  val check_unused : unit -> unit
  val add_var : loc:location -> var M.t -> var -> var M.t

  val add_new_var :
    tag ->
    location ->
    v_depth:int ->
    ?used:bool ->
    typ ->
    var M.t ->
    var M.t * var

  exception WildInTheWild of location
end = struct
  module M = Map.Make (String)

  type t = var M.t

  let empty = M.empty
  let find = M.find

  exception WildInTheWild of location

  let find_exn loc env k =
    match M.find_opt k env with
    | Some r -> r
    | None ->
        if k <> "_" then throw_undeclared loc k else raise @@ WildInTheWild loc

  let add_var ~loc env v =
    (match M.find_opt v.v_name env with
    | None -> ()
    | Some other_v ->
        if v.v_depth = other_v.v_depth then
          error loc (sprintf "Var %s is redeclared in this block" v.v_name));
    M.add v.v_name v env

  let all_vars = ref []

  let check_unused () =
    let check v =
      if v.v_name <> "_" && not v.v_used then
        error v.v_loc (sprintf "unused variable %s" v.v_name)
    in
    if not !debug then List.iter check !all_vars

  let add_new_var x loc ~v_depth ?used ty env =
    let v = new_var x loc ~v_depth ?used ty in
    all_vars := v :: !all_vars;
    (add_var ~loc env v, v)

  (* TOD?O type () et vecteur de types *)
end

let rec left_value expr =
  match expr.expr_desc with
  | TEident _ -> (*v.v_name <> "_"*) true
  | TEdot (el, _) -> left_value el
  | TEunop (Ustar, el) -> el.expr_desc <> TEnil
  | _ -> false

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

let unfold_many loc =
  let rec aux = function
    | Tmany [] :: _ | Tmany (_ :: _ :: _) :: _ ->
        error loc "Can't unpack values"
    | Tmany [ t ] :: ts | t :: ts -> t :: aux ts
    | [] -> []
  in
  function [ Tmany (_ :: _ as l) ] | l -> aux l

let ret_binop = function
  | Badd | Bsub | Bmul | Bdiv | Bmod -> Tint
  | Beq | Bne | Blt | Ble | Bgt | Bge | Band | Bor -> Tbool

let check_binop loc e1 e2 = function
  | Badd | Bsub | Bmul | Bdiv | Bmod | Bge | Bgt | Ble | Blt ->
      check_type loc ~expected:Tint e1.expr_type;
      check_type loc ~expected:Tint e2.expr_type
  | Band | Bor ->
      check_type loc ~expected:Tbool e1.expr_type;
      check_type loc ~expected:Tbool e2.expr_type
  | Beq | Bne ->
      if unfold_1 e1.expr_type ==! unfold_1 e2.expr_type then
        error loc "Can't compare values of different types";
      if e1.expr_desc = TEnil && e2.expr_desc = TEnil then
        error loc "Can't compare 2 nil values"

let type_function_body (structs : structure Henv.t) funs fun_ expr =
  let rec type_expr env depth e : expr * bool =
    let e, ty, rt = expr_desc env depth e.pexpr_loc e.pexpr_desc in
    ({ expr_desc = e; expr_type = ty }, rt)
  and just_expr env depth e = type_expr env depth e |> fst
  and type_in_block env depth (e : pexpr) =
    match e.pexpr_desc with
    | PEvars (ids, opt_t, exprs) ->
        if opt_t = None && exprs = [] then
          error e.pexpr_loc "missing variable type initialization";
        let t_exprs = List.map (fun e -> type_expr env depth e |> fst) exprs in
        let types =
          if t_exprs <> [] then
            List.map (fun e -> e.expr_type) t_exprs |> unfold_many e.pexpr_loc
          else
            List.init (List.length ids)
              (Fun.const @@ type_type structs @@ Option.get opt_t)
        in
        (match opt_t with
        | None ->
            if exprs = [] then
              error e.pexpr_loc "Missing variable type or initialization"
        | Some t -> (
            let t = type_type structs t in
            match List.find_opt (fun typ -> typ ==! t) types with
            | None -> ()
            | Some err ->
                error e.pexpr_loc
                  (sprintf
                     "expected all values to be of type %s, got %s instead"
                     (string_of_type t) (string_of_type err))));

        let lt = List.length types in
        let l_expected = List.length ids in
        if lt <> l_expected then
          error e.pexpr_loc
            (sprintf "%s values to unpack : expected %d, got %d"
               (if lt > l_expected then "To many" else "Not enough")
               l_expected lt);
        let env, vars =
          List.fold_left2
            (fun (env, vars) name typ ->
              if name.id <> "_" then
                let env, var =
                  Env.add_new_var ~v_depth:depth name.id name.loc typ env
                in
                (env, var :: vars)
              else
                ( env,
                  {
                    v_name = "_";
                    v_typ = Twild;
                    v_used = true;
                    v_depth = depth;
                    v_addr = -1;
                    v_id = -1;
                    v_loc = name.loc;
                  }
                  :: vars ))
            (env, []) ids types
        in
        ( env,
          { expr_desc = TEvars (List.rev vars, t_exprs); expr_type = tvoid },
          false )
    | _ ->
        let e, rt = type_expr env depth e in
        (env, e, rt)
  (*  returns the expr, the type of the expr and if there will be a return statement in any case *)
  and expr_desc env depth loc expr_p =
    match expr_p with
    | PEskip -> (TEskip, tvoid, false)
    | PEconstant c -> (TEconstant c, type_of_const c, false)
    | PEbinop (op, e1, e2) ->
        let e1, _ = type_expr env depth e1 in
        let e2, _ = type_expr env depth e2 in
        check_binop loc e1 e2 op;
        (TEbinop (op, e1, e2), ret_binop op, false)
    | PEunop (op, e1) ->
        let expr, _ = type_expr env depth e1 in
        if op = Uamp then
          if not (left_value expr) then
            error loc "Can't take the address of a non left value";
        (TEunop (op, expr), get_unop_type loc op expr, false)
    | PEcall ({ id = "fmt.Print"; _ }, el) ->
        let el = List.map (just_expr env depth) el in
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
        let t_els = List.map (just_expr env depth) el in
        let types = List.map (fun e -> e.expr_type) t_els |> unfold_many loc in
        let func = Henv.find_exn ~loc funs id.id in
        let n_arg = List.length types in
        let exp_n = List.length func.fn_params in
        if n_arg <> exp_n then
          error loc
            (sprintf "Expected %d arguement for function %s, got %d instead"
               exp_n func.fn_name n_arg);
        List.iter2
          (fun typ param ->
            if typ ==! param.v_typ then
              error loc
                (sprintf "Parameter %s is expected to have type %s, not %s"
                   param.v_name
                   (string_of_type param.v_typ)
                   (string_of_type typ)))
          types func.fn_params;
        (TEcall (func, t_els), Tmany func.return_types, false)
    | PEfor (b, e) ->
        let b, _ = type_expr env depth b in
        check_type loc ~expected:Tbool b.expr_type;
        let e, _ = type_expr env (depth + 1) e in
        (TEfor (b, e), tvoid, false)
    | PEif (e1, e2, e3) ->
        let f = type_expr env in
        let (e1, _), (e2, rtThen), (e3, rtElse) =
          (f depth e1, f (depth + 1) e2, f (depth + 1) e3)
        in
        check_type loc ~expected:Tbool e1.expr_type;
        (TEif (e1, e2, e3), tvoid, rtThen && rtElse)
    | PEnil -> (TEnil, Tptr Twild, false)
    | PEident { id; loc } ->
        let v = Env.find_exn loc env id in
        v.v_used <- true;
        (TEident v, v.v_typ, false)
    | PEdot (e, id) ->
        let e, _ = type_expr env depth e in
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
        let tlvls =
          List.map
            (fun e ->
              try just_expr env depth e
              with Env.WildInTheWild loc ->
                {
                  expr_desc = TEident (new_var "_" loc ~used:true Twild);
                  expr_type = Twild;
                })
            lvl
        in
        (match List.find_opt (fun x -> not @@ left_value x) tlvls with
        | Some _ -> error loc "expected left value"
        | _ -> ());
        let tels = List.map (just_expr env depth) el in
        let r_types = List.map (fun e -> e.expr_type) tels |> unfold_many loc in
        let l_types = List.map (fun e -> e.expr_type) tlvls in
        if List.length l_types <> List.length r_types then
          error loc
            (Printf.sprintf "Expected %d values, got %d instead"
               (List.length l_types) (List.length r_types));
        List.iter2
          (fun expected r -> check_type loc ~expected r)
          l_types r_types;
        (TEassign (tlvls, tels), tvoid, false)
    | PEreturn el ->
        let typed_els = List.map (fun e -> type_expr env depth e |> fst) el in
        if
          List.map (fun e -> e.expr_type) typed_els
          |> unfold_many loc = fun_.return_types
        then (TEreturn typed_els, tvoid, true)
        else error loc "Return values don't match function return type"
    | PEblock el ->
        let (_, rt), t_els =
          List.fold_left_map
            (fun (env, acc_rt) expr ->
              let env, e, rt = type_in_block env (depth + 1) expr in
              ((env, acc_rt || rt), e))
            (env, false) el
        in
        (TEblock t_els, tvoid, rt)
    | PEincdec (e, op) ->
        let e, fmt = type_expr env depth e in
        check_type loc ~expected:Tint e.expr_type;
        if not @@ left_value e then error loc "expected left value";
        (TEincdec (e, op), tvoid, fmt)
    | PEvars _ -> error loc "Unexpected variable declaration"
  in
  try
    (List.fold_left
       (fun env param ->
         if param.v_name <> "_" then Env.add_var ~loc:dummy_loc env param
         else env)
       Env.empty fun_.fn_params
    (* |> Env.add_new_var "_" dummy_loc ~used:true ~v_depth:0 Twild |> fst *)
    |> type_expr)
      0 expr
  with Env.WildInTheWild loc -> error loc "cannot use _ as a value or a type"

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
  | Twild -> error loc "size of Twild is undefined"

(* 2. declare functions and type fields *)
let phase2 structs funs = function
  | PDfunction { pf_name = { id; loc }; pf_params; pf_return_types; _ } ->
      let names = Hashtbl.create 10 in
      let fn_params =
        List.map
          (fun ({ id = v_name; loc = v_loc }, typ) ->
            if Hashtbl.mem names v_name then
              error v_loc (sprintf "duplicate parameter %s" v_name);
            Hashtbl.add names v_name true;
            new_var v_name v_loc (type_type structs typ))
          pf_params
      in
      let return_types = List.map (type_type structs) pf_return_types in
      Henv.add loc funs id { fn_name = id; fn_params; return_types }
  | PDstruct { ps_name = { id; _ }; ps_fields } ->
      let structure = Henv.find structs id in
      List.iter
        (fun (ident, ptyp) ->
          let f_typ = type_type structs ptyp in
          if Hashtbl.mem structure.s_fields ident.id then
            error ident.loc (sprintf "duplicate field %s" ident.id);
          Hashtbl.add structure.s_fields ident.id
            { f_name = ident.id; f_typ; f_ofs = 0 })
        ps_fields

(* 3. type check function bodies and compute size and offsets for sturct, also compute v_addr *)
let decl (structs : structure Henv.t) (functions : function_ Henv.t) = function
  | PDfunction { pf_name = { id; loc }; pf_body; _ } ->
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
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
