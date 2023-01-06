(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...
*)
[@@@ocamlformat "break-infix = fit-or-vertical"]

open Format
open Ast
open Tast
open X86_64

exception Anomaly of string

let debug = ref false
let strings = Hashtbl.create 32

let new_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let allocz n = movq (constint n) !%rdi ++ call "allocz"
let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in
  fun () ->
    incr r;
    "L_" ^ string_of_int !r

type env = {
  exit_label : string;
  ofs_this : int;
  nb_locals : int ref; (* maximum *)
  next_local : int; (* 0, 1, ... *)
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_type = Tbool }
(* f reçoit le label correspondant à ``renvoyer vrai''
   let compile_bool f =
     let l_true = new_label () and l_end = new_label () in
     f l_true
     ++ movq (imm 0) (reg rdi)
     ++ jmp l_end ++ label l_true
     ++ movq (imm 1) (reg rdi)
     ++ label l_end *)

let comp_to_jump label_true = function
  | Blt -> jl label_true
  | Ble -> jle label_true
  | Bgt -> jg label_true
  | Bge -> jge label_true
  | Beq -> je label_true
  | Bne -> jne label_true
  | _ -> assert false

let flag_to_rax op =
  let l_true = new_label () in
  let l_end = new_label () in
  comp_to_jump l_true op (* jump to l_true if the condition is true *)
  ++ movq cfalse !%rax
     (* if the condition is false then set rax to false and skip the l_true label *)
  ++ jmp l_end
  ++ label l_true
  ++ movq ctrue !%rax
  ++ label l_end

let int_binop = function
  | Badd -> addq !%rbx !%rax
  | Bsub -> subq !%rbx !%rax
  | Bmul -> imulq !%rbx !%rax
  | Bdiv -> cqto ++ idivq !%rbx
  | Bmod -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
  | _ -> assert false

let rec expr env e =
  match e.expr_desc with
  | TEskip -> empty_file
  | TEconstant (Cbool true) -> movq ctrue !%rax
  | TEconstant (Cbool false) -> movq cfalse !%rax
  | TEconstant (Cint x) -> movq (constint64 x) !%rax
  | TEnil -> movq cfalse !%rax
  | TEconstant (Cstring s) ->
      let l = new_string s in
      movq !$l !%rax
      (* todo check if I need (%rip) *)
  | TEbinop (Band, e1, e2) ->
      let skip = new_label () in
      expr env e1
      ++ cmpq cfalse !%rax (* check if rax is 0 or not *)
      ++ je skip (* if rax is false then don't evaluate the snd argument *)
      ++ expr env e2
      ++ label skip
  | TEbinop (Bor, e1, e2) ->
      let skip = new_label () in
      expr env e1
      ++ cmpq cfalse !%rax (* check if rax is 0 or not *)
      ++ jne skip (* if rax is true then don't evaluate the snd argument *)
      ++ expr env e2
      ++ label skip
  | TEbinop (((Blt | Ble | Bgt | Bge) as op), e1, e2) ->
      (* Comparaison ints *)
      expr env e1
      ++ pushq !%rax
      ++ expr env e2
      ++ popq rbx
      ++ cmpq !%rax !%rbx
      ++ flag_to_rax op
  | TEbinop (((Badd | Bsub | Bmul | Bdiv | Bmod) as op), e1, e2) ->
      (* arithmetique ints *)
      expr env e1 ++ pushq !%rax ++ expr env e2 ++ popq rbx ++ int_binop op
  | TEbinop (((Beq | Bne) as op), e1, e2) ->
      (* egalite toute valeur *)
      expr env e1
      ++ (let in_reg =
            match e1.expr_type with
            | Tint | Tbool | Tstring | Tptr _ | Tstruct _ -> true
            | Twild -> raise (Anomaly "wild type in comparaison")
            | Tmany l -> false
          in
          (if in_reg then pushq !%rax else empty_file (* save rax if needed *))
          ++ expr env e2
          ++ (if in_reg then popq rbx
             else empty_file (* pop result if needed *))
          ++
          match e1.expr_type with
          | Tint | Tbool | Tptr _ -> cmpq !%rax !%rbx
          | Tstring ->
              movq !%rax !%rdi
              ++ movq !%rbx !%rsi
              ++ xorq !%rax !%rax (* set upper 32 bits of rax to 0 *)
              ++ call "strcmp"
              ++ cmpq (imm 0) !%rax
          | Twild -> raise (Anomaly "wild type in comparaison")
          | Tstruct _ | Tmany _ -> failwith "TODO comp")
      ++ flag_to_rax op
      (* result of the comparaison should be in flag, we put it in rax *)
  | TEunop (Uneg, e1) ->
      expr env e1 ++ movq (imm 0) !%rbx ++ subq !%rax !%rbx ++ movq !%rbx !%rax
  | TEunop (Unot, e1) ->
      expr env e1
      ++ cmpq cfalse !%rax
      ++ movq cfalse !%rax
      ++ sete !%al (* set rax to non 0 if rax is 0 *)
  | TEunop (Uamp, e1) -> (* TODO code pour & *) assert false
  | TEunop (Ustar, e1) -> (* TODO code pour * *) assert false
  | TEprint el -> (* TODO code pour Print *) assert false
  | TEident x -> (* TODO code pour x *) assert false
  | TEassign ([ { expr_desc = TEident x } ], [ e1 ]) ->
      (* TODO code pour x := e *) assert false
  | TEassign ([ lv ], [ e1 ]) ->
      (* TODO code pour x1,... := e1,... *) assert false
  | TEassign (_, _) -> assert false
  | TEblock el -> (* TODO code pour block *) assert false
  | TEif (e1, e2, e3) ->
      expr e1 env
      ++ cmpq cfalse !%rax
      ++
      let l_false, l_skip = (new_label (), new_label ()) in
      je l_false
      ++ expr e2 env
      ++ jmp l_skip
      ++ label l_false
      ++ expr e3 env
      ++ label l_skip
  | TEfor (e1, e2) ->
      let l_start = new_label () in
      let l_end = new_label () in
      label l_start
      ++ expr e1 env
      ++ cmpq cfalse !%rax
      ++ je l_end
      ++ expr e2 env
      ++ jmp l_start
      ++ label l_end
  | TEnew ty ->
      let size = sizeof ty in
      malloc (size + 1)
      (* +1 to allow empty struct *)
  | TEcall (f, el) -> (* TODO code pour appel fonction *) assert false
  | TEdot (e1, { f_ofs = ofs }) -> (* TODO code pour e.f *) assert false
  | TEvars _ -> assert false (* fait dans block *)
  | TEreturn [] -> (* TODO code pour return e *) assert false
  | TEreturn [ e1 ] -> (* TODO code pour return e1,... *) assert false
  | TEreturn _ -> assert false
  | TEincdec (e1, op) -> (* TODO code pour return e++, e-- *) assert false

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *)
  let s = f.fn_name in
  label ("F_" ^ s)

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b = false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *)
  let funs = List.fold_left decl empty_file dl in
  {
    text =
      globl "main"
      ++ label "main"
      ++ call "F_main"
      ++ xorq !%rax !%rax
      ++ ret
      ++ funs
      ++ inline
           "\n\
            print_int:\n\
           \        movq    %rdi, %rsi\n\
           \        movq    $S_int, %rdi\n\
           \        xorq    %rax, %rax\n\
           \        call    printf\n\
           \        ret\n";
    (* TODO print pour d'autres valeurs *)
    (* TODO appel malloc de stdlib *)
    data =
      label "S_int"
      ++ string "%ld"
      ++ Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings empty_file;
  }
