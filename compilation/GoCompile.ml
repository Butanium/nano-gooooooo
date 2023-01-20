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

module Env = Map.Make (String)

type env = {
  exit_label : string;
  (* local_vars : int Env.t; *)
  nb_vars : int; (* nombre de variables locales dans la fonction *)
  mutable current_var_ofs : int; (* offset of the last variable *)
  mutable current_stack_ofs : int;
      (* offset of the stack (par exemple si on calcule un tuple il y aura un offset car on empile les valeurs) *)
}

let pushq op env =
  env.current_stack_ofs <- env.current_stack_ofs + 8;
  pushq op

let popq op env =
  env.current_stack_ofs <- env.current_stack_ofs - 8;
  (* raise (Anomaly "popq"); *)
  popq op

let empty_env =
  { exit_label = ""; nb_vars = 0; current_var_ofs = 0; current_stack_ofs = 0 }

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
  | _ -> failwith "comp_to_jump: not a comparison operator"

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
  | _ -> failwith "not a int_binop"

let var_stack = rbp
let stack = rsp
(* let add_ofs env = env.current_stack_ofs <- env.current_stack_ofs + 8 *)

let add_var env =
  let res = env.current_var_ofs + env.current_stack_ofs in
  Printf.eprintf "add_var: %d + %d = %d\n%!" env.current_var_ofs
    env.current_stack_ofs res;
  env.current_var_ofs <- env.current_var_ofs - 8;
  res

let ( & ) var env = var.v_addr + env.current_var_ofs

let print_of_type e =
  match e.expr_type with
  | Tint -> "print_int"
  | Tbool -> "print_bool"
  | Tstring -> "print_string"
  | Tptr _ -> "print_ptr"
  | Tstruct _ -> failwith "TODO print struct"
  | Twild -> raise (Anomaly "wild type in print")
  | Tmany l -> failwith "TODO print many"

let rec expr (env : env) e =
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
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      e1
      ++ cmpq cfalse !%rax (* check if rax is 0 or not *)
      ++ jne skip (* if rax is true then don't evaluate the snd argument *)
      ++ e2
      ++ label skip
  | TEbinop (((Blt | Ble | Bgt | Bge) as op), e1, e2) ->
      (* Comparaison ints *)
      let e1 = expr env e1 in
      let p = pushq !%rax env in
      let e2 = expr env e2 in
      e1 ++ p ++ e2 ++ popq rbx env ++ cmpq !%rax !%rbx ++ flag_to_rax op
  | TEbinop (((Badd | Bsub | Bmul | Bdiv | Bmod) as op), e1, e2) ->
      (* arithmetique ints *)
      let e1 = expr env e1 in
      let push = pushq !%rax env in
      let e2 = expr env e2 in
      e1 ++ push ++ e2 ++ popq rbx env ++ int_binop op
  | TEbinop (((Beq | Bne) as op), e1, e2) ->
      (* egalite toute valeur *)
      let e1as = expr env e1 in
      e1as
      ++ (let in_reg =
            match e1.expr_type with
            | Tint | Tbool | Tstring | Tptr _ | Tstruct _ -> true
            | Twild -> raise (Anomaly "wild type in comparaison")
            | Tmany l -> false
          in
          let p =
            if in_reg then pushq !%rax env
            else empty_file (* save rax if needed *)
          in
          p
          ++ expr env e2
          ++ (if in_reg then popq rbx env
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
      expr env e1 ++ movq cfalse !%rbx ++ subq !%rax !%rbx ++ movq !%rbx !%rax
  | TEunop (Unot, e1) ->
      expr env e1
      ++ cmpq cfalse !%rax
      ++ movq cfalse !%rax
      ++ sete !%al (* set rax to non 0 if rax is 0 *)
  | TEunop (Uamp, e1) -> (
      match e1.expr_desc with
      | TEident x -> movq !%stack !%rax ++ addq (constint x.v_addr) !%rax
      | TEdot (e, f) -> expr env e ++ movq (ind rax ~ofs:f.f_ofs) !%rax
      | TEunop (Ustar, e) -> expr env e
      | _ -> failwith "Uamp lv error")
  | TEunop (Ustar, e1) -> expr env e1 ++ movq (ind rax) !%rax
  | TEprint [ e ] ->
      expr env e
      ++ movq !%rax !%rdi
      ++ call (print_of_type e)
      ++ call "print_newline"
  | TEprint el ->
      List.fold_left
        (fun acc e ->
          acc
          ++ call "print_space"
          ++ expr env e
          ++ movq !%rax !%rdi
          ++ call (print_of_type e))
        empty_file el
      ++ call "print_newline"
  | TEident x -> movq (ind ~ofs:x.v_addr stack) !%rax
  (* | TEassign ([ { expr_desc = TEident x; _ } ], [ e1 ]) ->
      expr env e1 ++ movq !%rax (ind ~ofs:x.v_addr stack) *)
  | TEassign ([ lv ], [ e1 ]) ->
      let e1 = expr env e1 in
      let push = pushq !%rax env in
      let lv =
        expr_lv env
          lv (* doit retourner l'adresse où est stocker la lv dans !%rax *)
      in
      e1 ++ push ++ lv ++ popq rbx env ++ movq !%rax (ind rbx)
  | TEassign (_, _) -> assert false
  | TEblock el -> List.fold_left (fun acc e -> acc ++ expr env e) empty_file el
  | TEif (e1, e2, e3) ->
      let l_false, l_skip = (new_label (), new_label ()) in
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      let e3 = expr env e3 in
      e1
      ++ cmpq cfalse !%rax
      ++ je l_false
      ++ e2
      ++ jmp l_skip
      ++ label l_false
      ++ e3
      ++ label l_skip
  | TEfor (e1, e2) ->
      let l_start = new_label () in
      let l_end = new_label () in
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      label l_start
      ++ e1
      ++ cmpq cfalse !%rax
      ++ je l_end
      ++ e2
      ++ jmp l_start
      ++ label l_end
  | TEnew ty ->
      let size = sizeof ty in
      malloc size
  | TEcall (f, el) -> (* TODO code pour appel fonction *) assert false
  | TEdot (e1, { f_ofs = ofs; _ }) -> expr env e1 ++ movq (ind rax ~ofs) !%rax
  | TEvars (vrs, [ { expr_type = Tmany (_ :: _ :: _); _ } ]) -> assert false
  | TEvars (vrs, es) ->
      (* on peut avoir des effets de bords mais bon... *)
      List.fold_left2
        (fun acc v e ->
          let ofs = add_var env in
          v.v_addr <- ofs;
          expr env e ++ movq !%rax (ind ~ofs stack) ++ acc)
        empty_file vrs es
      (* mettre les valeurs calculées dans les variables *)
  | TEreturn [] -> jmp env.exit_label
  | TEreturn [ e1 ] -> expr env e1 ++ jmp env.exit_label
  | TEreturn _ -> assert false
  | TEincdec (e1, op) ->
      expr_lv env e1
      ++ movq (ind rax) !%rbx
      ++ (if op = Inc then addq (imm 1) !%rbx else subq (imm 1) !%rbx)
      ++ movq !%rbx (ind rax)

and expr_lv env e =
  (* retourne le pointeur vers e un lv *)
  match e.expr_desc with
  | TEident x -> movq !%stack !%rax ++ addq (constint (x & env)) !%rax
  (* on met l'adresse dans la pile de la variable (x & env)*)
  | TEdot (e, f) -> expr env e ++ addq (constint f.f_ofs) !%rax
  | TEunop (Ustar, e) -> expr env e
  | _ -> failwith "expr_lv error: not a lv"

let rec nb_vars { expr_desc; _ } =
  match expr_desc with
  | TEvars (vrs, _) ->
      List.length
        (List.filter
           (fun x ->
             (* x.v_used <- false;
                (* pour dire qu'on a pas encore attribuer de place dans la pile *)*)
             x.v_name <> "_")
           vrs)
  | TEblock b -> List.fold_left (fun x y -> x + nb_vars y) 0 b
  | _ -> 0

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *)
  let s = f.fn_name in
  let nb_vars = nb_vars e in
  let exit_function = "E_" ^ s in
  let env =
    {
      exit_label = exit_function;
      (*local_vars = Env.empty*)
      nb_vars;
      current_stack_ofs = 0;
      current_var_ofs = 8 * nb_vars;
    }
  in
  label ("F_" ^ s)
  ++ pushq !%rbp empty_env
  ++ movq !%rsp !%rbp
  ++ subq (imm (8 * nb_vars)) !%rsp
  ++ movq !%rsp !%rbp
  ++ expr env e
  ++ label exit_function
  ++ movq !%rbp !%rsp
  ++ addq (imm (8 * nb_vars)) !%rsp
  ++ ret

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
      (* TODO appel malloc de stdlib *)
      ++ inline
           "\n\
            print_int_or_nil:\n\
           \      test    %rdi, %rdi\n\
           \      jz      print_nil\n\
           \      movq    (%rdi), %rdi\n\
            print_int:\n\
           \      movq    %rdi, %rsi\n\
           \      movq    $S_int, %rdi\n\
           \      xorq    %rax, %rax\n\
           \      call    printf\n\
           \      ret\n\
            print_ptr:\n\
           \      movq    %rdi, %rsi\n\
           \      movq    $S_ptr, %rdi\n\
           \      xorq    %rax, %rax\n\
           \      call    printf\n\
           \      ret\n\
            print_string:\n\
           \      test    %rdi, %rdi\n\
           \      jz      print_nil\n\
           \      mov     %rdi, %rsi\n\
           \      mov     $S_string, %rdi\n\
           \      xorq    %rax, %rax\n\
           \      call    printf\n\
           \      ret\n\
            print_nil:\n\
           \      mov     $S_nil, %rdi\n\
           \      xorq    %rax, %rax\n\
           \      call    printf\n\
           \      ret\n\
            print_space:\n\
           \      mov     $S_space, %rdi\n\
           \      xorq    %rax, %rax\n\
           \      call    printf\n\
           \      ret\n\n\
            print_newline:\n\
           \      mov     $S_newline, %rdi\n\
           \      xorq    %rax, %rax\n\
           \      call    printf\n\
           \      ret\n\
            print_bool:\n\
           \      xorq    %rax, %rax\n\
           \      test    %rdi, %rdi\n\
           \      jz      1f\n\
           \      mov     $S_true, %rdi\n\
           \      call    printf\n\
           \      ret\n\
            1:      mov     $S_false, %rdi\n\
           \      call    printf\n\
           \      ret\n\
            allocz:\n\
           \      movq    %rdi, %rbx\n\
           \      call    malloc\n\
           \      testq   %rbx, %rbx\n\
           \      jnz     1f\n\
           \      ret\n\
            1:    movb    $0, (%rax, %rbx)\n\
           \      decq    %rbx\n\
           \      jnz     1b\n\
           \      ret\n";
    data =
      label "S_int"
      ++ string "%ld"
      ++ label "S_string"
      ++ string "%s"
      ++ label "S_ptr"
      ++ string "<%ld>"
      ++ label "S_true"
      ++ string "true"
      ++ label "S_false"
      ++ string "false"
      ++ label "S_nil"
      ++ string "<nil>"
      ++ label "S_space"
      ++ string " "
      ++ label "S_newline"
      ++ string "\n"
      ++ Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings empty_file;
  }
