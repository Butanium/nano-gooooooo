(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res 1
            ...
            res k
            arg 1
            ...
            arg n
            adr. retour
            ancien rbp
   rbp ---> var locales
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
    match Hashtbl.find_opt strings s with
    | Some label -> label
    | _ ->
        incr r;
        let l = "S_" ^ string_of_int !r in
        Hashtbl.add strings s l;
        l

let allocz n = movq (constint n) !%rdi ++ call "allocz"
let sizeof = Typing.sizeof
let unfold_typ = Typing.unfold_1

let new_label =
  let r = ref 0 in
  fun ?(name = "") () ->
    incr r;
    "L_" ^ name ^ (if name = "" then "" else "_") ^ string_of_int !r

type env = {
  exit_label : string;
  nb_args : int; (* nombre de variables locales dans la fonction *)
  mutable current_var_ofs : int; (* offset of the last variable *)
}

let mk_expr d = { expr_desc = d; expr_type = Twild }

let comp_to_jump label_true = function
  | Blt -> jl label_true
  | Ble -> jle label_true
  | Bgt -> jg label_true
  | Bge -> jge label_true
  | Beq -> je label_true
  | Bne -> jne label_true
  | _ -> failwith "comp_to_jump: not a comparison operator"

let flag_to_rax op =
  let l_true = new_label ~name:"istrue" () in
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
  let res = env.current_var_ofs in
  env.current_var_ofs <- env.current_var_ofs - 8;
  res

let ( !& ) var = constint var.v_addr (* position relative à var_stack *)
let pr_string s = movq !$(new_string s) !%rdi ++ call "print_string"

let rec print_of_type t =
  match unfold_typ t with
  | Tint -> call "print_int"
  | Tbool -> call "print_bool"
  | Tstring -> call "print_string"
  | Tptr _ -> call "print_ptr"
  | Tstruct s ->
      movq !%rdi !%rbx
      ++ (* save the struct *)
      pr_string s.s_name
      ++ pr_string " {"
      ++ Hashtbl.fold
           (fun _ field acc ->
             acc
             ++ pr_string (sprintf "%s = " field.f_name)
             ++ pushq !%rbx (* save the struct *)
             ++ movq (ind rbx ~ofs:field.f_ofs) !%rdi
             ++ print_of_type field.f_typ
             ++ pr_string "; "
             ++ popq rbx (* restore the struct *))
           s.s_fields empty_file
      ++ pr_string "}"
  | Twild -> raise (Anomaly "wild type in print")
  | Tmany l -> raise (Anomaly "many type in print")

exception Do_Not_Assign


let rec expr (env : env) _e =
  match _e.expr_desc with
  | TEskip -> empty_file
  | TEconstant (Cbool true) -> movq ctrue !%rax
  | TEconstant (Cbool false) -> movq cfalse !%rax
  | TEconstant (Cint x) -> movq (constint64 x) !%rax
  | TEnil -> movq cfalse !%rax
  | TEconstant (Cstring s) ->
      let l = new_string s in
      movq !$l !%rax
  | TEbinop (Band, e1, e2) ->
      let skip = new_label ~name:"skipand" () in
      expr env e1
      ++ cmpq cfalse !%rax (* check if rax is 0 or not *)
      ++ je skip (* if rax is false then don't evaluate the snd argument *)
      ++ expr env e2
      ++ label skip
  | TEbinop (Bor, e1, e2) ->
      let skip = new_label ~name:"skipor" () in
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
      let p = pushq !%rax in
      let e2 = expr env e2 in
      e1 ++ p ++ e2 ++ popq rbx ++ cmpq !%rax !%rbx ++ flag_to_rax op
  | TEbinop (((Badd | Bsub | Bmul | Bdiv | Bmod) as op), e1, e2) ->
      (* arithmetique ints *)
      let e1 = expr env e1 in
      let push = pushq !%rax in
      let e2 = expr env e2 in
      e1 ++ push ++ e2 ++ movq !%rax !%rbx ++ popq rax ++ int_binop op
  | TEbinop (((Beq | Bne) as op), e1, e2) ->
      (* egalite toute valeur *)
      let e1as = expr env e1 in
      e1as
      ++ (pushq !%rax
         ++ expr env e2
         ++ popq rbx
         ++
         match e1.expr_type with
         | Tint | Tbool | Tptr _ | Tstruct _ -> cmpq !%rax !%rbx
         (* Comme la sémantique d'éxecution de l'égalité n'est pas évoquée dans le sujet,
            je me permet de faire une comparaison physique pour les structures *)
         | Tstring ->
             movq !%rax !%rdi
             ++ movq !%rbx !%rsi
             ++ xorq !%rax !%rax (* set upper 32 bits of rax to 0 *)
             ++ call "strcmp"
             ++ cmpq (imm 0) !%rax
         | Twild -> raise (Anomaly "wild type in comparaison")
         | Tmany _ -> raise (Anomaly "many type in comparaison"))
      ++ flag_to_rax op
      (* result of the comparaison should be in flag, we put it in rax *)
  | TEunop (Uneg, e1) ->
      expr env e1 ++ movq cfalse !%rbx ++ subq !%rax !%rbx ++ movq !%rbx !%rax
  | TEunop (Unot, e1) ->
      expr env e1
      ++ cmpq cfalse !%rax
      ++ movq cfalse !%rax
      ++ sete !%al (* set rax to non 0 if rax is 0 *)
  | TEunop (Uamp, ({ expr_type = Tstruct _; _ } as e)) -> expr env e
  | TEunop (Uamp, e1) -> (
      match e1.expr_desc with
      | TEident x -> movq !%var_stack !%rax ++ addq !&x !%rax
      | TEdot (e, f) -> expr env e ++ addq (constint f.f_ofs) !%rax
      | TEunop (Ustar, e) -> expr env e
      | _ -> failwith "Uamp lv error")
  | TEunop (Ustar, e1) -> expr env e1 ++ movq (ind rax) !%rax
  | TEprint [ ({ expr_type = Tmany (_ :: _ :: _ as types); _ } as e) ] ->
      expr env e
      ++ (List.fold_left
            (fun (code, ofs) t ->
              (code ++ movq (ind stack ~ofs) !%rdi ++ print_of_type t, ofs + 8))
            (empty_file, 0) types
         |> fst)
  | TEprint [ e ] -> expr env e ++ movq !%rax !%rdi ++ print_of_type e.expr_type
  | TEprint el ->
      List.fold_left
        (fun acc e ->
          acc ++ expr env e ++ movq !%rax !%rdi ++ print_of_type e.expr_type)
        empty_file el
  | TEident x -> movq (ind ~ofs:x.v_addr var_stack) !%rax
  | TEassign ([ lv ], [ e1 ]) -> (
      if !debug then eprintf "assign 1 lv 1 e\n%!";
      expr env e1
      ++
      try
        pushq !%rax
        ++ expr_lv env
             lv (* doit retourner l'adresse où est stocker la lv dans !%rax *)
        ++ popq rbx
        ++ movq !%rbx (ind rax)
      with Do_Not_Assign ->
        empty_file (* if the expr should be assigned to _, we just ignore it *))
  | TEassign (lv, [ ({ expr_type = Tmany (_ :: _ :: _); _ } as e) ]) ->
      expr env e
      ++ (List.fold_left
            (fun (code, r_ofs) lv ->
              ( (code
                ++
                try (* If Do_Not_Assign is raised, we just ignore the lv (it's _) *)
                  expr_lv env lv
                  ++ movq (ind stack ~ofs:r_ofs) !%rbx
                  ++ movq !%rbx (ind rax)
                with Do_Not_Assign -> empty_file),
                r_ofs - 8 ))
            (empty_file, 0) lv
         |> fst)
  | TEassign (lvs, es) ->
      if !debug then
        Printf.eprintf "assign %d lvs %d es\n%!" (List.length lvs)
          (List.length es);
      List.fold_left
        (fun acc le -> acc ++ expr env le ++ pushq !%rax)
        empty_file es
      ++ (List.rev lvs
         |> List.fold_left
              (fun acc lv ->
                acc ++ expr_lv env lv ++ popq rbx ++ movq !%rbx (ind rax))
              empty_file)
  | TEblock el ->
      let debug_line = ref 0 in 
      List.fold_left
        (fun acc e ->
          let debug_header =
            incr debug_line;
            inline (sprintf "# line %d\n" !debug_line)
          in
          if !debug then eprintf "# line %d\n%!" !debug_line;
          acc ++ debug_header ++ expr env e)
        empty_file el
  | TEif (e1, e2, e3) ->
      let l_false, l_skip =
        (new_label ~name:"if_false" (), new_label ~name:"end_if" ())
      in
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
      let l_start = new_label ~name:"for_start" () in
      let l_end = new_label ~name:"for_end" () in
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
      allocz size
  | TEcall (f, el) ->
      let nb_ret = List.length f.return_types in
      let ret_stack_size = if nb_ret > 1 then nb_ret * 8 else 0 in
      (if nb_ret > 1 then
       subq (constint ret_stack_size)
         !%rsp (* allocate space for the return values *)
      else empty_file)
      ++ List.fold_left
           (fun acc e -> acc ++ expr env e ++ pushq !%rax)
           empty_file el
      ++ call ("F_" ^ f.fn_name)
      ++ addq (constint ((8 * List.length el) + ret_stack_size)) !%rsp
  | TEdot (e1, { f_ofs = ofs; _ }) -> expr env e1 ++ movq (ind rax ~ofs) !%rax
  | TEvars (vrs, [ ({ expr_type = Tmany (_ :: _ :: _); _ } as e) ]) ->
      expr env e
      ++ (List.fold_left
            (fun (code, r_ofs) v ->
              ( (code
                ++
                let ofs = add_var env in
                v.v_addr <- ofs;
                movq (ind stack ~ofs:r_ofs) !%rax
                ++ movq !%rax (ind ~ofs var_stack)),
                r_ofs - 8 ))
            (empty_file, 0) vrs
         |> fst)
  | TEvars (vrs, []) ->
      List.fold_left
        (fun acc v ->
          let ofs = add_var env in
          v.v_addr <- ofs;
          acc
          ++
          match v.v_typ with
          | Tstruct _ ->
              allocz (sizeof v.v_typ) ++ movq !%rax (ind ~ofs var_stack)
          | _ -> movq (constint 0) (ind ~ofs var_stack))
        empty_file vrs
  | TEvars (vrs, es) ->
      if !debug then
        Printf.eprintf "TEvars: %d vars, %d expr\n%!" (List.length vrs)
          (List.length es);
      List.fold_left2
        (fun acc v e ->
          acc
          ++
          if v.v_name = "_" then expr env e
          else
            let ofs = add_var env in
            v.v_addr <- ofs;
            expr env e ++ movq !%rax (ind ~ofs var_stack))
        empty_file vrs es
      (* mettre les valeurs calculées dans les variables *)
  | TEreturn [] -> jmp env.exit_label
  | TEreturn [ e1 ] -> expr env e1 ++ jmp env.exit_label
  | TEreturn l ->
      let ofs = 8 + 16 + (8 * List.length l) + (8 * env.nb_args) in
      (* le rbp dans la pile + l'adresse de retour + 8 à cause du code de function_
         + 8*le nb de valeurs à retourner + 8* le nb d'args*)
      let return_stack i = ind rbp ~ofs:(ofs - (i * 8)) in
      List.fold_left
        (fun (acc, i) e ->
          (acc ++ expr env e ++ movq !%rax (return_stack i), i + 1))
        (empty_file, 0) l
      |> fst
  | TEincdec (e1, op) ->
      expr_lv env e1
      ++ movq (ind rax) !%rbx
      ++ (if op = Inc then addq (imm 1) !%rbx else subq (imm 1) !%rbx)
      ++ movq !%rbx (ind rax)

and expr_lv env e =
  (* retourne le pointeur vers e un lv *)
  match e.expr_desc with
  | TEident { v_name = "_"; _ } -> raise Do_Not_Assign
  | TEident x -> movq !%var_stack !%rax ++ addq !&x !%rax
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
  let s = f.fn_name in
  let nb_vars = nb_vars e in
  let exit_function = "E_" ^ s in
  let env =
    {
      exit_label = exit_function;
      nb_args = List.length f.fn_params;
      current_var_ofs = 0;
    }
  in
  let nb_params = List.length f.fn_params in
  List.fold_left
    (fun addr (arg : var) ->
      arg.v_addr <- addr;
      addr - 8)
    ((8 * nb_params) + 16)
    f.fn_params
  |> ignore;
  label ("F_" ^ s)
  ++ inline "# function prologue\n"
  ++ pushq !%var_stack (* old var_stack to restore it at the end *)
  ++ movq !%stack !%var_stack
  ++ subq (constint 8)
       !%var_stack (* var_stack now points to the adress of the first var *)
  ++ subq
       (constint (8 * nb_vars))
       !%stack (* allocate space for local variables *)
  ++ inline "# function expr\n"
  ++ expr env e
  ++ label exit_function
  ++ movq !%var_stack !%stack
  ++ addq (constint 8) !%stack (* restore stack *)
  ++ popq var_stack (* restore var_stack *)
  ++ ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b = false) dl =
  debug := b;
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
      ++ string "<%p>"
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
      ++ Hashtbl.fold (fun s l d -> label l ++ string s ++ d) strings empty_file;
  }
