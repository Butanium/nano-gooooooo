(* Programme principal *)

open Format
open Lexing
open GoLexer
open GoParser
open Usage

let compile_and_run file =
  let file_name = Filename.chop_extension file in
  let exit_code =
    Printf.sprintf "gcc -no-pie %s.s -o %s.out" file_name file_name
    |> Sys.command
  in
  if exit_code <> 0 then failwith "compilation of the assembly file failed";
  print_endline "Result:";
  Printf.sprintf "./%s.out" file_name |> Sys.command |> ignore;
  Printf.printf "\nexiting...\n%!"

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = GoParser.file GoLexer.next_token lb in
    close_in c;
    Printexc.record_backtrace (debug || stack_trace);

    if debug then (
      printf "debugging....\n%!";
      let ast_dot_file =
        open_out (Filename.chop_suffix file ".go" ^ "_ast.dot")
      in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_ast f (not !no_pretty));
      close_out ast_dot_file);

    if !parse_only then exit 0;

    let f = Typing.file ~debug f in

    if debug then (
      let ast_dot_file =
        open_out (Filename.chop_suffix file ".go" ^ "_tast.dot")
      in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_tast f (not !no_pretty));
      close_out ast_dot_file);

    if !type_only then exit 0;

    let code = GoCompile.file ~debug f in

    let c = open_out (Filename.chop_suffix file ".go" ^ ".s") in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt code;
    close_out c;
    if !run then compile_and_run file
  with
  | GoLexer.Lexing_error s when not stack_trace ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
  | GoParser.Error when not stack_trace ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error\n@.";
      exit 1
  | Typing.Error (l, msg) when not stack_trace ->
      report_loc l;
      eprintf "error: %s\n@." msg;
      exit 1
  | Typing.Anomaly msg when not stack_trace ->
      eprintf "Typing Anomaly: %s\n@." msg;
      exit 2
  | GoCompile.Anomaly msg when not stack_trace ->
      eprintf "GoCompile Anomaly: %s\n@." msg;
      exit 2
  | e when not stack_trace ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
