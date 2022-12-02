(* Programme principal *)

open Format
open Lexing
open GoLexer
open GoParser
open Usage

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = GoParser.file GoLexer.next_token lb in
    close_in c;

    if debug then (
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
    close_out c
  with
  | GoLexer.Lexing_error s ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
  | GoParser.Error ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error\n@.";
      exit 1
  | Typing.Error (l, msg) ->
      report_loc l;
      eprintf "error: %s\n@." msg;
      exit 1
  | Typing.Anomaly msg ->
      eprintf "Typing Anomaly: %s\n@." msg;
      exit 2
  | GoCompile.Anomaly msg ->
      eprintf "GoCompile Anomaly: %s\n@." msg;
      exit 2
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
