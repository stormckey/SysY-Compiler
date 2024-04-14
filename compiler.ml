open Core

let make_parse_error_msg filebuf =
  let start_pos = Lexing.lexeme_start_p filebuf in
  let end_pos = Lexing.lexeme_end_p filebuf in
  let start_char = start_pos.pos_cnum - start_pos.pos_bol in
  let end_char = end_pos.pos_cnum - end_pos.pos_bol in
  Printf.sprintf "Parse error: line %d, characters %d-%d" start_pos.pos_lnum
    start_char end_char

let parse_to_ast_exn filebuf =
  try
    let ast = Parser.source Lexer.read filebuf in
    Ast.sexp_of_comp_unit ast |> Sexp.to_string_hum |> print_endline;
    ast
  with _ ->
    make_parse_error_msg filebuf |> print_endline;
    exit 1

let init_ctx = ([ Map.Poly.empty ], [ Map.Poly.empty ])

let run filename =
  In_channel.with_file filename ~f:(fun file ->
      Lexing.from_channel file |> parse_to_ast_exn
      |> Typechecker.typecheck init_ctx)

let main () =
  Command.basic ~summary:"simple compiler for SysY"
    (let%map_open.Command filename =
       anon (maybe_with_default "test.sy" ("filename" %: string))
     in
     fun () -> run filename)
  |> Command_unix.run

let () = main ()
