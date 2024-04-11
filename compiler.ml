open Core

let make_parse_error_msg filebuf =
  let start_pos = Lexing.lexeme_start_p filebuf in
  let end_pos = Lexing.lexeme_end_p filebuf in
  let start_char = start_pos.pos_cnum - start_pos.pos_bol in
  let end_char = end_pos.pos_cnum - end_pos.pos_bol in
  Printf.sprintf "Parse error: line %d, characters %d-%d" start_pos.pos_lnum
    start_char end_char

let run filename =
  In_channel.with_file filename ~f:(fun file ->
      let filebuf = Lexing.from_channel file in
      try
        Parser.source Lexer.read filebuf
        |> Ast.sexp_of_comp_unit |> Sexp.to_string_hum |> print_endline
      with _ ->
        make_parse_error_msg filebuf |> print_endline;
        exit 1)

let main () =
  Command.basic ~summary:"simple compiler for SysY"
    (let%map_open.Command filename =
       anon (maybe_with_default "test.sy" ("filename" %: string))
     in
     fun () -> run filename)
  |> Command_unix.run

let () = main ()
