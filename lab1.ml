open Core
open Runtime

let make_parse_error_msg filebuf =
  let diff_pos (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol in
  let start_pos = Lexing.lexeme_start_p filebuf in
  let end_pos = Lexing.lexeme_end_p filebuf in
  let start_char = diff_pos start_pos in
  let end_char = diff_pos end_pos in
  Printf.sprintf "Parse error: line %d, characters %d-%d"
    (start_pos.pos_lnum - 4) start_char end_char

let parse_to_ast_exn inputbuf =
  try
    let ast = Parser.source Lexer.read inputbuf in
    Tree.ast_to_tree ast |> PrintBox_text.output stdout
  with _ ->
    make_parse_error_msg inputbuf |> print_endline;
    exit 1

let run filename =
  runtime ^ In_channel.read_all filename
  |> Lexing.from_string |> parse_to_ast_exn

let main () =
  Command.basic ~summary:"simple compiler for SysY"
    (let%map_open.Command filename =
       anon (maybe_with_default "test.sy" ("filename" %: string))
     in
     fun () -> run filename)
  |> Command_unix.run

let () = main ()
