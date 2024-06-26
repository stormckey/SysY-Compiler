open Core
open Lib
open Sysy_parser

let make_parse_error_msg filebuf =
  let column_offset (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol in
  let start_pos = Lexing.lexeme_start_p filebuf in
  let start_char = column_offset start_pos in
  let end_char = Lexing.lexeme_end_p filebuf |> column_offset in
  Printf.sprintf "Syntax Error: line %d, characters %d-%d"
    (start_pos.pos_lnum - 7) start_char end_char

let parse_to_ast inputbuf =
  try
    let ast = Parser.source Lexer.read inputbuf in
    Tree.program_to_tree ast |> PrintBox_text.output stdout;
    print_endline "";
    ast
  with _ ->
    make_parse_error_msg inputbuf |> print_endline;
    exit 1

let main run =
  Command.basic ~summary:"simple compiler for SysY"
    (let%map_open.Command filename =
       anon (maybe_with_default "test.sy" ("filename" %: string))
     in
     fun () -> run filename)
  |> Command_unix.run
