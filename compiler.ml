open Ast
open Core

let run filename =
  let input = In_channel.create filename in
  let filebuf = Lexing.from_channel input in
  match
    try Ok (Parser.source Lexer.read filebuf)
    with _ ->
      let start_pos = Lexing.lexeme_start_p filebuf in
      let end_pos = Lexing.lexeme_end_p filebuf in
      Error
        (Printf.sprintf "Parse error: line %d, characters %d-%d"
           start_pos.pos_lnum
           (start_pos.pos_cnum - start_pos.pos_bol)
           (end_pos.pos_cnum - end_pos.pos_bol))
  with
  | Ok c -> print_endline (Sexp.to_string_hum (sexp_of_comp_unit c))
  | Error e ->
      print_endline e;
      exit 1

let main () =
  let open Command.Let_syntax in
  Command.basic ~summary:"simple interpreter"
    [%map_open
      let filename =
        anon (maybe_with_default "test.sy" ("filename" %: string))
      in
      fun () -> run filename]
  |> Command_unix.run

let () = main ()
