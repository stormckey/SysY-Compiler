open Ast
open Core
open Lexer
open Lexing

let run filename = 
  let input = In_channel.create filename in
  let filebuf = Lexing.from_channel input in
  let s = Parser.source Lexer.read filebuf in
  print_endline (Sexp.to_string_hum (sexp_of_comp_unit s))

let main () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"simple interpreter"
    [%map_open
      let filename = anon (maybe_with_default "test.lam" ("filename" %: string))
    in 
    fun () -> run filename]
  |> Command.run
  
let () = main ()

