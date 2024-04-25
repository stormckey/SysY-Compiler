open Core
open Lib.Runtime
open Cli

let run filename =
  runtime ^ In_channel.read_all filename
  |> Lexing.from_string |> parse_to_ast |> Type_checker.Typechecker.typecheck;
  print_endline "Type Checked"

let () = main run
