open Core
open Lib.Runtime
open Cli

let run filename =
  let init_ctx = ([ Map.Poly.empty ], [ Map.Poly.empty ]) in
  runtime ^ In_channel.read_all filename
  |> Lexing.from_string |> parse_to_ast
  |> Type_checker.Typechecker.typecheck init_ctx;
  print_endline "Type Checked"

let () = main run
