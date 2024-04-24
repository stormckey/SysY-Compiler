open Core
open Cli
open Lib.Runtime

let run filename =
  ignore
    (runtime ^ In_channel.read_all filename
    |> Lexing.from_string |> parse_to_ast)

let () = main run
