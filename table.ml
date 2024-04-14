open Core
open Type

type table = (string, value_type) Map.Poly.t
type ctx = table list
type ctxes = ctx * ctx (* func_ctx, var_ctx*)

let lookup (ctx : ctx) (name : string) : value_type option =
  let rec traverse ctx name =
    match ctx with
    | [] -> None
    | hd :: tl -> (
        match Map.Poly.find hd name with
        | None -> traverse tl name
        | Some ty -> Some ty)
  in
  traverse ctx name
