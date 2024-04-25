open Core
open Lib.Type

let sp = Printf.sprintf

type table = (string, value_type) Map.Poly.t
type ctx = table list
type ctxes = { fun_ctx : ctx; var_ctx : ctx } (* func_ctx, var_ctx*)

let lookup (ctx : ctx) (name : string) : value_type option =
  List.find_map ctx ~f:(fun scope -> Map.Poly.find scope name)

let lookup_function ctxes id =
  match lookup ctxes.fun_ctx id with
  | None -> raise (SemanticError (sp "function:%s is not defined" id))
  | Some (FuncType _ as func) -> func
  | _ -> raise (SemanticError (sp "try to call on non-function: %s" id))

let update_table table name ty =
  match Map.Poly.find table name with
  | None -> Map.Poly.set table ~key:name ~data:ty
  | Some _ ->
      raise (SemanticError (sp "Variable/Function %s already exist" name))

let update_ctx (ctx : ctx) (name : string) (ty : value_type) : ctx =
  match ctx with
  | [] -> raise (SemanticError "empty ctx can't be updated")
  | hd :: tl -> update_table hd name ty :: tl

let update_fun_ctx ctxes name ty =
  { ctxes with fun_ctx = update_ctx ctxes.fun_ctx name ty }

let update_var_ctx ctxes name ty =
  { ctxes with var_ctx = update_ctx ctxes.var_ctx name ty }

let push_new_var_ctx ctxes init =
  { ctxes with var_ctx = Map.Poly.of_alist_exn init :: ctxes.var_ctx }
(* (fst ctxes, Map.Poly.of_alist_exn init :: snd ctxes) *)
