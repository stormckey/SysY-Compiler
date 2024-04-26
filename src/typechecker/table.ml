open Core
open Lib.Type
open Lib.Err

let sp = Printf.sprintf

type table = (string, value_type) Map.Poly.t
type ctx = table list
type ctxes = { fun_ctx : ctx; var_ctx : ctx } (* func_ctx, var_ctx*)

let init_ctxes = { fun_ctx = [ Map.Poly.empty ]; var_ctx = [ Map.Poly.empty ] }

let lookup (ctx : ctx) (id : string) : value_type option =
  List.find_map ctx ~f:(fun scope -> Map.Poly.find scope id)

let lookup_function_content ctxes id =
  match lookup ctxes.fun_ctx id with
  | None -> raise_semantic_error (sp "function:%s is not defined" id)
  | Some (FuncType (return_type, paras)) -> (return_type, paras)
  | _ -> raise_semantic_error (sp "try to call on non-function: %s" id)

let lookup_var ctxes id =
  match lookup ctxes.var_ctx id with
  | None -> raise_semantic_error (Printf.sprintf "id:%s is not defined" id)
  | Some var -> var

let update_table table id ty =
  match Map.Poly.find table id with
  | None -> Map.Poly.set table ~key:id ~data:ty
  | Some _ -> raise_semantic_error (sp "Variable/Function %s already exist" id)

let update_ctx (ctx : ctx) (id : string) (ty : value_type) : ctx =
  match ctx with
  | [] -> raise_semantic_error "empty ctx can't be updated"
  | hd :: tl -> update_table hd id ty :: tl

let update_fun_ctx ctxes id ty =
  { ctxes with fun_ctx = update_ctx ctxes.fun_ctx id ty }

let update_var_ctx ctxes id ty =
  { ctxes with var_ctx = update_ctx ctxes.var_ctx id ty }

let push_new_var_ctx ctxes init =
  { ctxes with var_ctx = Map.Poly.of_alist_exn init :: ctxes.var_ctx }
(* (fst ctxes, Map.Poly.of_alist_exn init :: snd ctxes) *)
