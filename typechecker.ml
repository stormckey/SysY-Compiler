open Ast
open Core

type table = (string, value_type) Map.Poly.t
type ctx = table list
type ctxes = ctx * ctx (* func_ctx, var_ctx*)

let sp = Printf.sprintf
let pe = print_endline
let ty_to_string ty = sexp_of_value_type ty |> Sexp.to_string_hum

let make_type_mismatch ty1 ty2 =
  sp "Type mismatch, expect %s but get %s:" (ty_to_string ty1)
    (ty_to_string ty2)

let check_type_error expected actual convert exp =
  failwith
    (make_type_mismatch expected actual ^ (convert exp |> Sexp.to_string_hum))

let id_not_found_error id = failwith (sp "id:%s not defined" id)
let ( == ) a b = equal_value_type a b
let ( != ) a b = not (a == b)
let list_empty l = List.length l = 0
let list_nonempty l = not (list_empty l)

let zip l1 l2 =
  let rec aux l1 l2 acc =
    match (l1, l2) with
    | [], [] -> List.rev acc
    | h1 :: t1, h2 :: t2 -> aux t1 t2 ((h1, h2) :: acc)
    | _, _ ->
        failwith "the length of args is different from the length of params"
  in
  aux l1 l2 []

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

let rec check_primary_exp ctxes exp expected =
  match exp with
  | Exp e -> check_exp ctxes e expected
  | Lval (name, exp_list) -> (
      match lookup (snd ctxes) name with
      | None -> id_not_found_error name
      | Some ty -> (
          match ty with
          | IntType ->
              if list_nonempty exp_list then
                failwith (sp "try to treat %s:int as array" name)
          | ArrayType ->
              if list_empty exp_list then (
                if expected != ArrayType then
                  failwith "try to assign an array to non-array")
              else if expected != IntType then
                failwith "try to assign an int to non-int"
          | _ -> failwith "the lval must be of either int or array"))
  | Number _ ->
      if expected != IntType then
        check_type_error expected IntType sexp_of_primary_exp exp

and check_unary_exp ctxes exp expected =
  match exp with
  | UnaryPrimary primary_exp -> check_primary_exp ctxes primary_exp expected
  | Call (name, func_r_params) -> (
      match lookup (fst ctxes) name with
      | None -> failwith (sp "function:%s is not defined" name)
      | Some ty -> (
          match ty with
          | FuncType (return_type, args) ->
              zip args func_r_params
              |> List.iter ~f:(fun (arg, para) -> check_exp ctxes para arg);
              if return_type != expected then
                failwith
                  (sp "the return type of function: %s is different from %s"
                     name (ty_to_string expected))
          | _ -> failwith (sp "try to call on non-function: %s" name)))
  | UnaryOp (_, unary_exp) ->
      if expected == IntType then check_unary_exp ctxes unary_exp IntType
      else check_type_error expected IntType sexp_of_unary_exp exp

and check_mul_exp ctxes exp expected =
  match exp with
  | MulUnary unary_exp -> check_unary_exp ctxes unary_exp expected
  | MulMul (mul_exp, _, unary_exp) ->
      if expected == IntType then (
        check_mul_exp ctxes mul_exp IntType;
        check_unary_exp ctxes unary_exp IntType)
      else check_type_error expected BoolType sexp_of_mul_exp exp

and check_add_exp ctxes exp expected =
  match exp with
  | AddMul mul_exp -> check_mul_exp ctxes mul_exp expected
  | AddAdd (add_exp, mul_exp) | AddSub (add_exp, mul_exp) ->
      if expected == IntType then (
        check_add_exp ctxes add_exp IntType;
        check_mul_exp ctxes mul_exp IntType)
      else check_type_error expected BoolType sexp_of_add_exp exp

and check_rel_exp ctxes exp expected =
  match exp with
  | RelAdd add_exp -> check_add_exp ctxes add_exp expected
  | RelRel (rel_exp, _, add_exp) ->
      if expected == BoolType then (
        check_add_exp ctxes add_exp IntType;
        check_rel_exp ctxes rel_exp IntType)
      else check_type_error expected BoolType sexp_of_rel_exp exp

and check_eq_exp ctxes exp expected =
  match exp with
  | EqRel rel_exp -> check_rel_exp ctxes rel_exp expected
  | EqEq (eq_exp, rel_exp) | EqNeq (eq_exp, rel_exp) ->
      if expected == BoolType then (
        check_eq_exp ctxes eq_exp BoolType;
        check_rel_exp ctxes rel_exp BoolType)
      else check_type_error expected BoolType sexp_of_eq_exp exp

and check_l_and_exp ctxes exp expected =
  match exp with
  | AndEq andeq -> check_eq_exp ctxes andeq expected
  | AndAnd (l_and_exp, eq_exp) ->
      if expected == BoolType then (
        check_l_and_exp ctxes l_and_exp BoolType;
        check_eq_exp ctxes eq_exp BoolType)
      else check_type_error expected BoolType sexp_of_l_and_exp exp

and check_exp ctxes exp expected =
  match exp with
  | OrAnd orand -> check_l_and_exp ctxes orand expected
  | OrOr (l_or_exp, l_and_exp) ->
      if expected == BoolType then (
        check_l_and_exp ctxes l_and_exp BoolType;
        check_exp ctxes l_or_exp BoolType)
      else check_type_error expected BoolType sexp_of_exp exp

let push_new_var_ctx ctxes = (fst ctxes, Map.Poly.empty :: snd ctxes)

type which = Fun | Var

let update_table_exn table name ty =
  match Map.Poly.find table name with
  | None -> Map.Poly.set table ~key:name ~data:ty
  | Some _ -> failwith (sp "Variable %s already exist" name)

let update_ctx (ctx : ctx) (name : string) (ty : value_type) : ctx =
  match ctx with
  | [] -> failwith "empty ctx can't be updated"
  | hd :: tl -> update_table_exn hd name ty :: tl

let update_ctxes which ctxes name ty =
  match which with
  | Fun -> (update_ctx (fst ctxes) name ty, snd ctxes)
  | Var -> (fst ctxes, update_ctx (snd ctxes) name ty)

let update_var_def ctxes var_def =
  match var_def with
  | DefVar (id, exp) ->
      check_exp ctxes exp IntType;
      update_ctxes Var ctxes id IntType
  | DefArr (id, _) -> update_ctxes Var ctxes id ArrayType

let check_stmt _ stmt =
  match stmt with
  | Assign (_, _) -> ()
  | Expr _ -> ()
  | Block _ -> ()
  | If _ -> ()
  | While _ -> ()
  | Break -> ()
  | Continue -> ()
  | Return _ -> ()

let update_ctx_list ctxes update l =
  List.fold_left l ~init:ctxes ~f:(fun ctxes item -> update ctxes item)

let update_decl ctxes decl = update_ctx_list ctxes update_var_def decl

let update_block_item ctxes block_item =
  let ctxes = push_new_var_ctx ctxes in
  match block_item with
  | DeclLocal (_, decl) -> update_decl ctxes decl
  | Stmt stmt ->
      check_stmt ctxes stmt;
      ctxes

let check_block ctxes block = update_ctx_list ctxes update_block_item block

let update_func_def ctxes (return_type, name, func_f_params, block) =
  let _ = check_block ctxes block in
  let param_types =
    List.map func_f_params ~f:(function
      | IntParam _ -> IntType
      | ArrParam _ -> ArrayType)
  in
  let return_type =
    match return_type with Void -> VoidType | Int -> IntType
  in
  let func_type = FuncType (return_type, param_types) in
  update_ctxes Fun ctxes name func_type

let update_either_decl_funcdef ctxes either =
  match either with
  | DeclGlobal (_, decl) -> update_decl ctxes decl
  | FuncDef func_def -> update_func_def ctxes func_def

let typecheck ctxes program : unit =
  let _ = update_ctx_list ctxes update_either_decl_funcdef program in
  print_endline "Type Checked"
