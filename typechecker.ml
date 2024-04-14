open Ast
open Core
open Tree
open Table
open Type
open Err
open Util

let rec typecheck_lval ctxes (name, exp_list) =
  let self_tree = tree_of_lval (name, exp_list) in
  let check_type_mismatch = check_type_mismatch self_tree in
  let check_all_int () =
    List.iter exp_list ~f:(fun exp ->
        check_type_mismatch IntType (typecheck_exp ctxes exp) (tree_of_exp exp))
  in
  let get_type_after_indexing dims =
    let l_dims = List.length dims in
    let l_idxes = List.length exp_list in
    if l_dims = l_idxes - 1 then IntType
    else if l_dims < l_idxes - 1 then
      fail_with_tree self_tree "the dimension of indexes is too much"
    else ArrayType (drop_head_n dims l_idxes)
  in
  match lookup (snd ctxes) name with
  | None -> id_not_found_error name
  | Some IntType ->
      if list_nonempty exp_list then
        fail_with_tree self_tree (sp "can not index over an int:%s" name);
      IntType
  | Some (ArrayType dims) ->
      if list_empty exp_list then ArrayType dims
      else (
        check_all_int ();
        get_type_after_indexing dims)
  | _ -> failwith "the lval must be of either int or array"

and typecheck_primary_exp ctxes exp =
  match exp with
  | Exp e -> typecheck_exp ctxes e
  | Lval lval -> typecheck_lval ctxes lval
  | Number _ -> IntType

and typecheck_unary_exp ctxes exp =
  let self_tree = tree_of_unary_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  let check_args_paras args paras =
    zip args paras
    |> List.iter ~f:(fun (arg, para) ->
           check_type_mismatch para (typecheck_exp ctxes arg)
             (tree_of_unary_exp exp))
  in
  match exp with
  | UnaryPrimary primary_exp -> typecheck_primary_exp ctxes primary_exp
  | Call (name, args) -> (
      match lookup (fst ctxes) name with
      | None -> failwith (sp "function:%s is not defined" name)
      | Some (FuncType (return_type, paras)) ->
          check_args_paras args paras;
          return_type
      | _ -> failwith (sp "try to call on non-function: %s" name))
  | UnaryOp (_, unary_exp) ->
      check_type_mismatch IntType
        (typecheck_unary_exp ctxes unary_exp)
        (tree_of_unary_exp unary_exp);
      IntType

and typecheck_mul_exp ctxes exp =
  let self_tree = tree_of_mul_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  match exp with
  | MulUnary unary_exp -> typecheck_unary_exp ctxes unary_exp
  | MulMul (mul_exp, _, unary_exp) ->
      check_type_mismatch IntType
        (typecheck_mul_exp ctxes mul_exp)
        (tree_of_mul_exp mul_exp);
      check_type_mismatch IntType
        (typecheck_unary_exp ctxes unary_exp)
        (tree_of_unary_exp unary_exp);
      IntType

and typecheck_add_exp ctxes exp =
  let self_tree = tree_of_add_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  match exp with
  | AddMul mul_exp -> typecheck_mul_exp ctxes mul_exp
  | AddAdd (add_exp, mul_exp) | AddSub (add_exp, mul_exp) ->
      check_type_mismatch IntType
        (typecheck_add_exp ctxes add_exp)
        (tree_of_add_exp add_exp);
      check_type_mismatch IntType
        (typecheck_mul_exp ctxes mul_exp)
        (tree_of_mul_exp mul_exp);
      IntType

and typecheck_rel_exp ctxes exp =
  let self_tree = tree_of_rel_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  match exp with
  | RelAdd add_exp -> typecheck_add_exp ctxes add_exp
  | RelRel (rel_exp, _, add_exp) ->
      check_type_mismatch IntType
        (typecheck_add_exp ctxes add_exp)
        (tree_of_add_exp add_exp);
      check_type_mismatch IntType
        (typecheck_rel_exp ctxes rel_exp)
        (tree_of_rel_exp rel_exp);
      IntType

and typecheck_eq_exp ctxes exp =
  let self_tree = tree_of_eq_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  match exp with
  | EqRel rel_exp -> typecheck_rel_exp ctxes rel_exp
  | EqEq (eq_exp, rel_exp) | EqNeq (eq_exp, rel_exp) ->
      check_type_mismatch IntType
        (typecheck_eq_exp ctxes eq_exp)
        (tree_of_eq_exp eq_exp);
      check_type_mismatch IntType
        (typecheck_rel_exp ctxes rel_exp)
        (tree_of_rel_exp rel_exp);
      IntType

and typecheck_l_and_exp ctxes exp =
  let self_tree = tree_of_l_and_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  match exp with
  | AndEq andeq -> typecheck_eq_exp ctxes andeq
  | AndAnd (l_and_exp, eq_exp) ->
      check_type_mismatch IntType
        (typecheck_l_and_exp ctxes l_and_exp)
        (tree_of_l_and_exp l_and_exp);
      check_type_mismatch IntType
        (typecheck_eq_exp ctxes eq_exp)
        (tree_of_eq_exp eq_exp);
      IntType

and typecheck_exp ctxes exp =
  let self_tree = tree_of_exp exp in
  let check_type_mismatch = check_type_mismatch self_tree in
  match exp with
  | OrAnd orand -> typecheck_l_and_exp ctxes orand
  | OrOr (l_or_exp, l_and_exp) ->
      check_type_mismatch IntType
        (typecheck_l_and_exp ctxes l_and_exp)
        (tree_of_l_and_exp l_and_exp);
      check_type_mismatch IntType
        (typecheck_exp ctxes l_or_exp)
        (tree_of_exp l_or_exp);
      IntType

let update_var_def ctxes = function
  | DefVar (id, exp) ->
      check_type_mismatch (tree_of_exp exp) IntType (typecheck_exp ctxes exp)
        (tree_of_exp exp);
      update_ctxes Var ctxes id IntType
  | DefArr (id, []) -> update_ctxes Var ctxes id IntType
  | DefArr (id, dims) ->
      update_ctxes Var ctxes id (ArrayType (drop_head_n dims 1))

let update_ctx_list ctxes update l =
  List.fold_left l ~init:ctxes ~f:(fun ctxes item -> update ctxes item)

let update_decl ctxes decl = update_ctx_list ctxes update_var_def decl

let rec update_block_item expected ctxes block_item =
  match block_item with
  | DeclLocal (_, decl) -> update_decl ctxes decl
  | Stmt stmt ->
      check_stmt ctxes stmt expected;
      ctxes

and check_block ctxes block expected =
  update_ctx_list ctxes (update_block_item expected) block

and check_stmt ctxes stmt expected =
  let self_tree = tree_of_stmt stmt in
  let check_type_mismatch = check_type_mismatch self_tree in
  match stmt with
  | Assign (lval, exp) ->
      check_type_mismatch IntType
        (typecheck_lval ctxes lval)
        (tree_of_lval lval);
      check_type_mismatch IntType (typecheck_exp ctxes exp) (tree_of_stmt stmt)
  | Expr exp -> ignore (typecheck_exp ctxes exp)
  | Block block ->
      ignore ((push_new_var_ctx ctxes [] |> check_block) block expected)
  | If (guard, then_, else_maybe) -> (
      check_type_mismatch IntType
        (typecheck_exp ctxes guard)
        (tree_of_exp guard);
      check_stmt ctxes then_ expected;
      match else_maybe with
      | Some else_ -> check_stmt ctxes else_ expected
      | None -> ())
  | While (guard, stmt) ->
      check_type_mismatch IntType
        (typecheck_exp ctxes guard)
        (tree_of_exp guard);
      check_stmt ctxes stmt expected
  | Break -> ()
  | Continue -> ()
  | Return exp -> (
      match exp with
      | None -> check_type_mismatch expected VoidType self_tree
      | Some exp ->
          check_type_mismatch expected (typecheck_exp ctxes exp)
            (tree_of_exp exp))

let update_func_def ctxes (return_type, name, func_f_params, block) =
  let new_vars =
    List.map func_f_params ~f:(function
      | IntParam (_, id) -> (id, IntType)
      | ArrParam (_, id, dims) -> (id, ArrayType dims))
  in
  let param_types =
    List.map func_f_params ~f:(function
      | IntParam _ -> IntType
      | ArrParam (_, _, dims) -> ArrayType dims)
  in
  let return_type =
    match return_type with Void -> VoidType | Int -> IntType
  in
  let func_type = FuncType (return_type, param_types) in
  let ctxes = update_ctxes Fun ctxes name func_type in
  let ctxes = push_new_var_ctx ctxes new_vars in
  ignore (check_block ctxes block return_type);
  ctxes

let update_either_decl_funcdef ctxes = function
  | DeclGlobal (_, decl) -> update_decl ctxes decl
  | FuncDef func_def -> update_func_def ctxes func_def

let typecheck ctxes program : unit =
  ignore (update_ctx_list ctxes update_either_decl_funcdef program)
