open Ast
open Core
open Tree
open Table
open Type
open Util

[@@@warning "-8"]

(* check the type of the first arg
   * and add the current tree into exception if check failed *)
let rec check1 ctxes (ast : ast) (ty : value_type) : unit =
  try check_exp ctxes ast == ty
  with SemanticError msg ->
    raise (SemanticErrorWithCurTree (msg, ast_to_tree ast))

(* add context to check and add parent tree into exception*)
and make_check1 ctxes ast =
  let check1 a b =
    try check1 ctxes a b
    with SemanticErrorWithCurTree (msg, tree) ->
      raise (SemanticErrorWithCurTreeParentTree (msg, tree, ast_to_tree ast))
  in
  check1

(* take an exp, return its type *)
and check_exp ctxes exp : value_type =
  let check1 = make_check1 ctxes exp in
  match exp with
  | Or (left_exp, right_exp)
  | And (left_exp, right_exp)
  | Eq (left_exp, right_exp)
  | Neq (left_exp, right_exp)
  | Rel (left_exp, _, right_exp)
  | Add (left_exp, right_exp)
  | Sub (left_exp, right_exp)
  | Mul (left_exp, _, right_exp) ->
      check1 left_exp IntType;
      check1 right_exp IntType;
      IntType
  | UnaryOp (_, unary_exp) ->
      check1 unary_exp IntType;
      IntType
  | Call (id, args) -> (
      match lookup (fst ctxes) id with
      | None -> raise (SemanticError (sp "function:%s is not defined" id))
      | Some (FuncType (return_type, paras)) ->
          zip args paras |> List.iter ~f:(fun (arg, para) -> check1 arg para);
          return_type
      | _ -> raise (SemanticError (sp "try to call on non-function: %s" id)))
  | Exp exp -> check_exp ctxes exp
  | Lval (id, idxes) -> (
      match lookup (snd ctxes) id with
      | None -> id_not_found_error id
      | Some IntType ->
          if list_nonempty idxes then
            raise (SemanticError (sp "can not index over an int:%s" id));
          IntType
      | Some (ArrayType dims) ->
          if list_empty idxes then ArrayType dims
          else (
            List.iter idxes ~f:(fun dim -> check1 dim IntType);
            let l_dims = List.length dims in
            let l_idxes = List.length idxes in
            if l_dims = l_idxes - 1 then IntType
            else if l_dims < l_idxes - 1 then
              raise (SemanticError "the dimension of indexes is too much")
            else ArrayType (drop_head_n dims l_idxes))
      | _ -> raise (SemanticError "the lval must be of either int or array"))
  | Number _ -> IntType

(* take an ast, ctxes, return the updated ctxes *)
let rec update_ctxes ctxes ast : ctxes =
  let check1 = make_check1 ctxes ast in
  match ast with
  | Decl def_list -> List.fold_left def_list ~init:ctxes ~f:update_ctxes
  | DefVar (id, exp) ->
      check1 exp IntType;
      set_ctxes Var ctxes id IntType
  | DefArr (id, []) -> set_ctxes Var ctxes id IntType
  | DefArr (id, dims) -> set_ctxes Var ctxes id (ArrayType (drop_head_n dims 1))
  | FuncDef (return_type, id, paras, block) ->
      let new_vars, param_types =
        List.fold_right paras ~init:([], [])
          ~f:(fun param (vars_acc, types_acc) ->
            match param with
            | IntParam id -> ((id, IntType) :: vars_acc, IntType :: types_acc)
            | ArrParam (id, dims) ->
                ((id, ArrayType dims) :: vars_acc, ArrayType dims :: types_acc))
      in
      let func_type = FuncType (return_type, param_types) in
      let new_ctxes = set_ctxes Fun ctxes id func_type in
      let ctxes = push_new_var_ctx new_ctxes new_vars in
      typecheck_body ctxes block return_type;
      new_ctxes
  | _ -> ctxes

(*typecheck function body, return unit*)
and typecheck_body ctxes body return_type : unit =
  ignore
    (List.fold_left body ~init:ctxes ~f:(fun ctxes block_item ->
         match block_item with
         | Decl _ as decl -> update_ctxes ctxes decl
         | Stmt stmt ->
             check_stmt ctxes stmt return_type;
             ctxes))

(* type check stmt, return unit *)
and check_stmt ctxes stmt return_type =
  let check1 = make_check1 ctxes stmt in
  match stmt with
  | Assign (lval, exp) ->
      check1 lval IntType;
      check1 exp IntType
  | Block block ->
      let new_ctxes = push_new_var_ctx ctxes [] in
      typecheck_body new_ctxes block return_type
  | If (guard, then_, else_maybe) -> (
      check1 guard IntType;
      check_stmt ctxes then_ return_type;
      match else_maybe with
      | Some else_ -> check_stmt ctxes else_ return_type
      | None -> ())
  | While (guard, stmt) ->
      check1 guard IntType;
      check_stmt ctxes stmt return_type
  | Break | Continue -> ()
  | Return exp -> (
      match exp with
      | None -> return_type == VoidType
      | Some exp -> check1 exp return_type)
  | Exp exp -> ignore (check_exp ctxes exp)

(* the clean entry for typechecking*)
let typecheck_exn ctxes program =
  List.fold_left program ~init:ctxes ~f:update_ctxes

(* wrapped entry with exception handling*)
let typecheck ctxes ast : unit =
  let (CompUnit program) = ast in
  let pt tree =
    PrintBox_text.output stdout tree;
    print_endline ""
  in
  ignore
    (try typecheck_exn ctxes program with
    | SemanticErrorWithCurTreeParentTree (msg, tree1, tree2) ->
        print_endline msg;
        pt tree1;
        print_endline "-----------------in--------------------";
        pt tree2;
        exit 1
    | SemanticErrorWithCurTree (msg, tree) ->
        print_endline msg;
        pt tree;
        exit 1)

[@@@warning "+8"]
