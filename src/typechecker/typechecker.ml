open Core
open Lib.Ast
open Lib.Type
open Lib.Err
open Lib.Tree
open Table
open Util

[@@@warning "-8"]

(* check the type of the first arg
   * and add the current tree into exception if check failed *)
let rec check_type_exn ctxes (ast : ast) (ty : value_type) : unit =
  let f () = get_exp_exn ctxes ast == ty in
  catch_raise_with_ast f (ast_to_tree ast)

(* add context to check and add parent tree into exception*)

(* take an exp, return its type *)
and get_exp ctxes exp : value_type =
  let check_type_exn = check_type_exn ctxes in
  match exp with
  | Or (left_exp, right_exp)
  | And (left_exp, right_exp)
  | Eq (left_exp, right_exp)
  | Neq (left_exp, right_exp)
  | Rel (left_exp, _, right_exp)
  | Add (left_exp, right_exp)
  | Sub (left_exp, right_exp)
  | Mul (left_exp, _, right_exp) ->
      check_type_exn left_exp IntType;
      check_type_exn right_exp IntType;
      IntType
  | UnaryOp (_, unary_exp) ->
      check_type_exn unary_exp IntType;
      IntType
  | Call (id, args) ->
      let return_type, paras = lookup_function_content ctxes id in
      zip args paras
      |> List.iter ~f:(fun (arg, para) -> check_type_exn arg para);
      return_type
  | Exp exp -> get_exp_exn ctxes exp
  | Lval (id, idxes) -> (
      match lookup_var ctxes id with
      | IntType ->
          if list_nonempty idxes then
            raise_semantic_error (sp "can not index over an int:%s" id);
          IntType
      | ArrayType dims ->
          List.iter idxes ~f:(fun dim -> check_type_exn dim IntType);
          let l_dims = List.length dims in
          let l_idxes = List.length idxes in
          if l_idxes = 0 then ArrayType dims
          else if l_dims = l_idxes - 1 then IntType
          else if l_dims > l_idxes - 1 then ArrayType (drop_head_n dims l_idxes)
          else raise_semantic_error "the dimension of indexes is too much"
      | _ -> raise_semantic_error "the lval must be of either int or array")
  | Number _ -> IntType

and get_exp_exn ctxes exp =
  let f () = get_exp ctxes exp in
  catch_raise_with_ast f (ast_to_tree exp)

(* take an ast, ctxes, return the updated ctxes *)
let rec update_ctxes ctxes ast : ctxes =
  let check_type_exn = check_type_exn ctxes in
  let update_var_ctx = update_var_ctx ctxes in
  let update_fun_ctx = update_fun_ctx ctxes in
  match ast with
  | Decl def_list -> List.fold_left def_list ~init:ctxes ~f:update_ctxes_exn
  | DefVar (id, exp) ->
      check_type_exn exp IntType;
      update_var_ctx id IntType
  | DefArr (id, []) -> update_var_ctx id IntType
  | DefArr (id, dims) -> update_var_ctx id (ArrayType (drop_head_n dims 1))
  | FuncDef (return_type, id, params, block) ->
      let params_to_types = function
        | IntParam _ -> IntType
        | ArrParam (_, dims) -> ArrayType dims
      in
      let params_to_vars = function
        | IntParam id -> (id, IntType)
        | ArrParam (id, dims) -> (id, ArrayType dims)
      in
      let params_type = List.map params ~f:params_to_types in
      let ctxes = update_fun_ctx id (FuncType (return_type, params_type)) in
      let new_vars = List.map params ~f:params_to_vars in
      typecheck_block_exn ctxes block return_type new_vars;
      ctxes
  | _ -> ctxes

and update_ctxes_exn ctxes ast =
  let f () = update_ctxes ctxes ast in
  catch_raise_with_ast f (ast_to_tree ast)

(*typecheck function body, return unit*)
and typecheck_block ctxes (Block body) return_type init_table : unit =
  let ctxes = push_new_var_ctx ctxes init_table in
  let update_block_item ctxes = function
    | Decl _ as decl -> update_ctxes_exn ctxes decl
    | Stmt stmt ->
        check_stmt_exn ctxes stmt return_type;
        ctxes
  in
  ignore (List.fold_left body ~init:ctxes ~f:update_block_item)

and typecheck_block_exn ctxes block return_type init_table =
  let f () = typecheck_block ctxes block return_type init_table in
  catch_raise_with_ast f (ast_to_tree block)

(* type check stmt, return unit *)
and check_stmt ctxes stmt return_type =
  let check_type_exn = check_type_exn ctxes in
  let check_stmt_exn = check_stmt_exn ctxes in
  match stmt with
  | Assign (lval, exp) ->
      check_type_exn lval IntType;
      check_type_exn exp IntType
  | Block _ as block -> typecheck_block_exn ctxes block return_type []
  | IfElse (guard, then_, else_) ->
      check_type_exn guard IntType;
      check_stmt_exn then_ return_type;
      check_stmt_exn else_ return_type
  | IfThen (guard, then_) ->
      check_type_exn guard IntType;
      check_stmt_exn then_ return_type
  | While (guard, stmt) ->
      check_type_exn guard IntType;
      check_stmt_exn stmt return_type
  | Break | Continue -> ()
  | ReturnNone -> return_type == VoidType
  | Return exp -> check_type_exn exp return_type
  | Exp exp -> ignore (get_exp_exn ctxes exp)

and check_stmt_exn ctxes stmt return_type =
  let f () = check_stmt ctxes stmt return_type in
  catch_raise_with_ast f (ast_to_tree stmt)

(* the clean entry for typechecking*)
let typecheck_exn ctxes program =
  List.fold_left program ~init:ctxes ~f:update_ctxes_exn

(* wrapped entry with exception handling*)
let typecheck ast : unit =
  let ctxes = init_ctxes () in
  let (CompUnit program) = ast in
  let f () = typecheck_exn ctxes program in
  ignore (handleSemanticError f)

[@@@warning "+8"]
