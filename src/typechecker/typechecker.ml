open Core
open Lib.Ast
open Lib.Type
open Lib.Err
open Lib.Tree
open Table
open Util

(* get the type of the first arg and compare it to the second one
 * add the current tree into exception if check failed *)
let rec check_type_exn ctxes (ast : exp) (ty : value_type) : unit =
  let f () = get_exp_type_exn ctxes ast == ty in
  catch_raise_with_ast f (exp_to_tree ast)

(* take an exp, return its type *)
and get_exp_type ctxes exp : value_type =
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
  | Lval (id, idxes) -> (
      let l_idxes = List.length idxes in
      match lookup_var ctxes id with
      | IntType ->
          if l_idxes <> 0 then
            raise_semantic_error (sp "can not index over an int:%s" id);
          IntType
      | ArrayType dims ->
          let l_dims = List.length dims in
          List.iter idxes ~f:(fun dim -> check_type_exn dim IntType);
          if l_idxes = 0 then ArrayType dims
          else if l_dims = l_idxes - 1 then IntType
          else if l_dims > l_idxes - 1 then ArrayType (drop_head_n dims l_idxes)
          else raise_semantic_error "the dimension of indexes is too much"
      | _ -> raise_semantic_error "the lval must be of either int or array")
  | Number _ -> IntType

and get_exp_type_exn ctxes exp =
  let f () = get_exp_type ctxes exp in
  catch_raise_with_ast f (exp_to_tree exp)

(* take an ast, ctxes, return the updated ctxes *)
let rec ctxes_after_decl ctxes (decl : decl) : ctxes =
  let update_fun_ctx = update_fun_ctx ctxes in
  match decl with
  | DefVar def_var_list ->
      List.fold_left def_var_list ~init:ctxes ~f:ctxes_after_var_exn
  | DefFunc (return_type, id, params, block) ->
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

and ctxes_after_decl_exn ctxes (decl : decl) =
  let f () = ctxes_after_decl ctxes decl in
  catch_raise_with_ast f (decl_to_tree decl)

and ctxes_after_var ctxes (def_var : decl_var) =
  let check_type_exn = check_type_exn ctxes in
  let update_var_ctx = update_var_ctx ctxes in
  match def_var with
  | DefInt (id, exp) ->
      check_type_exn exp IntType;
      update_var_ctx id IntType
  | DefArr (id, []) -> update_var_ctx id IntType
  | DefArr (id, dims) -> update_var_ctx id (ArrayType (drop_head_n dims 1))

and ctxes_after_var_exn ctxes (def_var : decl_var) =
  let f () = ctxes_after_var ctxes def_var in
  catch_raise_with_ast f (decl_var_to_tree def_var)

(*typecheck function body, return unit*)
and typecheck_block ctxes (body : block) return_type init_table : unit =
  let ctxes = push_new_var_ctx ctxes init_table in
  let update_block_item ctxes = function
    | BlockDecl decl -> ctxes_after_decl_exn ctxes decl
    | BlockStmt stmt ->
        check_stmt_exn ctxes stmt return_type;
        ctxes
  in
  ignore (List.fold_left body ~init:ctxes ~f:update_block_item)

and typecheck_block_exn ctxes (block : block) return_type init_table =
  let f () = typecheck_block ctxes block return_type init_table in
  catch_raise_with_ast f (block_to_tree block)

(* type check stmt, return unit *)
and check_stmt ctxes stmt return_type =
  let check_type_exn = check_type_exn ctxes in
  let check_stmt_exn = check_stmt_exn ctxes in
  match stmt with
  | Assign (lval, exp) ->
      check_type_exn lval IntType;
      check_type_exn exp IntType
  | Block block -> typecheck_block_exn ctxes block return_type []
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
  | Exp exp -> ignore (get_exp_type_exn ctxes exp)

and check_stmt_exn ctxes (stmt : stmt) return_type =
  let f () = check_stmt ctxes stmt return_type in
  catch_raise_with_ast f (stmt_to_tree stmt)

(* the clean entry for typechecking*)
let typecheck_exn (ctxes : ctxes) (program : decl list) =
  List.fold_left program ~init:ctxes ~f:ctxes_after_decl_exn

(* wrapped entry with exception handling*)
let typecheck (program : decl list) : unit =
  let f () = typecheck_exn init_ctxes program in
  ignore (handleSemanticError f)
