open Ast
open Core
open Tree
open Table
open Type
open Util

[@@@warning "-8"]

let try_check f tr =
  try f () with
  | SemanticErrorWithCurTree (msg, tree) ->
      raise (SemanticErrorWithCurTreeParentTree (msg, tree, tr))
  | SemanticError msg -> raise (SemanticErrorWithCurTree (msg, tr))

(* check the type of the first arg
   * and add the current tree into exception if check failed *)
let rec check1 ctxes (ast : ast) (ty : value_type) : unit =
  try get_exp_exn ctxes ast == ty
  with SemanticError msg ->
    raise (SemanticErrorWithCurTree (msg, ast_to_tree ast))

(* add context to check and add parent tree into exception*)

(* take an exp, return its type *)
and get_exp ctxes exp : value_type =
  let check1 = check1 ctxes in
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
  | Exp exp -> get_exp_exn ctxes exp
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

and get_exp_exn ctxes exp =
  let f () = get_exp ctxes exp in
  try_check f (ast_to_tree exp)

(* take an ast, ctxes, return the updated ctxes *)
let rec update_ctxes ctxes ast : ctxes =
  let check1 = check1 ctxes in
  let set_ctxes = set_ctxes ctxes in
  match ast with
  | Decl def_list -> List.fold_left def_list ~init:ctxes ~f:update_ctxes_exn
  | DefVar (id, exp) ->
      check1 exp IntType;
      set_ctxes Var id IntType
  | DefArr (id, []) -> set_ctxes Var id IntType
  | DefArr (id, dims) -> set_ctxes Var id (ArrayType (drop_head_n dims 1))
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
      let new_ctxes = set_ctxes Fun id func_type in
      let ctxes = push_new_var_ctx new_ctxes new_vars in
      typecheck_body ctxes block return_type;
      new_ctxes
  | _ -> ctxes

and update_ctxes_exn ctxes ast =
  let f () = update_ctxes ctxes ast in
  try_check f (ast_to_tree ast)

(*typecheck function body, return unit*)
and typecheck_body ctxes body return_type : unit =
  let (Block body) = body in
  ignore
    (List.fold_left body ~init:ctxes ~f:(fun ctxes block_item ->
         match block_item with
         | Decl _ as decl -> update_ctxes_exn ctxes decl
         | Stmt stmt ->
             check_stmt_exn ctxes stmt return_type;
             ctxes))

(* type check stmt, return unit *)
and check_stmt ctxes stmt return_type =
  let check1 = check1 ctxes in
  let check_stmt_exn = check_stmt_exn ctxes in
  match stmt with
  | Assign (lval, exp) ->
      check1 lval IntType;
      check1 exp IntType
  | Block _ as block ->
      let new_ctxes = push_new_var_ctx ctxes [] in
      typecheck_body new_ctxes block return_type
  | IfElse (guard, then_, else_) ->
      check1 guard IntType;
      check_stmt_exn then_ return_type;
      check_stmt_exn else_ return_type
  | IfThen (guard, then_) ->
      check1 guard IntType;
      check_stmt_exn then_ return_type
  | While (guard, stmt) ->
      check1 guard IntType;
      check_stmt_exn stmt return_type
  | Break | Continue -> ()
  | ReturnNone -> return_type == VoidType
  | Return exp -> check1 exp return_type
  | Exp exp -> ignore (get_exp_exn ctxes exp)

and check_stmt_exn ctxes stmt return_type =
  let f () = check_stmt ctxes stmt return_type in
  try_check f (ast_to_tree stmt)

(* the clean entry for typechecking*)
let typecheck_exn ctxes program =
  List.fold_left program ~init:ctxes ~f:update_ctxes_exn

(* wrapped entry with exception handling*)
let typecheck ctxes ast : unit =
  let handleSemanticError msg tree1 tree2 =
    print_endline msg;
    (match (tree1, tree2) with
    | Some tree1, Some tree2 ->
        PrintBox_text.output stdout tree1;
        print_endline "\n-----------------in--------------------\n";
        PrintBox_text.output stdout tree2
    | Some tree1, None -> PrintBox_text.output stdout tree1
    | _ -> ());
    exit 1
  in
  let (CompUnit program) = ast in
  ignore
    (try typecheck_exn ctxes program with
    | SemanticErrorWithCurTreeParentTree (msg, tree1, tree2) ->
        handleSemanticError ("Semantic Error: " ^ msg) (Some tree1) (Some tree2)
    | SemanticErrorWithCurTree (msg, tree) ->
        handleSemanticError msg (Some tree) None
    | SemanticError msg ->
        handleSemanticError ("Semantic Error: " ^ msg) None None)

[@@@warning "+8"]
