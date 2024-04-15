open Ast
open Core
open Type

let t str list = PrintBox.tree (PrintBox.text str) list
let n str = t str []
let unaryop_to_tree = function Pos -> n "+" | Neg -> n "-" | Not -> n "!"
let binop_to_tree = function Div -> n "/" | Mul -> n "*" | Rem -> n "%"

let relop_to_tree = function
  | Lt -> n "<"
  | Gt -> n ">"
  | Le -> n "<="
  | Ge -> n ">="

[@@@warning "-8"]

let functype_to_tree = function VoidType -> n "Void" | IntType -> n "Int"

let rec ast_to_tree = function
  | Stmt stmt -> ast_to_tree stmt
  | Exp exp -> ast_to_tree exp
  | Number number -> n (string_of_int number)
  | Lval (id, exp_list) -> t "Lval" (n id :: ast_list_to_tree_list exp_list)
  | Call (id, func_r_params) ->
      t "Call" (n id :: ast_list_to_tree_list func_r_params)
  | UnaryOp (unary_op, unary_exp) ->
      t "UnaryOp" [ unaryop_to_tree unary_op; ast_to_tree unary_exp ]
  | Mul (mul_exp, binop, unary_exp) ->
      t "Mul"
        [ ast_to_tree mul_exp; binop_to_tree binop; ast_to_tree unary_exp ]
  | Add (add_exp, mul_exp) ->
      t "Add" (ast_list_to_tree_list [ add_exp; mul_exp ])
  | Sub (add_exp, mul_exp) ->
      t "Sub" (ast_list_to_tree_list [ add_exp; mul_exp ])
  | Rel (rel_exp, relop, add_exp) ->
      t "Rel" [ ast_to_tree rel_exp; relop_to_tree relop; ast_to_tree add_exp ]
  | Eq (eq_exp, rel_exp) -> t "Eq" (ast_list_to_tree_list [ eq_exp; rel_exp ])
  | Neq (eq_exp, rel_exp) -> t "Neq" (ast_list_to_tree_list [ eq_exp; rel_exp ])
  | And (l_and_exp, eq_exp) ->
      t "And" (ast_list_to_tree_list [ l_and_exp; eq_exp ])
  | Or (l_or_exp, l_and_exp) ->
      t "Or" (ast_list_to_tree_list [ l_or_exp; l_and_exp ])
  | DefVar (id, exp) -> t "DefVar" [ n id; ast_to_tree exp ]
  | DefArr (id, dim) -> t "DefArr" (n id :: int_list_to_tree_list dim)
  | IntParam id -> t "IntParam" [ n id ]
  | ArrParam (id, dim) -> t "ArrParam" (n id :: int_list_to_tree_list dim)
  | Decl decl -> t "Int" (ast_list_to_tree_list decl)
  | Assign (lval, exp) -> t "Assign" (ast_list_to_tree_list [ lval; exp ])
  | Block block -> t "Block" (ast_list_to_tree_list block)
  | IfElse (guard, then_, else_) ->
      t "IfElse" (ast_list_to_tree_list [ guard; then_; else_ ])
  | IfThen (guard, then_) -> t "IfThen" (ast_list_to_tree_list [ guard; then_ ])
  | While (guard, stmt) -> t "While" (ast_list_to_tree_list [ guard; stmt ])
  | Break -> n "Break"
  | Continue -> n "Continue"
  | Return exp -> t "Return" [ ast_to_tree exp ]
  | ReturnNone -> t "Return" []
  | FuncDef (func_type, id, func_f_params, block) ->
      t "FuncDef"
        ([ functype_to_tree func_type; n id ]
        @ ast_list_to_tree_list (func_f_params @ block))
  | CompUnit comp_unit -> t "CompUnit" (ast_list_to_tree_list comp_unit)

and ast_list_to_tree_list ast_list = List.map ast_list ~f:ast_to_tree

and int_list_to_tree_list int_list =
  List.map int_list ~f:(fun i -> n (string_of_int i))

[@@@warning "+8"]
