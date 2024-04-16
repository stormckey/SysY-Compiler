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

let node_to_string = function
  | Lval _ -> "Lval"
  | Call _ -> "Call"
  | Add _ -> "Add"
  | Sub _ -> "Sub"
  | Eq _ -> "Eq"
  | Neq _ -> "Neq"
  | And _ -> "And"
  | Or _ -> "Or"
  | DefArr _ -> "DefArr"
  | ArrParam _ -> "ArrParam"
  | Decl _ -> "Int"
  | Assign _ -> "Assign"
  | Block _ -> "Block"
  | IfThen _ -> "IfThen"
  | While _ -> "While"
  | Break -> "Break"
  | Continue -> "Continue"
  | ReturnNone -> "Return"
  | CompUnit _ -> "CompUnit"

let rec ast_to_tree = function
  | Stmt fst | Exp fst -> ast_to_tree fst
  | Number number -> n (string_of_int number)
  | IntParam id -> t "IntParam" [ n id ]
  | DefVar (id, exp) -> t "DefVar" [ n id; ast_to_tree exp ]
  | (DefArr (id, dim) | ArrParam (id, dim)) as node ->
      t (node_to_string node) (n id :: int_list_to_tree_list dim)
  | (Lval (id, l) | Call (id, l)) as node ->
      t (node_to_string node) (n id :: ast_list_to_tree_list l)
  | ( Add (fst, snd)
    | Sub (fst, snd)
    | Eq (fst, snd)
    | Neq (fst, snd)
    | And (fst, snd)
    | IfThen (fst, snd)
    | While (fst, snd)
    | Assign (fst, snd)
    | Or (fst, snd) ) as node ->
      t (node_to_string node) (ast_list_to_tree_list [ fst; snd ])
  | UnaryOp (unary_op, unary_exp) ->
      t "UnaryOp" [ unaryop_to_tree unary_op; ast_to_tree unary_exp ]
  | Rel (rel_exp, relop, add_exp) ->
      t "Rel" [ ast_to_tree rel_exp; relop_to_tree relop; ast_to_tree add_exp ]
  | Mul (mul_exp, binop, unary_exp) ->
      t "Mul"
        [ ast_to_tree mul_exp; binop_to_tree binop; ast_to_tree unary_exp ]
  | (Decl l | CompUnit l | Block l) as node ->
      t (node_to_string node) (ast_list_to_tree_list l)
  | IfElse (guard, then_, else_) ->
      t "IfElse" (ast_list_to_tree_list [ guard; then_; else_ ])
  | (Break | Continue | ReturnNone) as node -> n (node_to_string node)
  | Return exp -> t "Return" [ ast_to_tree exp ]
  | FuncDef (func_type, id, func_f_params, block) ->
      t "FuncDef"
        ([ functype_to_tree func_type; n id ]
        @ ast_list_to_tree_list func_f_params
        @ [ ast_to_tree block ])

and ast_list_to_tree_list ast_list = List.map ast_list ~f:ast_to_tree

and int_list_to_tree_list int_list =
  List.map int_list ~f:(fun i -> n (string_of_int i))

[@@@warning "+8"]
