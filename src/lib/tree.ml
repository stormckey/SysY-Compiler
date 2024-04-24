open Core
open Ast
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
  | Number num -> n (string_of_int num)
  | IntParam id -> t "IntParam" [ n id ]
  | Stmt fst | Exp fst -> ast_to_tree fst
  | Return fst -> t "Return" [ ast_to_tree fst ]
  | DefVar (id, fst) -> t "DefVar" [ n id; ast_to_tree fst ]
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
  | IfElse (fst, snd, trd) ->
      t "IfElse" (ast_list_to_tree_list [ fst; snd; trd ])
  | UnaryOp (unary_op, fst) ->
      t "UnaryOp" [ unaryop_to_tree unary_op; ast_to_tree fst ]
  | Rel (fst, relop, snd) ->
      t "Rel" [ ast_to_tree fst; relop_to_tree relop; ast_to_tree snd ]
  | Mul (fst, binop, snd) ->
      t "Mul" [ ast_to_tree fst; binop_to_tree binop; ast_to_tree snd ]
  | (DefArr (id, num_list) | ArrParam (id, num_list)) as node ->
      t (node_to_string node) (n id :: int_list_to_tree_list num_list)
  | (Lval (id, l) | Call (id, l)) as node ->
      t (node_to_string node) (n id :: ast_list_to_tree_list l)
  | (Decl l | CompUnit l | Block l) as node ->
      t (node_to_string node) (ast_list_to_tree_list l)
  | (Break | Continue | ReturnNone) as node -> n (node_to_string node)
  | FuncDef (func_type, id, l, fst) ->
      t "FuncDef"
        ([ functype_to_tree func_type; n id ]
        @ ast_list_to_tree_list l
        @ [ ast_to_tree fst ])

and ast_list_to_tree_list ast_list = List.map ast_list ~f:ast_to_tree

and int_list_to_tree_list int_list =
  List.map int_list ~f:(fun i -> n (string_of_int i))

[@@@warning "+8"]
