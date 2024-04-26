open Core
open Ast
open Type

let t str list = PrintBox.tree (PrintBox.text str) list (*tree*)
let n str = t str [] (*node/leaf*)
let unaryop_to_tree = function Pos -> n "+" | Neg -> n "-" | Not -> n "!"
let binop_to_tree = function Div -> n "/" | Mul -> n "*" | Rem -> n "%"

let relop_to_tree = function
  | Lt -> n "<"
  | Gt -> n ">"
  | Le -> n "<="
  | Ge -> n ">="

let functype_to_tree = function
  | VoidType -> n "Void"
  | IntType -> n "Int"
  | _ -> failwith "return type other than void and int when printing tree"

let rec program_to_tree prog = t "Program" (List.map prog ~f:decl_to_tree)

and decl_to_tree = function
  | DefVar decl_var_list ->
      t "DefVar" (List.map decl_var_list ~f:decl_var_to_tree)
  | DefFunc (func_type, id, param_list, block) ->
      t "DefFunc"
        ([ functype_to_tree func_type; n id ]
        @ List.map param_list ~f:decl_param_to_tree
        @ [ block_to_tree block ])

and decl_param_to_tree = function
  | IntParam id -> t "IntParam" [ n id ]
  | ArrParam (id, num_list) ->
      t "ArrParam" (n id :: List.map num_list ~f:(fun i -> n (string_of_int i)))

and decl_var_to_tree = function
  | DefInt (id, exp) -> t "DefInt" [ n id; exp_to_tree exp ]
  | DefArr (id, num_list) ->
      t "DefArr" (n id :: List.map num_list ~f:(fun i -> n (string_of_int i)))

and block_to_tree block = t "Block" (List.map block ~f:block_item_to_tree)

and block_item_to_tree = function
  | BlockDecl decl -> decl_to_tree decl
  | BlockStmt stmt -> stmt_to_tree stmt

and stmt_to_tree = function
  | Exp exp -> t "Exp" [ exp_to_tree exp ]
  | Assign (lval, exp) -> t "Assign" [ exp_to_tree lval; exp_to_tree exp ]
  | IfElse (exp, stmt1, stmt2) ->
      t "IfElse" [ exp_to_tree exp; stmt_to_tree stmt1; stmt_to_tree stmt2 ]
  | IfThen (exp, stmt) -> t "IfThen" [ exp_to_tree exp; stmt_to_tree stmt ]
  | While (exp, stmt) -> t "While" [ exp_to_tree exp; stmt_to_tree stmt ]
  | Break -> n "Break"
  | Continue -> n "Continue"
  | Return exp -> t "Return" [ exp_to_tree exp ]
  | ReturnNone -> n "Return"
  | Block block -> t "Block" (List.map block ~f:block_item_to_tree)

and exp_to_tree = function
  | Lval (id, l) -> t "Lval" (n id :: List.map l ~f:exp_to_tree)
  | Call (id, l) -> t "Call" (n id :: List.map l ~f:exp_to_tree)
  | UnaryOp (unary_op, exp) ->
      t "UnaryOp" [ unaryop_to_tree unary_op; exp_to_tree exp ]
  | Mul (exp1, binop, exp2) ->
      t "Mul" [ exp_to_tree exp1; binop_to_tree binop; exp_to_tree exp2 ]
  | Add (exp1, exp2) -> t "Add" [ exp_to_tree exp1; exp_to_tree exp2 ]
  | Sub (exp1, exp2) -> t "Sub" [ exp_to_tree exp1; exp_to_tree exp2 ]
  | Rel (exp1, relop, exp2) ->
      t "Rel" [ exp_to_tree exp1; relop_to_tree relop; exp_to_tree exp2 ]
  | Eq (exp1, exp2) -> t "Eq" [ exp_to_tree exp1; exp_to_tree exp2 ]
  | Neq (exp1, exp2) -> t "Neq" [ exp_to_tree exp1; exp_to_tree exp2 ]
  | And (exp1, exp2) -> t "And" [ exp_to_tree exp1; exp_to_tree exp2 ]
  | Or (exp1, exp2) -> t "Or" [ exp_to_tree exp1; exp_to_tree exp2 ]
  | Number num -> n (string_of_int num)
