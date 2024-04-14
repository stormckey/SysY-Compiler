open Ast
open Core

let t str list = PrintBox.tree (PrintBox.text str) list
let n str = t str []

let rec tree_of_lval (id, exp_list) =
  t "Lval" (n id :: List.map exp_list ~f:tree_of_exp)

and tree_of_primary_exp = function
  | Number number -> n (string_of_int number)
  | Exp exp -> t "Exp" [ tree_of_exp exp ]
  | Lval lval -> tree_of_lval lval

and tree_of_unary_op = function Pos -> n "+" | Neg -> n "-" | Not -> n "!"
and tree_of_func_r_params l = List.map l ~f:tree_of_exp

and tree_of_unary_exp = function
  | UnaryPrimary primary_exp -> tree_of_primary_exp primary_exp
  | Call (id, func_r_params) ->
      t "Call" (n id :: tree_of_func_r_params func_r_params)
  | UnaryOp (unary_op, unary_exp) ->
      t "UnaryOp" [ tree_of_unary_op unary_op; tree_of_unary_exp unary_exp ]

and tree_of_binop = function Div -> n "/" | Mul -> n "*" | Rem -> n "%"

and tree_of_mul_exp = function
  | MulUnary unary_exp -> tree_of_unary_exp unary_exp
  | MulMul (mul_exp, binop, unary_exp) ->
      t "Mul"
        [
          tree_of_mul_exp mul_exp;
          tree_of_binop binop;
          tree_of_unary_exp unary_exp;
        ]

and tree_of_add_exp = function
  | AddMul mul_exp -> tree_of_mul_exp mul_exp
  | AddAdd (add_exp, mul_exp) ->
      t "Add" [ tree_of_add_exp add_exp; tree_of_mul_exp mul_exp ]
  | AddSub (add_exp, mul_exp) ->
      t "Sub" [ tree_of_add_exp add_exp; tree_of_mul_exp mul_exp ]

and tree_of_relop = function
  | Lt -> n "<"
  | Gt -> n ">"
  | Le -> n "<="
  | Ge -> n ".="

and tree_of_rel_exp = function
  | RelAdd add_exp -> tree_of_add_exp add_exp
  | RelRel (rel_exp, relop, add_exp) ->
      t "Rel"
        [
          tree_of_rel_exp rel_exp; tree_of_relop relop; tree_of_add_exp add_exp;
        ]

and tree_of_eq_exp = function
  | EqRel rel_exp -> tree_of_rel_exp rel_exp
  | EqEq (eq_exp, rel_exp) ->
      t "Eq" [ tree_of_eq_exp eq_exp; tree_of_rel_exp rel_exp ]
  | EqNeq (eq_exp, rel_exp) ->
      t "Neq" [ tree_of_eq_exp eq_exp; tree_of_rel_exp rel_exp ]

and tree_of_l_and_exp = function
  | AndEq eq_exp -> tree_of_eq_exp eq_exp
  | AndAnd (l_and_exp, eq_exp) ->
      t "And" [ tree_of_l_and_exp l_and_exp; tree_of_eq_exp eq_exp ]

and tree_of_exp = function
  | OrAnd l_and_exp -> tree_of_l_and_exp l_and_exp
  | OrOr (l_or_exp, l_and_exp) ->
      t "Or" [ tree_of_exp l_or_exp; tree_of_l_and_exp l_and_exp ]

let tree_of_dim l = t "Dim" (List.map l ~f:(fun i -> n (string_of_int i)))

let tree_of_var_def = function
  | DefVar (id, exp) -> t "DefVar" [ n id; tree_of_exp exp ]
  | DefArr (id, dim) -> t "DefArr" [ n id; tree_of_dim dim ]

let tree_of_func_type = function Void -> n "Void" | Int -> n "Int"

let tree_of_func_f_param = function
  | IntParam (_, id) -> t "IntParam" [ n id ]
  | ArrParam (_, id, dim) -> t "ArrParam" [ n id; tree_of_dim dim ]

let tree_of_func_f_params l =
  t "FuncFParams" (List.map l ~f:tree_of_func_f_param)

let tree_of_decl decl = t "Int" (List.map decl ~f:tree_of_var_def)

let rec tree_of_stmt = function
  | Assign (lval, exp) -> t "Assign" [ tree_of_lval lval; tree_of_exp exp ]
  | Expr exp -> t "Expr" [ tree_of_exp exp ]
  | Block block -> tree_of_block block
  | If (guard, then_, else_) -> (
      match else_ with
      | None -> t "If" [ tree_of_exp guard; tree_of_stmt then_ ]
      | Some else_ ->
          t "If" [ tree_of_exp guard; tree_of_stmt then_; tree_of_stmt else_ ])
  | While (guard, stmt) -> t "While" [ tree_of_exp guard; tree_of_stmt stmt ]
  | Break -> n "Break"
  | Continue -> n "Continue"
  | Return exp -> (
      match exp with
      | None -> t "Return" []
      | Some exp -> t "Return" [ tree_of_exp exp ])

and tree_of_block_item = function
  | DeclLocal (_, decl) -> t "DeclLocal" [ tree_of_decl decl ]
  | Stmt stmt -> tree_of_stmt stmt

and tree_of_block b = t "Block" (List.map b ~f:tree_of_block_item)

let tree_of_either_decl_funcdef = function
  | DeclGlobal (_, decl) -> t "DeclGlobal" [ tree_of_decl decl ]
  | FuncDef (func_type, id, func_f_params, block) ->
      t "FuncDef"
        [
          tree_of_func_type func_type;
          n id;
          tree_of_func_f_params func_f_params;
          tree_of_block block;
        ]

let tree_of_comp_unit comp_unit =
  t "CompUnit" (List.map comp_unit ~f:tree_of_either_decl_funcdef)
