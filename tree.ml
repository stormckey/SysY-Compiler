open Ast
open Core

let tr = PrintBox.tree
let t = PrintBox.text
let s str = tr (t str) []

let rec tree_of_lval (id, exp_list) =
  tr (t "Lval") (s id :: List.map exp_list ~f:tree_of_exp)

and tree_of_primary_exp = function
  | Number number -> s (string_of_int number)
  | Exp exp -> tr (t "Exp") [ tree_of_exp exp ]
  | Lval lval -> tree_of_lval lval

and tree_of_unary_op = function Pos -> s "+" | Neg -> s "-" | Not -> s "!"

and tree_of_unary_exp = function
  | UnaryPrimary primary_exp ->
      tr (t "UnaryPrimary") [ tree_of_primary_exp primary_exp ]
  | Call (id, _) -> tr (t "Call") [ s id ]
  | UnaryOp (unary_op, unary_exp) ->
      tr (t "UnaryOp")
        [ tree_of_unary_op unary_op; tree_of_unary_exp unary_exp ]

and tree_of_binop = function Div -> s "/" | Mul -> s "*" | Rem -> s "%"

and tree_of_mul_exp = function
  | MulUnary unary_exp -> tr (t "MulUnary") [ tree_of_unary_exp unary_exp ]
  | MulMul (mul_exp, binop, unary_exp) ->
      tr (t "MulMul")
        [
          tree_of_mul_exp mul_exp;
          tree_of_binop binop;
          tree_of_unary_exp unary_exp;
        ]

and tree_of_add_exp = function
  | AddMul mul_exp -> tr (t "AndMul") [ tree_of_mul_exp mul_exp ]
  | AddAdd (add_exp, mul_exp) ->
      tr (t "AddAdd") [ tree_of_add_exp add_exp; tree_of_mul_exp mul_exp ]
  | AddSub (add_exp, mul_exp) ->
      tr (t "AddSub") [ tree_of_add_exp add_exp; tree_of_mul_exp mul_exp ]

and tree_of_relop = function
  | Lt -> s "<"
  | Gt -> s ">"
  | Le -> s "<="
  | Ge -> s ".="

and tree_of_rel_exp = function
  | RelAdd add_exp -> tr (t "RelAdd") [ tree_of_add_exp add_exp ]
  | RelRel (rel_exp, relop, add_exp) ->
      tr (t "RelRel")
        [
          tree_of_rel_exp rel_exp; tree_of_relop relop; tree_of_add_exp add_exp;
        ]

and tree_of_eq_exp = function
  | EqRel rel_exp -> tr (t "EqRel") [ tree_of_rel_exp rel_exp ]
  | EqEq (eq_exp, rel_exp) ->
      tr (t "EqEq") [ tree_of_eq_exp eq_exp; tree_of_rel_exp rel_exp ]
  | EqNeq (eq_exp, rel_exp) ->
      tr (t "EqNeq") [ tree_of_eq_exp eq_exp; tree_of_rel_exp rel_exp ]

and tree_of_l_and_exp = function
  | AndEq eq_exp -> tr (t "AndEq") [ tree_of_eq_exp eq_exp ]
  | AndAnd (l_and_exp, eq_exp) ->
      tr (t "AndAnd") [ tree_of_l_and_exp l_and_exp; tree_of_eq_exp eq_exp ]

and tree_of_exp = function
  | OrAnd l_and_exp -> tr (t "OrAnd") [ tree_of_l_and_exp l_and_exp ]
  | OrOr (l_or_exp, l_and_exp) ->
      tr (t "OrOr") [ tree_of_exp l_or_exp; tree_of_l_and_exp l_and_exp ]

let tree_of_dim l = tr (t "Dim") (List.map l ~f:(fun i -> s (string_of_int i)))

let tree_of_var_def = function
  | DefVar (id, exp) -> tr (t "DefVar") [ s id; tree_of_exp exp ]
  | DefArr (id, dim) -> tr (t "DefArr") [ s id; tree_of_dim dim ]

let tree_of_func_type = function Void -> s "Void" | Int -> s "Int"

let tree_of_func_f_param = function
  | IntParam (_, id) -> tr (t "IntParam") [ s id ]
  | ArrParam (_, id, dim) -> tr (t "ArrParam") [ s id; tree_of_dim dim ]

let tree_of_func_f_params l =
  tr (t "FuncFParams") (List.map l ~f:tree_of_func_f_param)

let tree_of_decl decl = tr (t "Int") (List.map decl ~f:tree_of_var_def)

let rec tree_of_stmt = function
  | Assign (lval, exp) -> tr (t "Assign") [ tree_of_lval lval; tree_of_exp exp ]
  | Expr exp -> tr (t "Expr") [ tree_of_exp exp ]
  | Block block -> tree_of_block block
  | If (guard, then_, else_) -> (
      match else_ with
      | None -> tr (t "If") [ tree_of_exp guard; tree_of_stmt then_ ]
      | Some else_ ->
          tr (t "If")
            [ tree_of_exp guard; tree_of_stmt then_; tree_of_stmt else_ ])
  | While (guard, stmt) ->
      tr (t "While") [ tree_of_exp guard; tree_of_stmt stmt ]
  | Break -> s "Break"
  | Continue -> s "Continue"
  | Return exp -> (
      match exp with
      | None -> tr (t "Return") []
      | Some exp -> tr (t "Return") [ tree_of_exp exp ])

and tree_of_block_item = function
  | DeclLocal (_, decl) -> tr (t "DeclLocal") [ tree_of_decl decl ]
  | Stmt stmt -> tree_of_stmt stmt

and tree_of_block b = tr (t "Block") (List.map b ~f:tree_of_block_item)

let tree_of_either_decl_funcdef = function
  | DeclGlobal (_, decl) -> tr (t "DeclGlobal") [ tree_of_decl decl ]
  | FuncDef (func_type, id, func_f_params, block) ->
      tr (t "FuncDef")
        [
          tree_of_func_type func_type;
          s id;
          tree_of_func_f_params func_f_params;
          tree_of_block block;
        ]

let tree_of_comp_unit comp_unit =
  tr (t "CompUnit") (List.map comp_unit ~f:tree_of_either_decl_funcdef)
