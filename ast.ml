open Core

type id = string [@@deriving sexp]
type int_const = int [@@deriving sexp]
type btype = Btype [@@deriving sexp]

type decl = btype * var_def list [@@deriving sexp]
and comp_unit = either_decl_funcdef list [@@deriving sexp]

and var_def = Single of id * init_val | Array of id * int_const list
[@@deriving sexp]

and init_val = exp [@@deriving sexp]
and func_def = func_type * id * func_f_params * block [@@deriving sexp]
and func_type = Void | Int [@@deriving sexp]
and func_f_params = func_f_param list [@@deriving sexp]

and func_f_param = Sin of btype * id | Arr of btype * id * int_const list
[@@deriving sexp]

and block = block_item list [@@deriving sexp]
and block_item = Decl_ of decl | Stmt of stmt [@@deriving sexp]

and stmt =
  | Assign of lval * exp
  | Expr of exp
  | Block of block
  | If of exp * stmt * stmt option
  | While of exp * stmt
  | Break
  | Continue
  | Return of exp option
[@@deriving sexp]

and exp = l_or_exp [@@deriving sexp]
and lval = id * exp list [@@deriving sexp]

and primary_exp = Exp of exp | Lval of lval | Number of number
[@@deriving sexp]

and number = int_const [@@deriving sexp]

and unary_exp =
  | Primary of primary_exp
  | Call of id * func_r_params
  | UnaryOp of unary_op * unary_exp
[@@deriving sexp]

and unary_op = POS | NEG | NOT [@@deriving sexp]
and func_r_params = exp list [@@deriving sexp]

and mul_exp = UnaryExp of unary_exp | MulExp of mul_exp * binop * unary_exp
[@@deriving sexp]

and binop = DIV | MUL | REM [@@deriving sexp]

and add_exp =
  | Mul of mul_exp
  | Add of add_exp * mul_exp
  | Sub of add_exp * mul_exp
[@@deriving sexp]

and rel_exp = ADD of add_exp | REL of rel_exp * relop * add_exp
[@@deriving sexp]

and relop = LT | GT | LE | GE [@@deriving sexp]

and eq_exp = Rel of rel_exp | Eq of eq_exp * rel_exp | Neq of eq_exp * rel_exp
[@@deriving sexp]

and l_and_exp = EQ of eq_exp | AND of l_and_exp * eq_exp [@@deriving sexp]
and l_or_exp = And of l_and_exp | Or of l_or_exp * l_and_exp [@@deriving sexp]
and either_decl_funcdef = Decl of decl | FuncDef of func_def [@@deriving sexp]
