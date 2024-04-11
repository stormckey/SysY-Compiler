open Core

type id = string [@@deriving sexp]
type int_const = int [@@deriving sexp]
type btype = Btype [@@deriving sexp]

type decl = btype * var_def list [@@deriving sexp]
and comp_unit = either_decl_funcdef list [@@deriving sexp]

and var_def = DefVar of id * exp | DefArr of id * int_const list
[@@deriving sexp]

and func_def = func_type * id * func_f_params * block [@@deriving sexp]
and func_type = Void | Int [@@deriving sexp]
and func_f_params = func_f_param list [@@deriving sexp]

and func_f_param =
  | IntParam of btype * id
  | ArrParam of btype * id * int_const list
[@@deriving sexp]

and block = block_item list [@@deriving sexp]
and block_item = DeclLocal of decl | Stmt of stmt [@@deriving sexp]

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
  | UnaryPrimary of primary_exp
  | Call of id * func_r_params
  | UnaryOp of unary_op * unary_exp
[@@deriving sexp]

and unary_op = Pos | Neg | Not [@@deriving sexp]
and func_r_params = exp list [@@deriving sexp]

and mul_exp = MulUnary of unary_exp | MulMul of mul_exp * binop * unary_exp
[@@deriving sexp]

and binop = Div | Mul | Rem [@@deriving sexp]

and add_exp =
  | AddMul of mul_exp
  | AddAdd of add_exp * mul_exp
  | AddSub of add_exp * mul_exp
[@@deriving sexp]

and rel_exp = RelAdd of add_exp | RelRel of rel_exp * relop * add_exp
[@@deriving sexp]

and relop = Lt | Gt | Le | Ge [@@deriving sexp]

and eq_exp =
  | EqRel of rel_exp
  | EqEq of eq_exp * rel_exp
  | EqNeq of eq_exp * rel_exp
[@@deriving sexp]

and l_and_exp = AndEq of eq_exp | AndAnd of l_and_exp * eq_exp
[@@deriving sexp]

and l_or_exp = OrAnd of l_and_exp | OrOr of l_or_exp * l_and_exp
[@@deriving sexp]

and either_decl_funcdef = DeclGlobal of decl | FuncDef of func_def
[@@deriving sexp]
