open Core

type id = string
type int_const = int
type btype = Btype

type decl = btype * var_def list
and comp_unit = either_decl_funcdef list
and var_def = DefVar of id * exp | DefArr of id * int_const list
and func_def = func_type * id * func_f_params * block
and func_type = Void | Int
and func_f_params = func_f_param list

and func_f_param =
  | IntParam of btype * id
  | ArrParam of btype * id * int_const list

and block = block_item list
and block_item = DeclLocal of decl | Stmt of stmt

and stmt =
  | Assign of lval * exp
  | Expr of exp
  | Block of block
  | If of exp * stmt * stmt option
  | While of exp * stmt
  | Break
  | Continue
  | Return of exp option

and exp = l_or_exp
and lval = id * exp list
and primary_exp = Exp of exp | Lval of lval | Number of number
and number = int_const

and unary_exp =
  | UnaryPrimary of primary_exp
  | Call of id * func_r_params
  | UnaryOp of unaryop * unary_exp

and unaryop = Pos | Neg | Not
and func_r_params = exp list
and mul_exp = MulUnary of unary_exp | MulMul of mul_exp * binop * unary_exp
and binop = Div | Mul | Rem

and add_exp =
  | AddMul of mul_exp
  | AddAdd of add_exp * mul_exp
  | AddSub of add_exp * mul_exp

and rel_exp = RelAdd of add_exp | RelRel of rel_exp * relop * add_exp
and relop = Lt | Gt | Le | Ge

and eq_exp =
  | EqRel of rel_exp
  | EqEq of eq_exp * rel_exp
  | EqNeq of eq_exp * rel_exp

and l_and_exp = AndEq of eq_exp | AndAnd of l_and_exp * eq_exp
and l_or_exp = OrAnd of l_and_exp | OrOr of l_or_exp * l_and_exp
and either_decl_funcdef = DeclGlobal of decl | FuncDef of func_def

type value_type =
  | IntType
  | VoidType
  | ArrayType of int list
  | FuncType of value_type * value_type list
[@@deriving sexp, equal]
