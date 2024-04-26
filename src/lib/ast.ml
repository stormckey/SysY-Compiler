open Type

type unary_op = Pos | Neg | Not
type binop = Div | Mul | Rem
type relop = Lt | Gt | Le | Ge

type program = decl list
and block = block_item list

and decl =
  | DefVar of decl_var list
  | DefFunc of value_type * string * func_param list * block

and decl_var = DefInt of string * exp | DefArr of string * int list
and func_param = IntParam of string | ArrParam of string * int list

and stmt =
  | Exp of exp
  | Assign of exp * exp
  | IfElse of exp * stmt * stmt
  | IfThen of exp * stmt
  | While of exp * stmt
  | Break
  | Continue
  | Return of exp
  | ReturnNone
  | Block of block

and exp =
  | Lval of string * exp list
  | Call of string * exp list
  | UnaryOp of unary_op * exp
  | Mul of exp * binop * exp
  | Add of exp * exp
  | Sub of exp * exp
  | Rel of exp * relop * exp
  | Eq of exp * exp
  | Neq of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Number of int

and block_item = BlockDecl of decl | BlockStmt of stmt
