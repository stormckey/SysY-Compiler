open Type

type unary_op = Pos | Neg | Not
type binop = Div | Mul | Rem
type relop = Lt | Gt | Le | Ge

type ast =
  | Number of int
  | BType
  | Decl of ast * ast list
  | CompUnit of ast list
  | DefVar of string * ast
  | DefArr of string * int list
  | FuncDef of value_type * string * ast list * ast list
  | IntParam of ast * string
  | ArrParam of ast * string * int list
  | Block of ast list
  | Stmt of ast
  | Exp of ast
  | Assign of ast * ast
  | If of ast * ast * ast option
  | While of ast * ast
  | Break
  | Continue
  | Return of ast option
  | Lval of string * ast list
  | Call of string * ast list
  | UnaryOp of unary_op * ast
  | Mul of ast * binop * ast
  | Add of ast * ast
  | Sub of ast * ast
  | Rel of ast * relop * ast
  | Eq of ast * ast
  | Neq of ast * ast
  | And of ast * ast
  | Or of ast * ast
