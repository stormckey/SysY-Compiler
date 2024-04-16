open Type

type unary_op = Pos | Neg | Not
type binop = Div | Mul | Rem
type relop = Lt | Gt | Le | Ge

type ast =
  | Number of int
  | Decl of ast list (* DefVar | DefArr list *)
  | CompUnit of ast list (* FuncDef | Decl list *)
  | DefVar of string * ast (* IntParam | ArrParam *)
  | DefArr of string * int list
  | FuncDef of
      value_type
      * string
      * ast list
      * ast (* IntParam | ArrParam list * block *)
  | IntParam of string
  | ArrParam of string * int list
  | Block of ast list (* Decl | Stmt list*)
  | Stmt of
      ast (* Assign | Exp | Block | If | While | Break | Continue | Return*)
  | Exp of ast (* Or | And | Eq | Neq | Add | Sub | Mul | Unary | Call | Lval *)
  | Assign of ast * ast (* Lval * Exp *)
  | IfElse of ast * ast * ast (* Exp * Stmt * Stmt *)
  | IfThen of ast * ast (* Exp * Stmt *)
  | While of ast * ast (* Exp * Stmt *)
  | Break
  | Continue
  | Return of ast (* Exp *)
  | ReturnNone
  | Lval of string * ast list (* Exp list*)
  | Call of string * ast list (* Exp list *)
  | UnaryOp of unary_op * ast (* Exp *)
  | Mul of ast * binop * ast (* Mul * binop * Unary *)
  | Add of ast * ast (* Add * Mul *)
  | Sub of ast * ast (* Add * Mul*)
  | Rel of ast * relop * ast (* Rel * Add *)
  | Eq of ast * ast (* Eq * Rel *)
  | Neq of ast * ast (* Eq * Rel*)
  | And of ast * ast (* And * Eq *)
  | Or of ast * ast (* Or * And *)
