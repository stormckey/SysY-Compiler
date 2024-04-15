type func_type = Void | Int
and unary_op = Pos | Neg | Not
and binop = Div | Mul | Rem
and relop = Lt | Gt | Le | Ge

type ast =
  [ `Id of string
  | `IntConst of int
  | `BType
  | `Decl of ast * ast list
  | `CompUnit of ast list
  | `VarDef of ast
  | `FuncDef of ast * ast * ast * ast
  | `FuncType of func_type
  | `FuncFParamList of ast list
  | `FuncFParam of ast
  | `IntParam of ast * ast
  | `ArrParam of ast * ast * ast
  | `Block of ast list
  | `BlockItem of ast
  | `DeclLocal of ast
  | `Stmt of ast
  | `Assign of ast * ast
  | `Expr of ast
  | `If of ast * ast * ast
  | `While of ast * ast * ast
  | `Break
  | `Continue
  | `Return of ast option
  | `Exp of ast
  | `Lval of ast * ast list
  | `Primary of ast
  | `Unary of ast
  | `UnaryPrimary of ast
  | `Call of ast * ast
  | `UnaryOp of unary_op * ast
  | `FuncRParamList of ast list
  | `Mul of ast
  | `MulUnary of ast
  | `MulMul of ast * binop * ast
  | `Add of ast
  | `AddMul of ast
  | `AddAdd of ast * ast
  | `AddSub of ast * ast
  | `Rel of ast
  | `RelAdd of ast
  | `RelRel of ast * relop * ast
  | `Eq of ast
  | `EqRel of ast
  | `EqEq of ast * ast
  | `EqNeq of ast * ast
  | `LAndExp of ast
  | `AndEq of ast
  | `AndAnd of ast * ast
  | `Either of ast
  | `DeclGlobal of ast ]
