%{
    open Lib.Ast
%}

%token INT
%token SEMICOLON
%token COMMA
%token ASSIGN
%token LBRACK
%token RBRACK
%token <int> INT_CONST
%token <string> ID
%token LPARE
%token RPARE
%token VOID
%token LBRACE
%token RBRACE
%token IF
%token ELSE
%token WHILE
%token BREAK
%token CONTINUE
%token RETURN
%token NOT
%token MUL
%token DIV
%token REM
%token ADD
%token SUB
%token LT
%token GT
%token LE
%token GE
%token EQ
%token NEQ
%token AND
%token OR
%token EOF

%nonassoc THEN
%nonassoc ELSE

%start <program> source

%%

source:
  p = decl_list; EOF { p }

decl_list:
| (* empty *) { [] }
| d = decl_var; c = decl_list { DefVar d :: c }
| f = decl_func; c = decl_list { f :: c}

decl_var:
  INT; v = var_list; SEMICOLON {v}

var_list:
  l = separated_list(COMMA, var) {l}

var:
| i = ID; ASSIGN; init = exp {DefInt (i, init)}
| i = ID; dim = dimensions_list {DefArr
 (i, dim)} 

decl_func:
| INT; i = ID; LPARE; p = func_f_params; RPARE; b =  block {DefFunc(IntType, i, p, b)}
| VOID; i = ID; LPARE; p = func_f_params; RPARE; b =  block {DefFunc(VoidType, i, p, b)}


func_f_params:
  l = separated_list(COMMA, func_f_param) {l}

func_f_param:
| INT; i = ID; {IntParam i}
| INT; i = ID; LBRACK; RBRACK; l = dimensions_list{ArrParam (i, l)}

dimensions_list:
  l = list(bracket_int) {l}

bracket_int:
 LBRACK; i = INT_CONST; RBRACK;{ i}

block:
| LBRACE; l = block_item_list; RBRACE; {l}

block_item_list:
| (* empty *) {[ ]}
| d = decl_var; l = block_item_list {BlockDecl( DefVar d) :: l}
| s = stmt; l = block_item_list { BlockStmt s :: l}

stmt:
| l = lval; ASSIGN; e = exp; SEMICOLON {Assign (l, e)}
| e = exp; SEMICOLON { Exp e}
| b = block {Block b}
| IF; LPARE; e = exp; RPARE; s1 =stmt; ELSE; s2 =stmt {IfElse (e, s1, s2)} 
| IF; LPARE; e = exp; RPARE; s =stmt; {IfThen (e, s)}  %prec THEN
| WHILE; LPARE; e = exp; RPARE; s = stmt {While (e, s)}
| BREAK; SEMICOLON {Break}
| CONTINUE; SEMICOLON {Continue}
| RETURN; e = exp; SEMICOLON; {Return e}
| RETURN; SEMICOLON {ReturnNone}

exp:
 l = l_or_exp {l}

exp_list:
| (* empty *) {[]}
| LBRACK; e = exp; RBRACK; l = exp_list {e :: l}

primary_exp:
| LPARE; e = exp; RPARE; {e}
| i = ID; l = exp_list {Lval (i, l)}
| n = INT_CONST {Number n}

lval:
| i = ID; l = exp_list {Lval (i,l)}


unary_exp:
| p = primary_exp {p}
| i = ID; LPARE; f = func_r_params; RPARE; {Call (i, f)}
| u = unary_op; ue = unary_exp; {UnaryOp (u, ue)}
 
unary_op:
| ADD {Pos}
| SUB {Neg}
| NOT {Not}

func_r_params:
 l = separated_list(COMMA, exp) {l}
 
mul_exp:
| u = unary_exp {u}
| m = mul_exp; b = binop; u = unary_exp {Mul (m, b, u)}

binop:
| DIV {Div}
| MUL {Mul}
| REM {Rem}

add_exp:
| m = mul_exp {m}
| a = add_exp; ADD; m = mul_exp {Add (a, m)}
| a = add_exp; SUB; m = mul_exp {Sub (a, m)}

rel_exp:
| a = add_exp {a}
| r = rel_exp; op = relop; a = add_exp {Rel (r, op, a)}

relop:
| LT {Lt}
| GT {Gt}
| LE {Le}
| GE {Ge}

eq_exp:
| r = rel_exp {r}
| e = eq_exp; EQ; r = rel_exp {Eq (e, r)}
| e = eq_exp; NEQ; r = rel_exp {Neq (e, r)}

l_and_exp:
| e = eq_exp {e}
| l = l_and_exp; AND; e = eq_exp {And (l,e)}

l_or_exp:
| a = l_and_exp {a}
| o = l_or_exp; OR; a = l_and_exp {Or (o, a)}
