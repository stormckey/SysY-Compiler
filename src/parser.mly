%{
    open Ast
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

%start <ast> source

%%

source:
  p = comp_unit; EOF { p }

comp_unit:
l = comp_unit_inside {CompUnit l}

comp_unit_inside:
| (* empty *) { [] }
| d = decl; c = comp_unit_inside { d :: c }
| f = funcdef; c = comp_unit_inside { f :: c}

decl:
  INT; v = vardef_list; SEMICOLON {Decl v}

vardef_list:
  l = separated_list(COMMA, vardef) {l}

vardef:
| i = id; ASSIGN; init = init_val {DefVar (i, init)}
| i = id; dim = dimensions_list {DefArr
 (i, dim)} 

init_val:
  e = exp {e}

funcdef:
| INT; i = id; LPARE; p = func_f_params; RPARE; b =  block {FuncDef (IntType, i, p, b)}
| VOID; i = id; LPARE; p = func_f_params; RPARE; b =  block {FuncDef (VoidType, i, p, b)}


func_f_params:
  l = separated_list(COMMA, func_f_param) {l}

func_f_param:
| INT; i = id; {IntParam i}
| INT; i = id; LBRACK; RBRACK; l = dimensions_list{ArrParam (i, l)}

dimensions_list:
  l = list(bracket_int) {l}

bracket_int:
 LBRACK; i = INT_CONST; RBRACK;{ i}

block:
| LBRACE; l = block_list; RBRACE; {Block l}

block_list:
| (* empty *) {[ ]}
| d = decl; l = block_list { d :: l}
| s = stmt; l = block_list { (Stmt s) :: l}

stmt:
| l = lval; ASSIGN; e = exp; SEMICOLON {Assign (l, e)}
| e = exp; SEMICOLON {e}
| b = block {b}
| IF; LPARE; e = exp; RPARE; s1 =stmt; ELSE; s2 =stmt {IfElse (e, s1, s2)} 
| IF; LPARE; e = exp; RPARE; s =stmt; {IfThen (e, s)}  %prec THEN
| WHILE; LPARE; e = exp; RPARE; s = stmt {While (e, s)}
| BREAK; SEMICOLON {Break}
| CONTINUE; SEMICOLON {Continue}
| RETURN; e = exp; SEMICOLON; {Return e}
| RETURN; SEMICOLON {ReturnNone}

exp:
 l = l_or_exp {Exp l}

lval:
 i = id; l = exp_list {Lval (i, l)}
 
exp_list:
| (* empty *) {[]}
| LBRACK; e = exp; RBRACK; l = exp_list {e :: l}

primary_exp:
| LPARE; e = exp; RPARE; {e}
| l = lval {l}
| n = number {Number n}

number:
 n = INT_CONST { n }
 
unary_exp:
| p = primary_exp {p}
| i = id; LPARE; f = func_r_params; RPARE; {Call (i, f)}
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

id:
    s = ID {s}

