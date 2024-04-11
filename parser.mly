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

%start <comp_unit> source

%%

source:
  p = comp_unit; EOF { p }

comp_unit:
| (* empty *) { [] }
| d = decl; c = comp_unit { (DeclGlobal d) :: c }
| f = funcdef; c = comp_unit {(FuncDef f) :: c}

decl:
  INT; v = vardef_list; SEMICOLON {(Btype, v)}

vardef_list:
  l = separated_list(COMMA, vardef) {l}

vardef:
| i = id; ASSIGN; init = init_val {DefVar (i, init)}
| i = id; dim = dimensions_list {DefArr
 (i, dim)} 

init_val:
  e = exp {e}

funcdef:
| INT; i = id; LPARE; p = func_f_params; RPARE; b =  block {(Int, i, p, b)}
| VOID; i = id; LPARE; p = func_f_params; RPARE; b =  block {(Void, i, p, b)}


func_f_params:
  l = separated_list(COMMA, func_f_param) {l}

func_f_param:
| INT; i = id; {IntParam (Btype,i)}
| INT; i = id; LBRACK; RBRACK; l = dimensions_list{ArrParam (Btype, i, l)}

dimensions_list:
  l = list(bracket_int) {l}

bracket_int:
 LBRACK; i = INT_CONST; RBRACK;{i}

block:
| LBRACE; l = block_list; RBRACE; {l}

block_list:
| (* empty *) {[ ]}
| d = decl; l = block_list { (DeclLocal d) :: l}
| s = stmt; l = block_list { (Stmt s) :: l}

stmt:
| l = lval; ASSIGN; e = exp; SEMICOLON {Assign (l, e)}
| e = exp; SEMICOLON {Expr e}
| b = block {Block b}
| IF; LPARE; e = exp; RPARE; s1 =stmt; ELSE; s2 = stmt {If (e, s1, Some s2)} 
| IF; LPARE; e = exp; RPARE; s =stmt; {If (e, s, None)}  %prec THEN
| WHILE; LPARE; e = exp; RPARE; s = stmt {While (e, s)}
| BREAK; SEMICOLON {Break}
| CONTINUE; SEMICOLON {Continue}
| RETURN; e = exp; SEMICOLON; {Return (Some e)}
| RETURN; SEMICOLON {Return None}

exp:
 l = l_or_exp {l}

lval:
 i = id; l = exp_list {(i, l)}
 
exp_list:
| (* empty *) {[]}
| LBRACK; e = exp; RBRACK; l = exp_list {e :: l}

primary_exp:
| LPARE; e = exp; RPARE; {Exp e}
| l = lval {Lval l}
| n = number {Number n}

number:
 n = INT_CONST { n }
 
unary_exp:
| p = primary_exp {UnaryPrimary p}
| i = id; LPARE; f = func_r_params; RPARE; {Call (i, f)}
| u = unary_op; ue = unary_exp; {UnaryOp (u, ue)}
 
unary_op:
| ADD {Pos}
| SUB {Neg}
| NOT {Not}

func_r_params:
 l = separated_list(COMMA, exp) {l}
 
mul_exp:
| u = unary_exp {MulUnary u}
| m = mul_exp; b = binop; u = unary_exp {MulMul (m, b, u)}

binop:
| DIV {Div}
| MUL {Mul}
| REM {Rem}

add_exp:
| m = mul_exp {AddMul m}
| a = add_exp; ADD; m = mul_exp {AddAdd (a, m)}
| a = add_exp; SUB; m = mul_exp {AddSub (a, m)}

rel_exp:
| a = add_exp {RelAdd a}
| r = rel_exp; op = relop; a = add_exp {RelRel (r, op, a)}

relop:
| LT {Lt}
| GT {Gt}
| LE {Le}
| GE {Ge}

eq_exp:
| r = rel_exp {EqRel r}
| e = eq_exp; EQ; r = rel_exp {EqEq (e, r)}
| e = eq_exp; NEQ; r = rel_exp {EqNeq (e, r)}

l_and_exp:
| e = eq_exp {AndEq e}
| l = l_and_exp; AND; e = eq_exp {AndAnd (l,e)}

l_or_exp:
| a = l_and_exp {OrAnd a}
| o = l_or_exp; OR; a = l_and_exp {OrOr (o, a)}

id:
    s = ID {s}





