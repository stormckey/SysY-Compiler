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
%token NOT_
%token MUL_
%token DIV_
%token REM_
%token ADD_
%token SUB_
%token LT_
%token GT_
%token LE_
%token GE_
%token EQ_
%token NEQ_
%token AND_
%token OR_
%token EOF

%start <comp_unit> source

%%

source:
  p = comp_unit; EOF { p }

comp_unit:
| (* empty *) { [] }
| d = decl; c = comp_unit { (Decl d) :: c }
| f = funcdef; c = comp_unit {(FuncDef f) :: c}

decl:
  INT; v = vardef_list; SEMICOLON {(Btype, v)}

vardef_list:
  l = separated_list(COMMA, vardef) {l}

vardef:
| i = id; ASSIGN; init = init_val {Single (i, init)}
| i = id; dim = dimensions_list {Array (i, dim)} 

init_val:
  e = exp {e}

funcdef:
| INT; i = id; LPARE; p = func_f_params; RPARE; b =  block {(Int, i, p, b)}
| VOID; i = id; LPARE; p = func_f_params; RPARE; b =  block {(Void, i, p, b)}


func_f_params:
  l = separated_list(COMMA, func_f_param) {l}

func_f_param:
| INT; i = id; {Sin (Btype,i)}
| INT; i = id; LBRACK; RBRACK; l = dimensions_list{Arr (Btype, i, l)}

dimensions_list:
  l = list(bracket_int) {l}

bracket_int:
 LBRACK; i = INT_CONST; RBRACK;{i}

block:
| LBRACE; l = block_list; RBRACE; {l}

block_list:
| (* empty *) {[ ]}
| d = decl; l = block_list { (Decl_ d) :: l}
| s = stmt; l = block_list { (Stmt s) :: l}

stmt:
| l = lval; ASSIGN; e = exp; SEMICOLON {Assign (l, e)}
| e = exp; SEMICOLON {Expr e}
| b = block {Block b}
| IF; LPARE; e = exp; RPARE; s1 =stmt; ELSE; s2 = stmt  {If (e, s1, Some s2)}
| IF; LPARE; e = exp; RPARE; s =stmt; {If (e, s, None)}
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
| p = primary_exp {Primary p}
| i = id; LPARE; f = func_r_params; RPARE; {Call (i, f)}
| u = unary_op; ue = unary_exp; {UnaryOp (u, ue)}
 
unary_op:
| ADD_ {POS}
| SUB_ {NEG}
| NOT_ {NOT}

func_r_params:
 l = separated_list(COMMA, exp) {l}
 
mul_exp:
| u = unary_exp {UnaryExp u}
| m = mul_exp; b = binop; u = unary_exp {MulExp (m, b, u)}

binop:
| DIV_ {DIV}
| MUL_ {MUL}
| REM_ {REM}

add_exp:
| m = mul_exp {Mul m}
| a = add_exp; ADD_; m = mul_exp {Add (a, m)}
| a = add_exp; SUB_; m = mul_exp {Sub (a, m)}

rel_exp:
| a = add_exp {ADD a}
| r = rel_exp; op = relop; a = add_exp {REL (r, op, a)}

relop:
| LT_ {LT}
| GT_ {GT}
| LE_ {LE}
| GE_ {GE}

eq_exp:
| r = rel_exp {Rel r}
| e = eq_exp; EQ_; r = rel_exp {Eq (e, r)}
| e = eq_exp; NEQ_; r = rel_exp {Neq (e, r)}

l_and_exp:
| e = eq_exp {EQ e}
| l = l_and_exp; AND_; e = eq_exp {AND (l,e)}

l_or_exp:
| a = l_and_exp {And a}
| o = l_or_exp; OR_; a = l_and_exp {Or (o, a)}

id:
    s = ID {s}





