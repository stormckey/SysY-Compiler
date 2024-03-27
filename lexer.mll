{
    open Lexing
    open Parser
    let p x = print_endline x
}

let digit = ['0'-'9']
let int = digit digit*
let white = [' ' '\t']
let newline = '\n' 
let comment1 = "//" [^ '\n']*
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
| white {read lexbuf}
| newline {new_line lexbuf; read lexbuf}
| comment1 {read lexbuf}
| "/*" {comment lexbuf}
| int {INT_CONST (int_of_string (Lexing.lexeme lexbuf))}
| "int" {INT}
| "," {COMMA}
| ";" {SEMICOLON}
| "=" {ASSIGN}
| "(" {LPARE}
| ")" {RPARE}
| "{" {LBRACE}
| "}" {RBRACE}
| "[" {LBRACK}
| "]" {RBRACK}
| "void" {VOID}
| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "break" {BREAK}
| "continue" {CONTINUE}
| "return" {RETURN}
| id {ID (Lexing.lexeme lexbuf)}
| '!' {NOT_}
| '*' {MUL_}
| '/' {DIV_}
| '%' {REM_}
| '+' {ADD_}
| '-' {SUB_}
| '<' {LT_}
| '>' {GT_}
| ">=" {GE_}
| "<=" {LE_}
| "==" {EQ_}
| "!=" {NEQ_}
| "&&" {AND_}
| "||" {OR_}
| eof {EOF}
| _ {failwith "unknown string to lex"}

and comment = parse
| "*/" {read lexbuf}
| _ {comment lexbuf}