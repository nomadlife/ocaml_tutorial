%{
	(** 고급 OCaml 프로그래밍 Part 2
  2019. 1. 11 - 14 @ Tezos blockchain camp
	언어 실행기 parser  
  
 *)
	open Interpreter.Calc_v8
%}
%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token PLUS MINUS STAR EQ LTE NOT IF THEN ELSE
%token LET IN
%token LP RP
%token EOF

%nonassoc IN
%nonassoc THEN
%nonassoc ELSE
%left EQ LTE  
%left ADD SUB
%left MUL
%right NOT

%start program
%type <Interpreter.Calc_v8.exp> program

%%

program:
   expr EOF { $1 }
    ;

expr: 
      LP expr RP { $2 }
    | NUM { INT ($1) }
    | TRUE { BOOL true }
    | FALSE { BOOL false }
    | ID { VAR ($1) }
    | ID LP expr RP { CALL ($1, $3) }
    | expr PLUS expr { ADD ($1, $3) }
    | expr MINUS expr  { SUB ($1,$3) }
    | expr STAR expr { MUL ($1,$3) }
    | expr EQ expr { EQUAL ($1,$3) }
    | expr LTE expr { LESS ($1,$3) }
    | NOT expr { NOT ($2) }
    | IF expr THEN expr ELSE expr { IF ($2, $4, $6) }
    | LET ID EQ expr IN expr { LET ($2, $4, $6)  }
    | LET ID LP ID RP EQ expr IN expr { LETF ($2, $4, $7, $9)  } 
    ;
%%
