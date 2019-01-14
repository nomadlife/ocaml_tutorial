{
	(** 고급 OCaml 프로그래밍 Part 2
  2019. 1. 11 - 14 @ Tezos blockchain camp
	언어 실행기 lexer  
  
 *)
 open Parser
 exception Eof
 exception LexicalError
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [("true", TRUE);
                    ("false", FALSE);
                    ("not", NOT);
                    ("if", IF);
                    ("then",THEN);
                    ("else",ELSE);
                    ("let", LET);
                    ("in", IN)
                  ] 
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start =
 parse blank { start lexbuf }
     | number { NUM (int_of_string (Lexing.lexeme lexbuf)) }
     | id { let id = (Lexing.lexeme lexbuf)
            in 
						try Hashtbl.find keyword_tbl id
            with _ -> ID id
           }
     | "+" {PLUS}
     | "-" {MINUS}
     | "*" {STAR}
     | "=" {EQ}
     | "<=" {LTE}
     | "(" {LP}
     | ")" {RP}
     | eof { EOF}
     | _ {raise LexicalError}

