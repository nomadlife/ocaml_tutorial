(* Problem 1 *)
let rec fib : int -> int
=fun n ->
match n with
| 0 -> 0
| 1 -> 1
| _ -> fib n-1 + fib n-2 ;;


(* Problem 2 *)
let rec lst2int 
  = fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::(lst2int tl);;
  

(* Problem 3 *)

let rec dropWhile : ('a -> bool) -> 'a list -> 'a list
=fun f l ->
match l with
| [] -> []
| hd::tl -> if f hd then dropWhile f tl else hd::tl;;


(* Problem 4 *)



(* Problem 5 *)
type exp = exp
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
  = fun e -> 
match e with 
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) -> 
  if e1 = e2 then (calculator e2) 
  else (calculator e3) + SIGMA (e1+1) e2 e3
