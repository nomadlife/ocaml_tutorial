(* Problem 1 *)
let rec fib : int -> int
=fun n -> n (* TODO *)


(* Problem 2 *)
let rec lst2int : int list -> int
=fun l -> 0 (* TODO *)


(* Problem 3 *)
let rec dropWhile : ('a -> bool) -> 'a list -> 'a list
=fun p l -> l (* TODO *) 


(* Problem 4 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n m -> ZERO (* TODO *)

let rec natmul : nat -> nat -> nat
=fun n m -> ZERO (* TODO *)


(* Problem 5 *)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> 0 (* TODO *)
