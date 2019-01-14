module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator   : t -> int
  val denominator : t -> int
  val toString    : t -> string
  val toReal      : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module FractionImpl : Fraction = struct
  (* A fraction is a rational number p/q, where q != 0.*)
  type t = int * int

	(* [gcd x y] is the greatest common divisor of [x] and [y].
	 * requires: [x] and [y] are positive. *)
  let rec gcd (x:int) (y:int) : int =
    if x = 0 then y
    else if (x < y) then gcd (y - x) x
    else gcd y (x - y)
  	
	let norm (a,b) = let gcd = (gcd a b) in (a / gcd , b / gcd) 
		
  (* [make n d] is n/d. Requires d != 0. *)
  let make n d = norm (n, d) 

  let numerator   : t -> int = fun (n, d) -> n
  let denominator : t -> int = fun (n, d) -> d
  let toString    : t -> string = fun (n, d) -> string_of_int n ^ " / " ^ string_of_int d
  let toReal      : t -> float = fun (n, d) -> float_of_int n /. float_of_int d
	 
	let add (a,b) (c,d) = norm (a*d + c*b, b*d)
  let mul (a,b) (c,d) = norm (a*c, b*d)
end

let _ =
	let fraction = 
		FractionImpl.add (FractionImpl.make 2 6) (FractionImpl.make 2 12) 
	in  
	prerr_endline (FractionImpl.toString fraction) 
