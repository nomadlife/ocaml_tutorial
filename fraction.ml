(* ? *)
module type Fraction = sig
type t
val make : int -> int -> t

val numerator : t -> int
val denominator : t -> int
val toString : t -> toString
val toReal : t -> float 
val add : t -> t -> t
val mul : t -> t -> t
end

let rec gcd (x:int)(y:int) : int =
if x = 0 then y
else if (x<y) then gcd (y-x) x
else gcd y (x-y)
