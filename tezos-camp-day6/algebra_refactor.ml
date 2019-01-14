module type Ring = sig
  type t
  val zero  : t
  val one   : t
  val (+)   : t -> t -> t
  val (~-)  : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val of_int : int -> t
end

module type Field = sig
  include Ring
	val (/) : t -> t -> t
end 

module IntRingImpl = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) = (+)
  let (~-) = (~-)
  let ( * ) = ( * )
  let to_string = string_of_int
  let of_int n = n
end

module IntRing : Ring = IntRingImpl

module IntField : Field = struct
	include IntRingImpl
  let (/) = (/)
end

module FloatRingImpl = struct
  type t = float
  let zero = 0.
  let one = 1.
  let (+) = (+.)
  let (~-) = (~-.)
  let ( * ) = ( *. )
  let to_string = string_of_float
  let of_int n = float_of_int n
end

module FloatRing : Ring = FloatRingImpl

module FloatField : Field = struct
	include FloatRingImpl
  let (/) = (/.)
end


module MakeRational(F: Field) = struct
	type t = F.t * F.t
	open F
	let zero = (zero, zero)
	let one = (one, one)
	let (+) (a,b) (c,d) = (a*d + c*b, b*d)
  let (~-) (a,b) = (-a,b)
  let (/) (a,b) (c,d) = a*d, b*c
  let ( * ) (a,b) (c,d) = (a*c, b*d)
	let to_string (a,b) = to_string a ^ "/" ^ to_string b
  let of_int n = ((of_int n), F.one)
end 
	
module IntRational : Field = MakeRational(IntField)
module FloatRational : Field = MakeRational(FloatField)

let _ = FloatField.(of_int 9 + of_int 3 / of_int 4 |> to_string |> print_endline)
let _ = IntRational.(
    let half = one / (one+one) in 
    let quarter = half*half in 
    let three = one+one+one in 
    let nine = three*three in 
    print_endline (to_string (nine + (three*quarter)))
  )