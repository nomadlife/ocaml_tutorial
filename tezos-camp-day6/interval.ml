module type Comparable = sig
  type t
  val compare : t -> t -> int
end 

module Make_interval(Endpoint : Comparable) = struct

    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    (** [create low high] creates a new interval from [low] to
        [high].  If [low > high], then the interval is empty *)
    let create low high =
      if Endpoint.compare low high > 0 then Empty
      else Interval (low,high)

    (** Returns true iff the interval is empty *)
    let is_empty = function
      | Empty -> true
      | Interval _ -> false

    (** [contains t x] returns true iff [x] is contained in the
        interval [t] *)
    let contains t x =
      match t with
      | Empty -> false
      | Interval (l,h) ->
        Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

    (** [intersect t1 t2] returns the intersection of the two input
        intervals *)
    let intersect t1 t2 =
      let min x y = if Endpoint.compare x y <= 0 then x else y in
      let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (l1,h1), Interval (l2,h2) ->
        create (max l1 l2) (min h1 h2)

  end 
	
	module Int_interval =
    Make_interval(struct
      type t = int
      let compare = Pervasives.compare
    end)
		
	module String_interval = Make_interval(String)
	
	module Rev_int_interval =
    Make_interval(struct
      type t = int
      let compare x y = Pervasives.compare y x
    end)
	 
	let interval = Int_interval.create 4 3
	let rev_interval = Rev_int_interval.create 4 3
	(* let _ = Int_interval.contains rev_interval 3 *)
	(* Importantly, Rev_int_interval.t is a different type than Int_interval.t, *)
	(* even though its physical representation is the same. *)

	(* bypassing create is possible *)
	let b1 = Int_interval.is_empty (* going through create *) 
     (Int_interval.create 4 3)
	
	let b2 = Int_interval.is_empty (* going through create *) 
     (Int_interval.Interval (4, 3))
	
	let _ = Printf.printf "%b %b\n" b1 b2
 
	