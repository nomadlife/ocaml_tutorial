module type Q = sig
	type 'a queue
	val empty: 'a queue 
	val is_empty: 'a queue -> bool
	val enqueue: 'a -> 'a queue -> 'a queue 
	val peek: 'a queue -> 'a 
	val dequeue: 'a queue -> 'a queue
end

module ListQueue : Q = struct
  type 'a queue = 'a list

  let empty = []

  let is_empty q = (q = [])

  let enqueue x q = q @ [x] 

  let peek = function
    | []   -> failwith "Empty"
    | x::_ -> x

  let dequeue = function
    | []   -> failwith "Empty"
    | _::q -> q
end

module TwoListQueue : Q = struct
  type 'a queue = {front:'a list; back:'a list}

  let empty = {front=[]; back=[]}

  let is_empty = function
    | {front=[]; back=[]} -> true
    | _ -> false

  let norm = function
    | {front=[]; back} -> {front=List.rev back; back=[]}
    | q -> q

  let enqueue x q = {q with back=x::q.back} 

  let peek = function 
    | {front=[]; _} -> failwith "Empty"
    | {front=x::_; _} -> x

  let dequeue = function
    | {front=[]; _} -> failwith "Empty"
    | {front=_::xs; back} -> (norm {front=xs; back})
end

(* Creates a ListQueue filled with [n] elements. *)
let fill_queue n (enqueue, empty_q) =
	let rec loop n q =
    if n=0 then q
    else loop (n-1) (enqueue n q) in
  loop n empty_q

let _ = 
	let start = Sys.time() in
	let empty_q = TwoListQueue.empty in 
	let _ = fill_queue 20000 (TwoListQueue.enqueue, empty_q) in
	let elapsed_time = Sys.time() -. start in  
	Printf.printf "%.2f seconds\n" elapsed_time	
	