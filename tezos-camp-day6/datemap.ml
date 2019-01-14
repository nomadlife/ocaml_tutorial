type date = { month:int; day:int }
module Date = struct
  type t = date
  let compare {month=month1; day=day1} {month=month2; day=day2} =
		match Pervasives.compare month1 month2 with 
		| 0 -> Pervasives.compare day1 day2 
		| c -> c 
end


module DateMap = Map.Make(Date) 

type calendar = string DateMap.t

let map =
	let date1 = {month = 1; day = 7} in
	let date2 = {month = 1; day = 15} in   
	let m = DateMap.add date1 "tezos camp starts" DateMap.empty in 
	DateMap.add date2 "tezos camp ends" m 

let first_after : calendar -> Date.t -> string 
= fun calendar target_date ->
	DateMap.fold (fun date content s ->
		if s = "" && (Date.compare date target_date) > 0 then content 
		else s  
		) calendar "" 

let _ = 
	DateMap.iter (fun date content -> 
		Printf.printf "%d/%d: %s\n" date.month date.day content
	) map;
	 
	print_endline (first_after map {month = 1; day = 5}) 

