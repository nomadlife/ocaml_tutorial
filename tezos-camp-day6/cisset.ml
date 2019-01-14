module Cis = struct
	type t = string
	let compare x y = String.compare (String.lowercase_ascii x) (String.lowercase_ascii y)  
end

module CisSet = Set.Make(Cis)

let _ = print_endline (string_of_bool (CisSet.(equal (of_list ["grr"; "argh"]) (of_list ["GRR"; "aRgh"]))))