open Printf
exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold_left max hd tl 

let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max [])
