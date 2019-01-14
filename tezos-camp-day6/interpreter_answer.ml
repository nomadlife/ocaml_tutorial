(** 고급 OCaml 프로그래밍 Part 2
  2019. 1. 11 - 14 @ Tezos blockchain camp
	언어 실행기 (interpreter) 구현
	
	학습 목표: 정수 덧셈만 가능한 계산기 (module Calc_v1 에 정의) 부터 
  mini OCaml 실행기 (module Calc_v8 에 정의) 로 발전시키는 과정으로부터 
	OCaml 의 주요 개념들의 필요성과 사용법 익히기.      
 *)

(** 1. 정수 덧셈만 가능한 계산기 *)
(* 더할 정수들을 리스트로 받는다. *)
module Calc_v1 = struct 
  let calc : int list -> int 
  = fun num_list ->
  	let add = (fun x y -> x + y) in  
  	List.fold_left add 0 num_list 
  (* num_list = [i1; i2; ... ; in] 이라고 할 때, (단 i1 ... in 은 정수) *)
  (* List.fold_left add 0 [i1; i2; ... ; in] = (add (add (add (add 0 i1) i2) i3) ... in) *)
  
  (* 혹은 아래와 같이 재귀함수를 이용하여 정의 가능 *)
  let rec calc 
  = fun num_list ->
  	match num_list with 
  	| hd :: tl -> hd + (calc tl) 
  	| [] -> 0  
  (* 재귀호출 과정 *)
  (* calc [1;2;3] *)
  (*   = 1 + (calc [2;3]) *)
  (*   = 1 + (2 + (calc [3])) *)
  (*   = 1 + (2 + (3 + (calc []))) *)
  (*   = 1 + (2 + (3 + 0)) *)
end

(** 2. 뺄셈 추가 *)
(* 숫자들 뿐 아니라 덧셈 및 뺄셈 연산자도 함께 받아야 하므로 정수 리스트로 표현 불가. *)
(* 한 가지 방법으로 1 + 3 - 2 를 [1; "+"; 3; "-"; "2" ] 와 같이 표현하는 것을 생각해볼 수 있음. *)
(* 그러나 이는 타입 오류. OCaml 리스트는 한 가지 타입의 값들만 담을 수 있기 때문.  *)
(* 숫자를 문자열로 표현하면 이 문제를 회피할 수 있음. 예) ["1"; "+"; "3"; "-"; "2" ] *)
module Calc_v2 = struct
  let rec calc : string list -> int
  = fun str_list -> 
  	match str_list with
  	| hd1 :: hd2 :: tl -> (* 길이가 2 이상인 리스트 *)
  		if (String.compare hd2 "+") = 0 then  (* 앞에서 2번째 원소 문자열이 "+" *)
  			(int_of_string hd1) + (calc tl)
  		else      														(* 앞에서 2번째 원소 문자열이 "-" *)
  			(int_of_string hd1) - (calc tl)
  	| hd :: tl -> int_of_string hd (* 길이가 1 인 리스트 *)
  	| [] -> 0  
  
  (* 이 프로그램의 문제: *)
  (*   바람직하지 않은 문자열 사용을 막을 수 없음. *)
  (*     예: ["1"; "+"; "a"], ["-"; "+"]  *)
  (*   이는 서로 다른 두 종류의 객체 (정수와 연산자)를 문자열을 이용하여 억지로 표현했기 때문 *)
  (*   다음의 조건을 만족시켰으면: *)
  (*   (1) 잘못된 입력이 주어질 가능성을 타입 검사로 실행 전 원천봉쇄 *)
  (*   (2) 정수와 연산자를 포함하는 한 타입을 고안하여 리스트가 잘 정의 *)
  (*   (3) 정수와 연산자를 엄격히 구분 *)
  
  type exp = 
  	| INT of int   (* 정수 *) 
  	| ADD (* 덧셈 연산자 *) 
  	| SUB (* 뺄셈 연산자 *)
  (* 여기서 INT, ADD, SUB는 서로 다른 종류의 객체들을 잘 구분하기 위한 태그 *)
  (* ADD 와 SUB의 경우 태그인 동시에 하나의 상수 값이기도 함 *)
  (* [1; "+"; 3; "-"; "2" ] 는 이제 [INT(1); ADD; INT(3); SUB; INT(2)] 로 표현 가능 *) 
  
  (* 새 타입 num_or_operator 을 이용하여 재정의 *)
  let rec calc : exp list -> int
  = fun lst -> 
  	let get_num v =
  		match v with 
  		| INT (n) -> n
  		| _ -> failwith "invalid input!"
  	in
  	match lst with
  	| hd1 :: hd2 :: tl -> (* 길이가 2 이상인 리스트 *)
  		(match hd2 with 
  		| ADD -> (get_num hd1) + (calc tl)
  		| SUB -> (get_num hd1) - (calc tl)
  		| _ -> failwith "invalid input!"
  		)
  	| hd :: tl -> get_num hd (* 길이가 1 인 리스트 *)
  	| [] -> 0  
end

(** 3. 곱셈 추가 *)
(* 덧셈과 뺄셈, 곱셈이 함께 등장할 경우 연산 우선순위가 모호. *)
(* 예: 1 + 3 * 2 이 (1 + 3) * 2 인지 1 + (3 * 2) 인지.  *)
(* 1차원 리스트의 한계. 2차원인 나무 구조 필요. *)
module Calc_v3 = struct 
  type exp = 
  	| INT of int   (* 정수 *) 
  	| ADD of exp * exp  
  	| SUB of exp * exp 
    | MUL of exp * exp
  (* (1 + 3) * 2 는 MUL(ADD(INT(1), INT(3)), INT(2)) 로 표현  *)
  (* 1 + (3 * 2) 는 ADD(INT(1), MUL(INT(3), INT(2))) 로 표현  *)
  
  let rec calc: exp -> int 
  = fun e -> 
  	match e with 
  	| INT n -> n 
  	| ADD (e1, e2) -> (calc e1) + (calc e2) 
  	| SUB (e1, e2) -> (calc e1) - (calc e2) 
  	| MUL (e1, e2) -> (calc e1) * (calc e2) 
  (* test *)
  (* let _ =                                        *)
  (* 	let e = MUL(ADD(INT(1), INT(3)), INT(2)) in  *)
  (* 	let v = calc e in                            *)
  (* 	Printf.printf "%d\n" v;                      *)
		
  (* v 를 계산하는 과정: *)
  (* calc MUL(ADD(INT(1), INT(3)), INT(2)) *)
  (* -> (calc ADD(INT(1), INT(3))) * (calc INT(2)) *)
  (* -> (calc INT(1)) + (calc INT(3)) * 2 *)
  (* -> (1 + 3) * 2 -> 8 *)
end


(** 4. 변수 1개 추가 *)
(* 변수 X로 표현되는 식과 X의 값을 받아서 계산 결과를 내놓는 계산기 *)
module Calc_v4 = struct 
  type exp = 
  	| X (* 변수 X를 지칭 *)
  	| INT of int 
  	| ADD of exp * exp  
  	| SUB of exp * exp 
    | MUL of exp * exp
  
	(* exp 에 추가로 X의 값도 인자로 받음. *)
  let rec calc: exp -> int -> int 
  = fun e x -> 
  	match e with 
  	| X -> x
  	| INT n -> n 
  	| ADD (e1, e2) -> (calc e1 x) + (calc e2 x) 
  	| SUB (e1, e2) -> (calc e1 x) - (calc e2 x) 
  	| MUL (e1, e2) -> (calc e1 x) * (calc e2 x) 
  
	(* test *)
  (* let _ =                                    *)
  (* 	let e = MUL(ADD(X, INT(3)), INT(2)) in   *)
  (* 	let v = calc e 3 in                      *)
  (* 	Printf.printf "%d\n" v;                  *)
end 		

(** 5. 변수 여러개 추가 *)
(* 변수 여러개로 표현되는 식과 각 변수들의 값을 받아서 계산 결과를 내놓는 계산기 *)
module Calc_v5 = struct 
  type exp = 
  	| VAR of string (* 변수를 지칭. 예) VAR("x"), VAR("y") ... *)
  	| INT of int 
  	| ADD of exp * exp  
  	| SUB of exp * exp 
    | MUL of exp * exp
  
	(* 변수 ID 에서 정수 값으로의 맵핑이 필요. *)
	type environment = (string * int) list
	(* 예) [("x", 1); ("y", 2)]: x와 y에 각각 1과 2 할당.  *)

	(* exp 와 env 를 인자로 받음. *)
  let rec calc: exp -> environment -> int 
  = fun e env -> 
  	match e with 
  	| VAR name -> List.assoc name env 
  	| INT n -> n 
  	| ADD (e1, e2) -> (calc e1 env) + (calc e2 env) 
  	| SUB (e1, e2) -> (calc e1 env) - (calc e2 env) 
  	| MUL (e1, e2) -> (calc e1 env) * (calc e2 env) 
  
	(* test *)
  (* let _ =                                               *)
  (* 	let e = MUL(ADD((VAR("x")), INT(3)), VAR("y")) in   *)
  (* 	let v = calc e [("x", 1); ("y", 2)] in              *)
  (* 	Printf.printf "%d\n" v;                             *)
end 		

(** 6. let 표현식 추가.  *)
module Calc_v6 = struct 
  type exp = 
		| LET of string * exp * exp (* let x = e in e' *)
  	| VAR of string
  	| INT of int 
  	| ADD of exp * exp  
  	| SUB of exp * exp 
    | MUL of exp * exp
  
	(* 변수 ID 에서 정수 값으로의 맵핑이 필요. *)
	type environment = (string * int) list
	(* 예) [("x", 1); ("y", 2)]: x와 y에 각각 1과 2 할당.  *)

	(* exp 와 env 를 인자로 받음. *)
  let rec calc: exp -> environment -> int 
  = fun e env -> 
  	match e with 
		| LET (x, e, e') -> 
			let v = calc e env in 
			let env' = (x, v) :: env in 
			calc e' env' 
  	| VAR name -> List.assoc name env 
  	| INT n -> n 
  	| ADD (e1, e2) -> (calc e1 env) + (calc e2 env) 
  	| SUB (e1, e2) -> (calc e1 env) - (calc e2 env) 
  	| MUL (e1, e2) -> (calc e1 env) * (calc e2 env) 
  
	(* test *)
  (* let _ =                                                                     *)
  (* 	let e = LET("x", INT(3), LET("y", INT(4), ADD(VAR("x"), VAR("y"))))  in   *)
  (* 	let v = calc e [] in                                                      *)
  (* 	Printf.printf "%d\n" v;                                                   *)
end 		

(** 7. 조건문 IF 추가. *)
module Calc_v7 = struct 
  type exp = 
		| EQUAL of exp * exp (* e1 = e2 *)
		| LESS of exp * exp  (* e1 <= e2 *)
		| NOT of exp  (* !e *)
		| BOOL of bool (* true or false *)
		| IF of exp * exp * exp (* if (e1) then e2 else e3 *)
		| LET of string * exp * exp 
  	| VAR of string
  	| INT of int 
  	| ADD of exp * exp  
  	| SUB of exp * exp 
    | MUL of exp * exp
  
	(* 변수 ID 에서 정수 값으로의 맵핑이 필요. *)
	type environment = (string * int) list
	(* 예) [("x", 1); ("y", 2)]: x와 y에 각각 1과 2 할당.  *)

	(* exp 와 env 를 인자로 받음. *)
  let rec calc: exp -> environment -> int 
  = fun e env -> 
  	match e with
		| BOOL _ | EQUAL _ | LESS _ | NOT _  -> failwith "invalid command"
		| IF (e1, e2, e3) -> 
			let cond = eval_cond e1 env in 
			if (cond) then calc e2 env else calc e3 env    
		| LET (x, e, e') -> 
			let v = calc e env in 
			let env' = (x, v) :: env in 
			calc e' env' 
  	| VAR name -> List.assoc name env 
  	| INT n -> n 
  	| ADD (e1, e2) -> (calc e1 env) + (calc e2 env) 
  	| SUB (e1, e2) -> (calc e1 env) - (calc e2 env) 
  	| MUL (e1, e2) -> (calc e1 env) * (calc e2 env) 
  
	and eval_cond: exp -> environment -> bool
	= fun e env -> 
		match e with 
		| BOOL b -> b 
		| EQUAL (e1, e2) -> (calc e1 env) = (calc e2 env) 
		| LESS (e1, e2) -> (calc e1 env) <= (calc e2 env)
		| NOT e -> not (eval_cond e env)
		| _ -> failwith "invalid command" 
	
	(* test *)
  (* let _ =                                                                                           *)
  (* 	let e = LET("x", INT(3), LET("y", INT(4), IF(EQUAL(VAR("x"), VAR("y")), INT(1), INT(2))))  in   *)
  (* 	let v = calc e [] in                                                                            *)
  (* 	Printf.printf "%d\n" v;                                                                         *)
end 		

(** 8. 매개변수 1개인 함수 추가. *)
module Calc_v8 = struct 
  type exp = 
		| LETF of string * string * exp * exp (* let f(p) = e1 in e2  *)
		| CALL of string * exp (* f(e) *) 
		| READ 
		| EQUAL of exp * exp 
		| LESS of exp * exp  
		| NOT of exp 
		| BOOL of bool 
		| IF of exp * exp * exp 
		| LET of string * exp * exp 
  	| VAR of string
  	| INT of int 
  	| ADD of exp * exp  
  	| SUB of exp * exp 
    | MUL of exp * exp
  
	(* 변수 ID 에서 값으로의 맵핑이 필요. *)
	type value_or_func = Num of int | Func of string * exp 
	type environment = (string * value_or_func) list
	(* 예) [("x", Num 1); Func ("f", ("a1", ADD(VAR("a1"), INT(1))))]: x 에 1, f 에 매개변수 a1 함수 몸체 a1 + 1 인 함수 할당 *)

	(* exp 와 env 를 인자로 받음. *)
  let rec calc: exp -> environment -> int 
  = fun e env -> 
  	match e with
		| READ -> read_int()
		| LETF (f, param, e1, e2) -> 
			let env' = (f, Func (param, e1)) :: env in 
			calc e2 env' 
		| CALL (f, arg) ->  
			let (param, body) = 
				(match (List.assoc f env) with
  			| Num n -> failwith "invalid" 
  			| Func (p, b) -> (p, b))
			in 
			calc (LET(param, arg, body)) env 
		| BOOL _ | EQUAL _ | LESS _ | NOT _  -> failwith "invalid command"
		| IF (e1, e2, e3) -> 
			let cond = eval_cond e1 env in 
			if (cond) then calc e2 env else calc e3 env    
		| LET (x, e, e') -> 
			let v = calc e env in 
			let env' = (x, Num v) :: env in 
			calc e' env' 
  	| VAR name -> 
			(match (List.assoc name env) with
			| Num n -> n 
			| Func _ -> failwith "invalid"
			)
  	| INT n -> n 
  	| ADD (e1, e2) -> (calc e1 env) + (calc e2 env) 
  	| SUB (e1, e2) -> (calc e1 env) - (calc e2 env) 
  	| MUL (e1, e2) -> (calc e1 env) * (calc e2 env) 
  
	and eval_cond: exp -> environment -> bool
	= fun e env -> 
		match e with 
		| BOOL b -> b 
		| EQUAL (e1, e2) -> (calc e1 env) = (calc e2 env) 
		| LESS (e1, e2) -> (calc e1 env) <= (calc e2 env)
		| NOT e -> not (eval_cond e env)
		| _ -> failwith "invalid command" 
	
	(* test *)
  (* let _ =                                                                                  *)
	(* 	let body = IF(LESS(VAR("a"), INT(10)), CALL("f", ADD(VAR("a"), INT(1))), VAR("a")) in  *)
  (* 	let e = LETF ("f", "a", body, CALL("f", INT(2)))  in                                   *)
  (* 	let v = calc e [] in                                                                   *)
  (* 	Printf.printf "%d\n" v                                                                 *)
	
end 		

(** 9. 파서를 이용하여 파일을 읽어들여 실행  *)
(* interpret_from_file.ml 참조. *)

(** (실습 1.) Calc_v8 을 확장하여 나눗셈 연산과, 외부 입력으로부터 정수를 받는 READ 구문을 추가하세요 (read_int() 함수 사용) *)
(** (실습 2.) Calc_v8 을 확장하여 함수가 임의의 갯수의 인자를 받을 수 있도록 하세요. *)
(** (실습 3.) Calc_v8 을 확장하여 SWITCH 구문을 추가하세요. 
    SWITCH (e, [(e1, e1'), (e2, e2'), ... (en, en')]
		의미: e의 결과가 e1 이면 e1' 실행, e2이면 e2'실행 ...  
	*)
