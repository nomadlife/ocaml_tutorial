(** 고급 OCaml 프로그래밍 Part 2
  2019. 1. 11 - 14 @ Tezos blockchain camp
	언어 실행기 (interpreter)  
  
	mini OCaml 실행기 (module Calc_v8 에 정의) 핵심 부분과 
	파서 (lexer.mll, parser.mly에 정의) 를 결합하여 
	텍스트 파일로부터 코드를 읽어들여서 실행하는 언어 실행기 구현.
 *)

open Interpreter

let _ =
	let open Calc_v8 in  
	try
		let file_name = Sys.argv.(1) in  
  	let file_channel = open_in file_name in
  	let lexbuf = Lexing.from_channel file_channel in
  	let pgm = Parser.program Lexer.start lexbuf in
  	let v = calc pgm [] in 
  	Printf.printf "%d\n" v;
  	close_in file_channel
	with Invalid_argument _ -> prerr_endline (Printf.sprintf "Usages: %s [file to interpret]\n" Sys.argv.(0)); exit(-1)
	| _ -> failwith "invalid file!"
