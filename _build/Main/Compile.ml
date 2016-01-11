(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

open Parser
open Hight_level
open Syn

let rec examine_ast lexbuf =
        let res = nexttoken lexbuf in
        print_lexeme res;
        print_string " ";
        match res with
            | EOF -> ()
            | _ -> examine_ast lexbuf

let execute lexbuf verbose =
	let exp = (Parser.code_source Hight_level.nexttoken lexbuf) in (
	print_endline (Syn.terms_to_string exp); );
