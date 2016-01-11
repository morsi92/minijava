{
open Parser
(*type lexeme =
    |CLASS
    (*|LIDENT of string
    |UIDENT of string*)
	|IDENT of string
	|LEFT_BRACE
    |RIGHT_BRACE
    |LEFT_BRACKET
    |RIGHT_BRACKET
    |SEMICOLON
    |POINT
    |COMMA
    |INTERFACE
    |PUBLIC
    |STATIC
    |PROTECTED
    |PRIVATE
    |ABSTRACT
    |DEFAULT
    |EXTENDS
    |IMPLEMENTS
    |IMPORT
    |PACKAGE
    |FINAL
    |THROWS
    |EOF
    |COMMENT_LINE
    |OPEN_COMMENT
    |CLOSE_COMMENT
    |RETURN
    |STRICTFP
    |SYNCHRONIZED
    |LEFT_DIPLE
    |RIGHT_DIPLE
    |AT
    |MARK
    |EQUAL
    |VOLATILE
    |TRANSIENT
	|PATH of string
	|INT
	|STRING
	|FLOAT
	|VOID
	|BOOL

*)
}


let letter = ['a'-'z' 'A'-'Z']
let letterMaj = ['A'-'Z']
let letterMinus = ['a'-'z']
let digit = ['0'-'9']
let ident = letter(letter|digit|'_')*
(*let uident = letterMaj(letter|digit|'_')* *)
(*let lident = letterMinus(letter|digit|'_')* *)
let space = [' ' '\t']
let newline = ['\n' '\r']
let path = ident(('.')ident)*('.''*')?

rule nexttoken = parse
    |space+ { nexttoken lexbuf }
    |newline+ { nexttoken lexbuf } (* Temp set like this *)
    (* Detecting Comments and ignoring them*)
    |"//" [^'\n''\r']* "\n"  {print_string ("one line comment ignored" ^"\n") ; Lexing.new_line lexbuf; nexttoken lexbuf}
    | "/*" _* "*/"           {print_string ("multiple line comment ignored" ^"\n") ; Lexing.new_line lexbuf; nexttoken lexbuf}
    |"import" {IMPORT}
    |"package" {PACKAGE}
    |"{" {LEFT_BRACE}
    |"}" {RIGHT_BRACE}
    |"(" {LEFT_BRACKET}
    |")" {RIGHT_BRACKET}
    |";" {SEMICOLON}
    |"." {POINT}
    |"," {COMMA}
    |"@" {AT}
    |"\"" {MARK}
    |"=" {EQUAL}
    (*|"//" {COMMENT_LINE}*)
    (*|"/*" {OPEN_COMMENT}*)
    (*|"*/" {CLOSE_COMMENT}*)
    |"<" {LEFT_DIPLE}
    |">" {RIGHT_DIPLE}
    |"abstract" {ABSTRACT}
    |"final" {FINAL}
    |"static" {STATIC}
    |"public" {PUBLIC}
    |"protected" {PROTECTED}
    |"private" {PRIVATE}
    |"default" {DEFAULT}
    |"strictfp" {STRICTFP}
	|"native" {NATIVE}
    |"class" {CLASS}
    |"interface" {INTERFACE}
    |"implements" {IMPLEMENTS}
    |"extends" {EXTENDS}
    |"throws" {THROWS}
    |"return" {RETURN}
    |"int" {INT}
    |"float" {FLOAT}
    |"boolean" {BOOL}
    |"String" {STRING}
    |"void" {VOID}
    |"synchronized" {SYNCHRONIZED}
    |"volatile"{VOLATILE}
    |"transient"{TRANSIENT}
    |eof {EOF}
	|ident as str {IDENT(str)}
	|path as str {PATH(str)}
	(*|lident as str { LIDENT(str) } *)
    (*|uident as str { UIDENT(str) } *)



{
let print_lexeme tok = function
	|EOF -> print_string "EOF"
	|CLASS -> print_string "CLASS"
	|EXTENDS -> print_string "EXTENDS"
	|LEFT_BRACE -> print_string "{"
	|RIGHT_BRACE -> print_string "}"
	|IDENT s -> print_string "IDENT "; print_string s
}
