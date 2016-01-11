{
    open ExpressionParser

    let print_lexeme = function
        | EOF -> print_string "EOF"
        | PLUS -> print_string "PLUS"
        | MINUS -> print_string "MINUS"
        | TIMES -> print_string "DIV"
        | BY -> print_string "TIMES"
        | IDENTIFIER id ->
            print_string "IDENTIFIER(";
            print_string id;
            print_string ")"
        | FLOAT f ->
            print_string "FLOAT(";
            print_float f;
            print_string ")"
        | INTEGER n ->
            print_string "INT(";
            print_int n;
            print_string ")"
        | lexeme -> print_string "(Autre lexeme)"
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = letter (letter | digit | '_')*
let space = [' ' '\t' '\n']
let real_number = digit+ '.' digit*
let integer_number = digit+

rule nexttoken = parse
    space+ { nexttoken lexbuf }
    | eof { EOF }
    | "(" { LPAR }
    | ")" { RPAR }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { BY }
    | "%" { MOD }
    | "<=" { LE }
    | "<" { LT }
    | ">=" { LE }
    | ">" { GT }
    | "==" { EQ }
    | "!=" { NE }
    | identifier as id { IDENTIFIER id }
    | integer_number as e { INTEGER (int_of_string e) }
    | real_number as f { FLOAT (float_of_string f) }
