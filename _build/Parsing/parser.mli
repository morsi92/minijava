exception Error

type token = 
  | VOLATILE
  | VOID
  | TRANSIENT
  | THROWS
  | SYNCHRONIZED
  | STRING
  | STRICTFP
  | STATIC
  | SEMICOLON
  | RIGHT_DIPLE
  | RIGHT_BRACKET
  | RIGHT_BRACE
  | RETURN
  | PUBLIC
  | PROTECTED
  | PRIVATE
  | POINT
  | PATH of (string)
  | PACKAGE
  | OPEN_COMMENT
  | NATIVE
  | MARK
  | LEFT_DIPLE
  | LEFT_BRACKET
  | LEFT_BRACE
  | INTERFACE
  | INT
  | IMPORT
  | IMPLEMENTS
  | IDENT of (string)
  | FLOAT
  | FINAL
  | EXTENDS
  | EQUAL
  | EOF
  | ENUM
  | DEFAULT
  | COMMENT_LINE
  | COMMA
  | CLOSE_COMMENT
  | CLASS
  | BOOL
  | AT
  | ABSTRACT


val code_source: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( Syn.global list )