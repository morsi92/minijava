%{
  (*calling Syntax module*)
	open Syn
	open Exceptions
	let rec getInterfaces = function 
	| [] -> []
	| ","::t -> (getInterfaces t)
	| h::t -> h::getInterfaces t

%}

%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token PUBLIC PRIVATE PROTECTED STRICTFP DEFAULT VOLATILE TRANSIENT
%token INTERFACE CLASS
%token IMPLEMENTS EXTENDS THROWS NATIVE
%token IMPORT PACKAGE
%token SEMICOLON POINT COMMA LEFT_DIPLE RIGHT_DIPLE AT MARK EQUAL
%token COMMENT_LINE
%token OPEN_COMMENT
%token CLOSE_COMMENT
%token ABSTRACT FINAL
%token STATIC SYNCHRONIZED
%token RETURN
%token INT
%token <string> LIDENT 
%token <string> UIDENT
%token STRING
%token <string> PATH
%token FLOAT
%token BOOL
%token VOID
%token EOF

%start code_source
%type < Syn._header list> code_source

%%
code_source :
	|dc=header EOF{ dc}
	|error {Location.print (Location.symbol_loc $startpos $endpos); raise Exceptions.SyntaxError}
header :
  	| h1=header_package h2=header_import* {h1;h2}
	|h2=header_import* {h2}
header_package :
  	| PACKAGE path=PATH SEMICOLON {Package{package_path=path}}

header_import :
	| IMPORT path=PATH SEMICOLON {Import{import_path=path}}


