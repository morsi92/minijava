%{

	open Syn
	open Exceptions

	let rec getAttributes = function 
	| [] -> []
	| (Attribute a)::t -> a::(getAttributes t)
	| h::t -> getAttributes t

	let rec getMethods = function 
	| [] -> []
	| (Method a)::t -> a::(getMethods t)
	| h::t -> getMethods t
%}

%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token PUBLIC PRIVATE PROTECTED STRICTFP DEFAULT VOLATILE TRANSIENT
%token INTERFACE CLASS ENUM
%token IMPLEMENTS EXTENDS THROWS 
%token IMPORT PACKAGE
%token SEMICOLON POINT COMMA LEFT_DIPLE RIGHT_DIPLE AT MARK EQUAL
%token COMMENT_LINE
%token OPEN_COMMENT
%token CLOSE_COMMENT
%token ABSTRACT FINAL
%token STATIC SYNCHRONIZED NATIVE
%token RETURN
%token INT
%token <string> IDENT
%token STRING
%token <string> PATH
%token FLOAT
%token BOOL
%token VOID
%token EOF

%start code_source
%type < Syn.global list > code_source

%%

code_source :
	|header=header_package dc=body* EOF {header;dc} 
	|error {Location.print (Location.symbol_loc $startpos $endpos); raise Exceptions.SyntaxError}

header_package :
	|header=header_import {Header{header=header}} (* *)
	|PACKAGE path=path SEMICOLON {Header{header=[{name="package";path=path}]}}
	|PACKAGE path=path SEMICOLON header=header_import {Header{header={name="package";path=path}::header}}
	

header_import :
	|{[]} 
	| IMPORT path=path SEMICOLON {[{name="import";path=path}]}
	| IMPORT path=path SEMICOLON hi=header_import {{name="import";path=path}::hi}

	
path :
	|ident=IDENT {ident}
	|path=PATH {path}
body :
	|(*an=annot*) dc=dec_class {dc} 
	|(*an=annot*) di=dec_interface {di}
	|(*an=annot*) de=dec_enum {de}
	(*|(*annot?*) (*dec_annot*)*)
(*annot:
	| {Annotation{name="";param=""}}
  | AT i=IDENT SEMICOLON {Annotation{name=i;param=""}}
  | AT i=IDENT LEFT_BRACKET MARK j=IDENT MARK RIGHT_BRACKET SEMICOLON {Annotation{name=i;param=j}}
  (*)| AT UIDENT LEFT_BRACKET (UIDENT EQUAL MARK STRING MARK) (COMMA UIDENT EQUAL MARK STRING MARK)* RIGHT_BRACKET {} *)
	(*@Example (b="0",r="1")*)
*)
dec_class:
	|modifiers=modifier_class_toplevel CLASS name=IDENT generic=generic LEFT_BRACE cb=class_body* RIGHT_BRACE {Class{modifiers=modifiers;name=name;parent="Object";interfaces=[];generic=generic;attributes=(getAttributes cb);methods=(getMethods cb)}}
	|modifiers=modifier_class_toplevel CLASS name=IDENT generic=generic IMPLEMENTS interfaces=interfaces LEFT_BRACE cb=class_body* RIGHT_BRACE {Class{modifiers=modifiers;name=name;parent="Object";interfaces=interfaces;generic=generic;attributes=(getAttributes cb);methods=(getMethods cb)}}
	|modifiers=modifier_class_toplevel CLASS name=IDENT generic=generic EXTENDS parent=IDENT  LEFT_BRACE cb=class_body* RIGHT_BRACE {Class{modifiers=modifiers;name=name;parent=parent;interfaces=[];generic=generic;attributes=(getAttributes cb);methods=(getMethods cb)}}
	|modifiers=modifier_class_toplevel CLASS name=IDENT generic=generic EXTENDS parent=IDENT IMPLEMENTS interfaces=interfaces  LEFT_BRACE cb=class_body* RIGHT_BRACE {Class{modifiers=modifiers;name=name;parent=parent;interfaces=interfaces;generic=generic;attributes=(getAttributes cb);methods=(getMethods cb)}}


modifier_class_toplevel :
	|{[]}
	|modifier=modifier_class {[modifier]}
	|modifier=modifier_class modifier_class_toplevel=modifier_class_toplevel {modifier::modifier_class_toplevel}
modifier_class:
	|PUBLIC {"public"}
	|STRICTFP {"strictfp"}
	|FINAL {"final"}
	|ABSTRACT {"abstract"}
	
dec_interface:
	|INTERFACE name=IDENT generic=generic  LEFT_BRACE RIGHT_BRACE {Interface{name=name;interfaces=[];generic=generic}}
	|INTERFACE name=IDENT generic=generic IMPLEMENTS interfaces=interfaces LEFT_BRACE RIGHT_BRACE {Interface{name=name;interfaces= interfaces;generic=generic}}

interfaces :	
	|i=IDENT {[i]}
	|i=IDENT COMMA interfaces=interfaces {i::interfaces}
generic :
	|{""}
	|LEFT_DIPLE name=IDENT RIGHT_DIPLE {"<"^name^">"}
dec_enum :
	|ENUM name=IDENT LEFT_BRACE RIGHT_BRACE {Enum{name=name}}
class_body:
	|modifiers=modifiers_attribute _type=what_type name=IDENT SEMICOLON {Attribute{modifiers=modifiers;_type=_type;name=name}}
	|(*modifier_method*) _type=what_type name=IDENT LEFT_BRACKET parameters=parameters RIGHT_BRACKET LEFT_BRACE RIGHT_BRACE {Method{modifiers=[];_type=_type;name=name;parameters=parameters;exceptions=[]}}
	|(*modifier_method*) _type=what_type name=IDENT LEFT_BRACKET parameters=parameters RIGHT_BRACKET THROWS exceptions=exceptions LEFT_BRACE RIGHT_BRACE {Method{modifiers=[];_type=_type;name=name;parameters=parameters;exceptions=exceptions}}
	|modifiers=abstract_modifiers _type=what_type name=IDENT LEFT_BRACKET parameters=parameters RIGHT_BRACKET  LEFT_BRACE RIGHT_BRACE {Method{modifiers=modifiers;_type=_type;name=name;parameters=parameters;exceptions=[]}}
	|modifiers=abstract_modifiers _type=what_type name=IDENT LEFT_BRACKET parameters=parameters RIGHT_BRACKET THROWS exceptions=exceptions LEFT_BRACE RIGHT_BRACE {Method{modifiers=modifiers;_type=_type;name=name;parameters=parameters;exceptions=exceptions}}

abstract_modifiers:
	|ABSTRACT {["abstract"]}
	|PUBLIC ABSTRACT {["public";"abstract"]}
exceptions:
	|e=IDENT {[e]}
	|e=IDENT COMMA exceptions=exceptions {e::exceptions}	
modifiers_attribute:
	|{[]}
	|modifier=modifier_attribute {[modifier]}
	|modifier=modifier_attribute modifiers_attribute=modifiers_attribute {modifier::modifiers_attribute}

(*modifiers_method:
	|{[]}
	|modifier=modifier_method {[modifier]}
	|modifier=modifier_method modifiers_method=modifiers_method {modifier::modifiers_method} *)
modifier_attribute :
	|PUBLIC {"public"}
	|PRIVATE {"private"}
	|PROTECTED {"protected"}
	|STATIC {"static"}
	|FINAL {"final"}
	|VOLATILE {"volatile"}
	|TRANSIENT {"transient"}

(*modifier_method :
	|PUBLIC {"public"}
	|PRIVATE {"private"}
	|PROTECTED {"protected"}
	|STATIC {"static"}
	|FINAL {"final"}
	|DEFAULT {"default"}
	|STRICTFP {"strictfp"}
	|NATIVE {"native"}
	|SYNCHRONIZED {"synchronized"}*)
parameters:
	|{[]}
	|parameters=parameters_filled {parameters}
parameters_filled:	
	|_type=what_type name=IDENT {[{_type=_type;name=name}]}
	|_type=what_type name=IDENT COMMA parameters=parameters {{_type=_type;name=name}::parameters}

what_type :
	|INT {"int"}
	|VOID {"void"}
	|STRING {"String"}
	|BOOL {"boolean"}
	|FLOAT {"float"}
	|ident=IDENT {ident}
%%
