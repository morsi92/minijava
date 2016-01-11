(* This module is reserved for the building of thr abstract Syntax Tree
	We created print out functions of the compile in this module to :
	 		- allow  a string presentation of the AST
			- print more significant messages (rather than association string print out to Token generation in the lexer)

	We are using OCaml Records to represent Java entitie's structure
*)

 (*Types definitions *)

(* Package type : package_path *)
type _package = {package_path:string}

(* Import type : package_path *)
type _import = {import_path:string}

(* The attribute type is characterized by its modifier, type and name *)
type _attribute = {modifiers:string list ;_type:string;name:string}

(* a formal parameter is represented by its type and name*)
type _parameter = {_type:string;name:string}

(* a method type has modifiers , type of return, name, formal parameters and exceptions list *)
type _method = {modifiers:string list;_type:string;name:string;parameters:_parameter list;exceptions:string list}

(*a Class type contains modifiers, name, parent (super class), list of interfaces, is_generic? , and a list of attributes and methods
forming its body*)
type _class = {modifiers:string list;name:string;parent:string;interfaces:string list;generic:string;attributes:_attribute list;methods:_method list}

(*interfaces could be generic, they are characterized by name,list of interfaces they are implementing and list of methods
 "work in progress" *)
type _interface = {name:string;interfaces:string list;generic:string}

(*type enum is characterized with its name and list of values "work in progress"*)
type _enum = {name:string}

(*we are dealing with only two types of Annotations : @annot and @annot("string")*)
type _annotation = {name:string;param:string}

(*package_or_import type takes package or import for its name and the directory path *)
type package_or_import = {name:string;path:string}

(*the header is a list of package_or_import type*)
type _header = {header : package_or_import list}

(*the type class body is list of attributes and methods*)
type _class_body =
	|Attribute of _attribute
	|Method of _method

(*type parameters = Parameter of _parameter *)
type global =
	|Enum of _enum
	|Class of _class
	|Interface of _interface
	|Annotation of _annotation
	|Header of _header

(*Implementing functions that allow the AST to string transformation*)

(*this is a list of helper function implements used in types_to string print out *)
let rec print_list  = function
	|[] -> ""
	|e::f -> e ^ " "^ print_list f

(*the following functions checks if the list in the input is non empty and returns a word used in types_to_string implementation*)
let print_implements  = function
	|[] -> ""
	|_ -> " implements "

let print_attributes  = function
	|[] -> ""
	|_ -> " attributes: "

let print_methods  = function
	|[] -> ""
	|_ -> " Methods: "

let print_throws  = function
	|[] -> ""
	|_ -> " throws: "

(**)
let rec print_attributes_list : _attribute list -> string = function
	|[] -> ""
	|{modifiers=modifiers;_type=_type;name=name}::t -> (print_list modifiers)^name ^ " : "^ _type ^ " ; " ^ print_attributes_list t

let rec print_parameters_list : _parameter list -> string = function
	|[] -> ""
	|{_type=_type;name=name}::t -> name ^ " : "^ _type ^ " ; " ^ print_parameters_list t
let rec print_methods_list : _method list -> string = function
	|[] -> ""
	|{modifiers=modifiers;_type=_type;name=name;parameters=parameters;exceptions=exceptions}::t ->(print_list modifiers)^ _type ^ " "^ name ^ "("^ print_parameters_list parameters ^")"^(print_throws exceptions)^(print_list exceptions)^ print_methods_list t

let rec print_header_list = function
	|[] -> ""
	|{name=name;path=path}::t -> name ^ " "^path ^ " ; " ^ print_header_list t

let class_to_string : _class -> string = function
	|{modifiers=modifiers;name=name;parent=parent;interfaces=interfaces;generic=generic;attributes=attributes;methods=methods} ->(print_list modifiers)^ "class " ^ name^ generic ^ " inherits " ^ parent ^ (print_implements interfaces) ^ (print_list interfaces) ^ (print_attributes attributes) ^ (print_attributes_list attributes) ^ (print_methods methods) ^  (print_methods_list methods)
let interface_to_string : _interface -> string = function
	|{name=name;interfaces=interfaces;generic=generic} -> "interface "^ name^ generic ^  (print_implements interfaces) ^ (print_list interfaces)
let enum_to_string : _enum -> string = function
	|{name=name} -> "enum " ^ name
let annotation_to_string : _annotation -> string = function
	|{name=name;param=param} -> match (name,param) with
		| ("","") -> ""
		| (_,"")  -> "@" ^ name
		| (_,_)   -> "@" ^ name ^ "(\"" ^ param ^ "\")"
let package_to_string : _package -> string = function
	|{package_path=package_path} -> "package "^package_path^";"
let import_to_string : _import -> string = function
	|{import_path=import_path} -> "import "^import_path^";"
let header_to_string : _header -> string = function
	|{header=header} -> print_header_list header

let term_to_string : global -> string = function
	|Enum e -> enum_to_string e
	|Class c -> class_to_string c
	|Interface i -> interface_to_string i
	|Annotation a -> annotation_to_string a
	|Header h -> header_to_string h

let terms_to_string = function
	|l -> String.concat "\n" (List.map term_to_string l)
