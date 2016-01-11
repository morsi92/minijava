
type _package = {package_path:string}
type _import = {import_path:string}
type _attribute = {modifiers:string list ;_type:string;name:string}
type _parameter = {_type:string;name:string}
type _method = {modifiers:string list;_type:string;name:string;parameters:_parameter list;exceptions:string list}
type _class = {modifiers:string list;name:string;parent:string;interfaces:string list;generic:string;attributes:_attribute list;methods:_method list}
type _interface = {name:string;interfaces:string list;generic:string}
type _enum = {name:string}
type _annotation = {name:string;param:string}
type package_or_import = {name:string;path:string}
type _header = {header : package_or_import list}
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
let rec print_list  = function 
	|[] -> ""
	|e::f -> e ^ " "^ print_list f

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
