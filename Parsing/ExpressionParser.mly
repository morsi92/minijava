%{
	open Syntax
%}

%token EOF LPAR RPAR PLUS MINUS TIMES BY MOD
%token LT LE EQ NE GT GE
%token AND OR
%token <string> IDENTIFIER
%token <float> FLOAT
%token <int> INTEGER
%token <bool> BOOLEAN

%start expression
%type <Syntax.expression> expression

%left OR
%left AND
%left LT LE EQ NE GT GE
%left PLUS MINUS
%left TIMES BY MOD

%%

expression:
	| c = comp EOF {c}
comp:
	| c = comp o = boolop d = comp { Logicop(o,c,d) }
	| e = expr o = compop t = expr { Bincomp(o,e,t) }
	| b = BOOLEAN { BoolLit b }
	| e = expr { e }
expr:
	| e = expr o = bop t = expr { Binop(o,e,t) }
	| id = IDENTIFIER { Var id}
	| f = FLOAT { FloatLit f }
	| e = INTEGER { IntLit e }

%inline bop:
	| PLUS     { Plus }
	| MINUS      { Minus }
	| TIMES     { Times }
	| BY       { Divide }
	| MOD       { Remainder }

%inline boolop:
	| AND { And }
	| OR { Or }

%inline compop:
	| LT { LessThan }
	| LE { LessEqual }
	| EQ { Equal }
	| GT { GreaterThan }
	| GE { GreaterEqual }
	| NE { NotEqual }

%%

