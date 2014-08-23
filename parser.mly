%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET
%token NEW POINTER BAR
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD EXPONENT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE VAR
%token <int> LITERAL
%token <float> FLOAT
%token <bool> BOOLEAN
%token <string> STRING
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ 
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EXPONENT

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 } 
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = [];
	 body = List.rev $6 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

/*
vdecl_list:
       { [] }
  | vdecl_list vdecl { $2 :: $1 }*/
 

vdecl:
   VAR ID SEMI { $2 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

 
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | FLOAT            { Float($1) }
  | BOOLEAN          { Boolean($1) }
  | STRING           { String($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EXPONENT expr { Binop($1, Exponent, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr MOD    expr { Binop($1, Mod, $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | NEW LBRACKET expr RBRACKET {VectorInit($3) }
  | NEW LBRACKET expr RBRACKET LBRACKET expr RBRACKET {MatrixInit($3,$6) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | ID LBRACKET expr RBRACKET { VectRef($1,$3) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { MatxRef($1, $3, $6) }
  | ID LBRACKET expr RBRACKET ASSIGN expr { VectAssign($1,$3, $6) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr { MatxAssign($1,$3, $6, $9) }
  | LBRACKET mat_opt RBRACKET {Matrix($2) } 
  | NEW BAR keyvalue_opt BAR {JMap($3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
  
   mrow:
	 { [] }
	| expr { [$1] }
	| mrow COMMA expr { $3 :: $1 }
	
    mat_opt:
	 mrow { [List.rev $1] }
	| mat_opt SEMI mrow { $1 @ [List.rev $3] }
    
     keyvalue_opt:
    { [] }
    | keyvalue_list { List.rev $1 }
    
    keyvalue_list:
    expr POINTER expr { [$1, $3] }
    | keyvalue_list COMMA expr POINTER expr {($3, $5)::$1 }

