%{
open Simc
%}

%token AMP
%token ADD SUB MUL DIV LEQ LE GEQ GR EQ NEQ

%token EOF LPAR RPAR LBRAC RBRAC LCURL RCURL DEF COLON SCOLON COMMA DOT INT VOID
%token IF ELSE RETURN GOTO CONTINUE BREAK WHILE FOR SWITCH CASE DEFAULT STRUCT DO
%token <int>    VAL
%token <string> ID

%start decls
%type <Simc.expr>       exp
%type <Simc.lval>       lval
%type <Simc.typ>        typ
%type <Simc.stmt>       stmt
%type <Simc.decl list>  decls

%nonassoc LOW AMP DEF
%nonassoc LEQ LE GEQ GR EQ NEQ
%left ADD SUB
%left MUL DIV
%nonassoc DOT LBRAC LPAR

%%

lval: ID                   { Var $1 }
    | MUL s_exp            { Deref $2 }
    | lval DOT ID          { Field ($1,$3) }
    | lval LBRAC exp RBRAC { Index ($1,$3) }
    ;

f_args_:                    { [] }
       | COMMA exp f_args_  { $2::$3 }
       ;

f_args:               { [] }
      | exp f_args_   { $1::$2 }
      ;

s_exp: VAL                    { Val $1 }
     | AMP lval               { Addr $2 }
     | lval     %prec LOW     { Lval $1 }
     | s_exp LPAR f_args RPAR { App ($1,$3) }
     | LPAR exp RPAR          { $2 }
     ;

exp: s_exp            { $1 }
   | exp ADD exp      { Binop ($1,Add,$3) }
   | exp SUB exp      { Binop ($1,Sub,$3) }
   | exp MUL exp      { Binop ($1,Mul,$3) }
   | exp DIV exp      { Binop ($1,Div,$3) }
   | exp LEQ exp      { Binop ($1,Leq,$3) }
   | exp LE  exp      { Binop ($1,Le ,$3) }
   | exp GEQ exp      { Binop ($1,Geq,$3) }
   | exp GR  exp      { Binop ($1,Gr ,$3) }
   | exp EQ  exp      { Binop ($1,Eq ,$3) }
   | exp NEQ exp      { Binop ($1,Neq,$3) }
   | exp DEF exp      { Binop ($1,Asn,$3) }
   ;

typ: INT              { Int }
    | VOID            { Void }
    | STRUCT ID       { Struct $2 }
    | typ MUL         { Ptr $1 }
    ;

vdecl: typ ID         { ($1,$2) }
;

vdecls:                        { [] }
      | vdecl SCOLON vdecls    { $1::$3 }
      ;

decl: STRUCT ID LCURL vdecls RCURL                 { StructDecl ($2,$4) }
    | vdecl SCOLON                                 { (fun (x,y) -> Global (x,y) ) $1 }
    | typ ID LPAR args RPAR LCURL stmts RCURL      { Function ($1,$2,$4,$7) }
    ;

args:              { [] }
    | vdecl args_  { $1::$2 }
    ;

args_:                    {[]}
     | COMMA vdecl args_  { $2::$3 }
     ;

decls:            { [] }
     | decl decls { $1::$2 }
     | EOF        { [] }
     ;

stmt: exp SCOLON                                    { Expr $1 }
    | vdecl SCOLON                                  { (fun (x,y) -> Local (x,y) ) $1 }
    | IF LPAR exp RPAR stmt ELSE stmt               { IfThenElse ($3,$5,$7) }
    | FOR LPAR exp SCOLON exp SCOLON exp RPAR stmt  { For ($3,$5,$7,$9) }
    | WHILE LPAR exp RPAR stmt                      { While ($3,$5) }
    | DO stmt WHILE LPAR exp RPAR SCOLON            { DoWhile ($2,$5) }
    | ID COLON                                      { Label $1 }
    | GOTO ID SCOLON                                { Goto $2 }
    | SWITCH LPAR exp RPAR LCURL sstmts RCURL       { Switch ($3,$6) }
    | CONTINUE SCOLON                               { Continue }
    | BREAK SCOLON                                  { Break }
    | RETURN exp SCOLON                             { Return (Some $2) }
    | RETURN SCOLON                                 { Return None }
    | LCURL stmts RCURL                             { Block $2 }
    ;

stmts:            { [] }
     | stmt stmts { $1::$2 }
     ;

sstmt: CASE VAL COLON  { Case $2 }
     | DEFAULT COLON   { Default }
     | stmt            { NormalStatement $1 }
     ;

sstmts:               { [] }
      | sstmt sstmts  { $1::$2 }
      ;

