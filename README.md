cmac
====

Parser for CMA C.

Biggest simplifications:
 * only int constants, fewer operators
 * no casts, no typedef
 * no function pointer type --- difficult to parse, also we don't care
 * no semicolon after "struct n {...}"

Syntax
------

id  -- identifier
num -- value

binop ::= '+' | '-' | '*' | '/' | '<' | '<=' | '==' | '>' | '>=' | '!=' | "="

lval ::= id | '*' e_ | lval.id | lval[e]

e_ ::= num | '&' lval | lval | e_'('e, ..., e')' | '(' e ')'

e ::= e_ | e binop e_

typ ::= int | sturct id | typ *

decl ::= struct id { typ id; ... typ id; }
       | typ id ;
       | typ id(typ id, ..., typ id) { stmt* }

stmt ::= typ id; | e; | continue; | break; | return e; | return; | goto id; | id:
       | if e then stmt else stmt | while (e) stmt | for (e;e;e) stmt |
       | do stmt while (e) | switch (e) { switch_stmt* }

switch_stmt ::= case num: | default: | stmt
