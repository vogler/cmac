type binop = Add | Sub | Mul | Div | Leq | Le | Geq | Gr | Eq | Neq | Asn

type lval =
  | Var   of string
  | Deref of expr
  | Field of lval * string
  | Index of lval * expr
and expr =
  | Val   of int
  | Lval  of lval
  | Addr  of lval
  | Binop of expr * binop * expr
  | App   of expr * expr list

type typ =
  | Int | Void
  | Struct of string
  | Ptr    of typ

type stmt =
  | Continue
  | Break
  | Return     of expr option
  | Local      of typ  * string
  | Expr       of expr
  | IfThenElse of expr * stmt * stmt
  | For        of expr * expr * expr * stmt
  | While      of expr * stmt
  | DoWhile    of stmt * expr
  | Label      of string
  | Goto       of string
  | Switch     of expr * (switch_stmt list)
  | Block      of stmt list
and switch_stmt =
  | Default
  | Case            of int
  | NormalStatement of stmt

type decl =
  | StructDecl of string * (typ * string) list
  | Global     of typ * string
  | Function   of typ * string * (typ * string) list * stmt list

(* helper functions *)

let binopToString = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Leq -> "<="
  | Le  -> "<"
  | Geq -> ">="
  | Gr  -> ">"
  | Eq  -> "=="
  | Neq -> "!="
  | Asn -> "="

let rec typToString = function
  | Int -> "int"
  | Void -> "void"
  | Struct x -> "struct "^x
  | Ptr t -> typToString t^" *"

let rec lvalToString = function
  | Var x -> x
  | Deref e -> exprToString e
  | Index (l,e) -> lvalToString l^"["^exprToString e^"]"
  | Field (l,n) -> lvalToString l^"."^n

and exprToString = function
  | Val x         -> string_of_int x
  | Lval x        -> lvalToString x
  | Addr x        -> lvalToString x
  | Binop (x,o,y) -> exprToString x^" "^binopToString o^" "^exprToString y
  | App (f, xs)   -> exprToString f^"("^funArgsToString xs^")"

and funArgsToString = function
    | [] -> ""
    | [x] -> exprToString x
    | x::xs -> exprToString x^", "^funArgsToString xs

and stmtToString = function
    | Continue -> "continue;\n"
    | Break -> "break;\n"
    | Return None -> "return;\n"
    | Return (Some x) -> "return "^exprToString x^";\n"
    | Local (t,x) -> typToString t^" "^x^";\n"
    | Expr e -> exprToString e^";\n"
    | IfThenElse (b,x,y) -> "if ("^exprToString b^")\n"^stmtToString x^"else\n"^stmtToString y
    | For (i,t,p,s) -> "for ("^exprToString i^";"^exprToString t^";"^exprToString p^")\n"^stmtToString s
    | While (b,s) -> "while ("^exprToString b^")\n"^stmtToString s
    | DoWhile (s,b) -> "do\n"^stmtToString s^"while ("^exprToString b^");\n"
    | Label s -> s^":"
    | Goto s  -> "goto "^s^";\n"
    | Switch (e,xs) -> "switch ("^exprToString e^") {\n"^sstmtToString xs^"}\n"
    | Block xs -> "{\n"^stmtsToString xs^"}\n"

and sstmtToString = function
  | []                      -> ""
  | Default::xs             -> "default:\n"^sstmtToString xs
  | (Case x)::xs            -> "case "^string_of_int x^":\n"^sstmtToString xs
  | (NormalStatement s)::xs -> stmtToString s^sstmtToString xs

and stmtsToString = function
  | [] -> ""
  | x::xs -> "\t" ^ stmtToString x ^ stmtsToString xs

let rec defsToString = function
  | [] -> "\n"
  | (t,x)::xs -> "\n\t"^typToString t^" "^x^";"^defsToString xs

let rec argToString = function
  | [] -> ""
  | [t,x] -> typToString t^" "^x
  | (t,x)::xs -> typToString t^" "^x^", "^argToString xs

let declToString = function
  | StructDecl (n,xs) -> "struct "^n^" {"^defsToString xs^"}\n"
  | Global  (t,n)  ->  typToString t^" "^n^";\n"
  | Function (r,n,args,xs) -> typToString r^" "^n^"("^argToString args^") {\n"^stmtsToString xs^"}\n"

let rec declsToString = function
  | [] -> ""
  | x::xs -> declToString x^declsToString xs
