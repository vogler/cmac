{
  open Printf
  open Simc_pars

  let tokenString = function
    | EOF      -> ""
    | ERROR    -> "ERROR"
    | AMP      -> "&"
    | ADD      -> "+" 
    | SUB      -> "-"
    | MUL      -> "*"
    | DIV      -> "/"
    | LEQ      -> "<="
    | LE       -> "<"
    | GEQ      -> ">="
    | GR       -> ">"
    | EQ       -> "=="
    | NEQ      -> "!="
    | VAL x    -> string_of_int x
    | ID x     -> x
    | COMMA    -> ","
    | LPAR     -> "(" 
    | RPAR     -> ")"
    | IF       -> "if"
    | THEN     -> "then"
    | ELSE     -> "else"
    | LBRAC    -> "["
    | RBRAC    -> "]"
    | LCURL    -> "{"
    | RCURL    -> "}"
    | DEF      -> "="
    | COLON    -> ":"
    | SCOLON   -> ";"
    | RETURN   -> "return"
    | GOTO     -> "goto"
    | CONTINUE -> "continue"
    | BREAK    -> "break"
    | WHILE    -> "while"
    | FOR      -> "for"
    | SWITCH   -> "switch"
    | CASE     -> "case"
    | DEFAULT  -> "default"
    | STRUCT   -> "struct"
    | INT      -> "int"
    | VOID     -> "void"
    | DO       -> "do"
    | DOT      -> "."
  
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*

rule ctok = parse
  | digit+ as inum { VAL (int_of_string inum)}
  | "."            { DOT       }
  | "("            { LPAR      }
  | ")"            { RPAR      }
  | "["            { LBRAC     }
  | "]"            { RBRAC     }
  | "{"            { LCURL     }
  | "}"            { RCURL     }
  | "+"            { ADD       }
  | "-"            { SUB       }
  | "*"            { MUL       }
  | "/"            { DIV       }
  | "<="           { LEQ       }
  | "<"            { LE        }
  | "!="           { NEQ       }
  | ">"            { GR        }
  | ">="           { GEQ       }
  | "=="           { EQ        }
  | "&"            { AMP       }
  | "="            { DEF       }
  | ","            { COMMA     }
  | ":"            { COLON     }
  | ";"            { SCOLON    }
  | "if"           { IF        }
  | "then"         { THEN      }
  | "else"         { ELSE      }
  | "return"       { RETURN    }
  | "goto"         { GOTO      }
  | "continue"     { CONTINUE  }
  | "break"        { BREAK     }
  | "while"        { WHILE     }
  | "for"          { FOR       }
  | "switch"       { SWITCH    }
  | "case"         { CASE      }
  | "default"      { DEFAULT   }
  | "struct"       { STRUCT    }
  | "dot"          { DOT       }
  | "do"           { DO        }
  | "int"          { INT       }
  | "void"         { VOID      }
  | id     as word { ID word  }
  | [' ' '\t' '\n']	{ ctok lexbuf }
  | _ as c
  	{ printf "Unrecognized character: %c\n" c; ERROR}
  | eof
  	{ EOF }
  	
{(*  	
  let rec parseAll lexbuf =
    let tstream = ref [] in 
    let rec lex_one () = 
      try tstream := ctok lexbuf :: !tstream; lex_one () with End_of_file -> ()
    in
    lex_one ();
    List.rev !tstream

  let main () =
     let printToken t = printf "%s " (tokenString t) in
     let cin =
       if Array.length Sys.argv > 1
       then open_in Sys.argv.(1)
       else stdin
     in
     let lexbuf = Lexing.from_channel cin in
     List.iter printToken (parseAll lexbuf);
     printf "\n"

   let _ = Printexc.print main ()
*)
}