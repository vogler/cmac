{
  open Simc_pars
  exception Token of string
  let line = ref 1
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let nl = '\r'?'\n'             (* new line *)
let ws  = [' ' '\t']           (* whitespace *)
let id  = ('_' | alpha) ('_' | alpha | digit)*
let endlinecomment = "//" [^'\n']*
let multlinecomment = "/*"([^'*']|('*'+[^'*''/'])|nl)*'*'+'/'
let comments = endlinecomment | multlinecomment

rule ctok = parse
  | ws | comments  { ctok lexbuf }     (* skip blanks and comments *)
  | nl             { incr line; ctok lexbuf } (* keep track of current line *)
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
  | "do"           { DO        }
  | "int"          { INT       }
  | "void"         { VOID      }
  | id as word     { ID word  }
  | eof            { EOF }
  | _ as x         { raise(Token (Char.escaped x^": unknown token in line "^string_of_int !line)) }
