open Printf 
open Simc_lex

let parse buf : Simc.decl list =
  Simc_pars.decls Simc_lex.ctok buf

let main () =
  let lexbuf = Lexing.from_channel stdin in
  printf "%s\n" (Simc.declsToString (parse lexbuf))

let _ = Printexc.print main () 

