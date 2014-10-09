open Batteries

let parse buf : Simc.decl list =
  Simc_pars.decls Simc_lex.ctok buf

let _ =
  let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  let lexbuf = Lexing.from_channel cin in
  print_endline @@ Simc.declsToString @@ parse lexbuf
