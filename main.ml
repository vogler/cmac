open Batteries

let parse = Simc_pars.decls Simc_lex.ctok % Lexing.from_channel

let _ =
  let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  print_endline @@ Simc.declsToString @@ parse cin
