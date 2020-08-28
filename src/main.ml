open Pitypes
open Parsing_helper
open RustParser

let readFile filename =
  let pi_state0 = Pitsyntax.parse_file filename in
  parse(pi_state0)

  (* parse pi_state0 *)


let main = readFile ((Sys.argv.(1)))
