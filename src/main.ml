open Pitypes
open Parsing_helper

let readFile filename =
  let pi_state0 = Pitsyntax.parse_file filename in
  ignore (Printf.printf "Debug info");
  pi_state0
  (* parse pi_state0 *)


let main =
  readFile ((Sys.argv.(1)))
