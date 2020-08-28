(* Terms are either variables, represented as deBrujin indexes or
   functions *)
type term =
  | FreeVar of string * string