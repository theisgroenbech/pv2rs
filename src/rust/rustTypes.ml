(* Terms are either variables, represented as deBrujin indexes or
   functions *)
type rust =
  | Garbage
  | FreeVar of string * string
  | Func of string list * string * string
