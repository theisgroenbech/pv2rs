(* Terms are either variables, represented as deBrujin indexes or
   functions *)
type rust =
  | Garbage
  | Type of string
  | FreeVar of string * string
  | Func of string list * string * string
