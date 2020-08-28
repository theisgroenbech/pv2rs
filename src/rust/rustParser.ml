open Lltypes
open Types
open Pitypes
open Display
open RustTypes
open RustPrinter

let fixed_or_renamable f =
    match f with
    Fixed s -> s
    | Renamable r -> r.orig_name

let parse state =
  match state.pi_process_query with
    SingleProcessSingleQuery(p, _) | SingleProcess(p,_) ->
    let frees = (List.map(fun f -> (f.f_name, (snd f.f_type).tname)) (List.rev state.pi_freenames)) in
    let env = List.map (fun (s,t) -> FreeVar((String.lowercase (fixed_or_renamable s)),(String.capitalize t))) frees in

    printFreeVars env;

    Printf.printf "\n";
  | _ -> ()
