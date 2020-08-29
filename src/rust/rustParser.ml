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

let rec createArguments f =
  match f with
  t::tl -> t.tname :: (createArguments tl)
  | _ -> []

let translateEquation eq ttl =
  if((List.length (fst eq.f_type)) = 0) then Garbage (* This is Constants, we take care of those in translateConsts*)
  else begin
  let params = createArguments((fst eq.f_type))  in
  let result = (snd(eq.f_type)).tname in
  Func(params, fixed_or_renamable eq.f_name , result)
  end

let translateConsts f =
  match f.f_cat with
    Red (r::rs) -> []
  | Eq ttl->
    if((List.length (fst f.f_type)) = 0) then
      [Const(fixed_or_renamable(f.f_name), (snd f.f_type).tname)]
    else []
  | _ -> []

let translateFuncs f =
  match f.f_cat with
  Eq ttl-> translateEquation f ttl
  | _ -> Garbage

let rec cleanFunctions tf =
  match tf with
  | t::ts when (fixed_or_renamable t.f_name) = "||" || (fixed_or_renamable t.f_name) = "true" || (fixed_or_renamable t.f_name) = "not" || (fixed_or_renamable t.f_name) = "false" || (fixed_or_renamable t.f_name) = "&&" ->
              (cleanFunctions ts)
  | t::ts -> t::(cleanFunctions ts)
  | [] -> []

let rec cleanFuncs tf =
  match tf with
  | (Func(a,b,c))::ts -> (Func(a,b,c)) :: cleanFuncs ts
  | _::ts -> (cleanFuncs ts)
  | [] -> []

let parse state =
  match state.pi_process_query with
    SingleProcessSingleQuery(p, _) | SingleProcess(p,_) ->

    (* Types *)
    let types = List.map (fun x-> Type(x.tname)) state.pi_types in

    (* Free Variables *)
    let frees = (List.map(fun f -> (f.f_name, (snd f.f_type).tname)) (List.rev state.pi_freenames)) in
    let env = List.map (fun (s,t) -> FreeVar(((fixed_or_renamable s)),(t))) frees in

    (* Functions *)
    let ll = (cleanFunctions state.pi_funs) in
    let funcs = List.map (fun f -> translateFuncs f) ll in
    let cleanFuncs = cleanFuncs funcs in

    (* Constants *)
    let consts = List.concat(List.map (fun f -> translateConsts f) ll) in

    printStructs (env@types);
    printConsts (consts);
    printFuncs cleanFuncs;
    printMain env;

    Printf.printf "\n";
  | _ -> ()
