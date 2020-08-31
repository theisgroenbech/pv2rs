open Lltypes
open Types
open Pitypes
open Display
open RustTypes
open RustPrinter
(* STATIC VARIABLES *)
let channel_suffix = "_ch"

let print p =
  Printf.printf "%s\n" p

let rec commasep f l =
  match l with
  | [] -> ""
  | [x] -> f x
  | x::xs -> f x ^ ", " ^ commasep f xs

let rec indentLine i = String.make i ' '

let newLine i = "\n" ^ indentLine i

let show t =
  let rec showVTerm t =
    match t with
    | VVar(i) -> i
    | VFun(name, args) -> name ^ "(" ^ commasep showVTerm args ^ ")"

  and showVForm t (indent:int) =
    match t with
    | VPred(name, args) -> name ^ "(" ^ commasep showVTerm args  ^ ")"
    | VTensor((VTensor(_,_) as l), r) -> showVForm l (indent+1) ^ newLine indent ^ "*" ^newLine (indent+1) ^ showVForm r (indent+1)
    | VTensor(l, (VTensor(_,_) as r)) -> newLine (indent+1) ^ showVForm l (indent+1) ^ newLine indent ^ "*" ^  showVForm r (indent+1)
    | VTensor((VLolli(_,_) as l), r) -> showVForm l (indent+1) ^ newLine indent ^ "*" ^ newLine (indent+1) ^ showVForm r (indent+1)
    | VTensor(l, (VLolli(_,_) as r)) -> newLine (indent+1) ^ showVForm l (indent+1) ^ newLine indent ^ "*" ^ showVForm r (indent+1)
    | VTensor(l, r) -> newLine (indent+1) ^ showVForm l (indent+1) ^ newLine indent ^ "*" ^ newLine (indent+1) ^ showVForm r (indent+1)
    | VLolli((VTensor(_,_) as l), r) -> "(" ^ showVForm l (indent+1) ^ newLine indent ^ "-o" ^ newLine (indent+1) ^ showVForm r (indent+1) ^ ")"
    | VLolli(l, (VTensor(_,_) as r)) -> newLine (indent+1) ^ "(" ^ showVForm l (indent+1) ^ newLine indent ^ "-o" ^ showVForm r (indent+1) ^ ")"
    | VLolli((VLolli(_,_) as l), r) -> "(" ^ showVForm l (indent+1) ^ newLine indent ^ "-o" ^ newLine (indent+1) ^ showVForm r (indent+1) ^ ")"
    | VLolli(l, (VLolli(_,_) as r)) -> newLine (indent+1) ^ "(" ^ showVForm l (indent+1) ^ newLine indent ^ "-o" ^ showVForm r (indent+1) ^ ")"
    | VLolli(l, r) -> newLine (indent+1) ^ "(" ^ showVForm l (indent+1) ^ newLine indent ^ "-o" ^ newLine (indent+1) ^ showVForm r (indent+1) ^ ")"
    | VForall(name, f) -> "(forall " ^ commasep (fun x -> x) name ^ ". " ^ showVForm f indent ^ ")"
    | VExists(name, f) -> "(exists " ^ commasep (fun x -> x) name ^ ". " ^ showVForm f indent ^ ")"
    | VPling(f) -> "!" ^ showVForm f indent
  in showVForm t 0


let rec createTensors l =
  match l with
    x :: [] -> x
  | x::xs -> VTensor(x,createTensors xs)
  | [] -> VPred("one",[])

let fixed_or_renamable f =
    match f with
    Fixed s -> s
    | Renamable r -> r.orig_name

let rec translateTerm t  =
  let rec createPairs = function
    x::y::xs when xs != [] -> translateTerm x :: ("pair" ^ translateTerm y) :: createPairs xs
  | x::xs -> [translateTerm x]@createPairs xs
  | [] -> [] in

  match t with
    Var b -> b.vname.orig_name ^ ":" ^ b.btype.tname
(*    | _ -> "NONE"*)
    | FunApp(f,l) ->
(*    "NONE"*)
      (*if (String.length (fixed_or_renamable f.f_name)) = 0 then
        String.concat "," (createPairs l)
      else *)
(*      if ((List.length l) = 0) then*)
(*        (fixed_or_renamable f.f_name) ^ ":" ^ (snd f.f_type).tname*)
(*      else*)
(*""*)

        (List.map (fun x-> Printf.printf "TERMS: %s" (fixed_or_renamable(f.f_name))) l);
        (fixed_or_renamable f.f_name) ^ ":"^ (snd f.f_type).tname  ^ (String.concat ":" (List.map translateTerm l))



let rec getTermString t  =
  match t with
    Var b -> b.vname.orig_name
  | FunApp(f,l) ->
    String.concat (fixed_or_renamable f.f_name) (List.rev (List.map (fun x -> getTermString x) l))


(*let rec translatePattern p =

  let rec createPairs = function
    x::y::xs when xs != [] -> translatePattern x :: [VFun("pair", [translatePattern y] @ createPairs xs)]
  | x::xs -> [translatePattern x]@createPairs xs
  | [] -> [] in

  match p with
    PatVar x ->
      VVar(x.vname.orig_name)
  | PatTuple (f, plist)->
      if (String.length (fixed_or_renamable f.f_name)) = 0 then
          VFun("pair", createPairs plist)
      else
        VFun((fixed_or_renamable f.f_name), List.map translatePattern plist)
  | PatEqual x -> translateTerm x*)

let rec freeVars pat =
  match pat with
    PatVar x -> [x.vname.orig_name ^ ":" ^ x.btype.tname]
  | PatTuple (f, plist) -> List.concat (List.map freeVars plist)
  | PatEqual t ->
      match t with
        Var b -> [b.vname.orig_name ^ ":" ^ b.btype.tname]
      | FunApp(f,l) -> [(fixed_or_renamable f.f_name) ^ ":" ^ (snd f.f_type).tname]

let rec containsEquals pat =
  match pat with
    PatVar x -> false
  | PatTuple (f, plist) -> let l =  (List.map containsEquals plist) in
                         if (List.exists ((=)true) l)
                         then true else false
  | PatEqual t -> true

let rec freeVarsNoEquals pat =
  match pat with
    PatVar x -> [x.vname.orig_name ^ ":" ^ x.btype.tname]
  | PatTuple (f, plist) -> List.concat (List.map freeVars plist)
  | PatEqual t -> []

let createChannel env =
  let rec c i =
    match (List.exists ((=) ("c"^(string_of_int i))) env) with
      true -> c (i+1)
    | false -> "c"^(string_of_int i)
  in c 0
  (* (getTermString t)^channel_suffix *)

let rec getTerm2 t =

  let rec getStrings e i =
    match e with
      Var b -> ["v"^b.vname.orig_name]
    | FunApp(f,l) ->
      let da = List.concat (List.map (fun e -> getStrings e (i+1)) l) in

      ["f"^(string_of_int i)^(fixed_or_renamable f.f_name)] @ da
    in
    getStrings t 0


let rec normalize = function (* TODO Reimplement this *)
(*    FunApp({f_name="<>"} as f, [x; y]) ->
     FunApp({f with f_name="not"}, [FunApp({f with f_name="="}, [x; y])])
  | FunApp({f_name="&&"} as f, [x; FunApp({f_name="||"}, [y; z])]) ->
     (FunApp({f with f_name="||"}, [normalize (FunApp({f with f_name="&&"}, [x; y])); normalize (FunApp({f with f_name="&&"}, [x; z]))]))
  | FunApp({f_name="&&"} as f, [FunApp({f_name="||"}, [y; z]); x]) ->
     (FunApp({f with f_name="||"}, [normalize (FunApp({f with f_name="&&"}, [y; x])); normalize (FunApp({f with f_name="&&"}, [z; x]))]))
  | FunApp({f_name="not"} as f, [FunApp({f_name="&&"}, [x; y])]) ->
     normalize (FunApp({f with f_name="||"}, [FunApp({f with f_name="not"}, [x]); FunApp({f with f_name="not"}, [y])]))
  | FunApp({f_name="not"} as f, [FunApp({f_name="||"}, [x; y])]) ->
     normalize (FunApp({f with f_name="&&"}, [FunApp({f with f_name="not"}, [x]); FunApp({f with f_name="not"}, [y])]))
  | FunApp({f_name="||"} as f, [x; y]) ->
     FunApp({f with f_name="||"}, [normalize x; normalize y])
  | FunApp({f_name="&&"} as f, [x; y]) as r ->
     let r' = FunApp({f with f_name="&&"}, [normalize x; normalize y]) in
     if r = r' then r else normalize r'*)
  | t -> t


let rec freeVarsTerm t  =
  match t with
    Var b -> [b.vname.orig_name ^ ":" ^ b.btype.tname]
  | FunApp(f,l) -> List.concat (List.map (fun e -> freeVarsTerm e) l)

let rec freeVarsTermList ts =
  let rec freeVarsTermList' = function
    [] -> []
  | t::ts -> let v = freeVarsTerm t in
            let vs = freeVarsTermList' ts in
            (v @ vs) in
   List.sort_uniq compare (freeVarsTermList' ts)

let rec translate t env =
  match t with
    Nil -> ""
  | Par (p, q) ->
      let pp = translate p env in
      let qq = translate q env in
      "Par(" ^ pp ^ "," ^ qq ^ ")"
  | Repl (p, occ) ->
    "Replicate(" ^ (translate p env)^ ")"
  | Restr (f, (args,_), p, occ) ->
      "new " ^ (fixed_or_renamable f.f_name) ^ ":" ^ (snd f.f_type).tname ^ "\n" ^ translate p env
  | Output (tc, t, p, occ) ->
         if p = Nil then
           "Out(" ^ translateTerm tc ^  ", " ^ translateTerm t ^ ")"
         else
           "Out()"
  | Input (tc, pat, p,occ) ->
        let freeVars = freeVars pat in
(*        let env = freeVars @ env in*)
          if p = Nil then
            "In(" ^ (String.concat ", " freeVars)  ^ ")"(*VForall(freeVars, VPred("msg", [translateTerm tc; translatePattern pat]))*)
          else
            "In(" ^ (String.concat ", " freeVars)  ^ ")" (*VForall(freeVars, VLolli(VPred("msg", [translateTerm tc; translatePattern pat]), translate p env))*)
  | Let (pat, t, p, q, occ) ->
      let freeVars = freeVarsNoEquals pat in
(*      let channel = createChannel env in*)
(*      let env = channel :: freeVars @ env in*)
(*translateTerm t ^*)
       "Let("^ (String.concat ", " freeVars) ^") " ^ translate p env
(*      if (freeVars = []) then begin
       if p = Nil then
       "LET"
         VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VPred("res", [VVar(channel); translatePattern pat])))
       else
         VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VLolli(VPred("res", [VVar(channel); translatePattern pat]), translate p env)))
      end else begin
        if p = Nil then
          VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VForall(freeVars, VPred("res", [VVar(channel); translatePattern pat]))))
        else
          VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VForall(freeVars, VLolli(VPred("res", [VVar(channel); translatePattern pat]), translate p env))))
      end*)
  | Event (f,(env_args,_),p,occ) -> "EVENT"
  | NamedProcess(name, tl, p) -> name ^" = \n   "^ translate p env^ ""
  | Phase (i,p,occ) -> "PHASE"(*NOT SUPPORTED*)
(*  translate p env*)
  | LetFilter (bl, f, p, q, occ) -> translate p env
  | Insert (t, p, occ) -> "INSERT"
  | Get (pat, t, p, Nil, occ) when t = Terms.true_term -> "GET"
  | _ -> failwith "Type not supported"
(*  | _ -> "x"*)


(*   | Test (t, p, q,occ) ->
     let channel = createChannel env in
     let env = channel :: env in
      let rec translate' = function
        (*Test(FunApp({f_name="="}, [t1; t2]), p, q, occ) ->
          VTensor(
            VExists([channel],
               VTensor(
                 VPred("msg", [VVar(channel); translateTerm t1]),
                 VLolli(
                   VPred("msg", [VVar(channel); translateTerm t2]),
                   translate p env))),
           translate q env)
       | Test(FunApp({f_name="||"}, [t1; t2]), p, q, occ) ->
          VTensor(translate (Test(t1, p, q, occ)) env, translate (Test(t2, p, q, occ)) env)
       | Test(FunApp({f_name="&&"}, [t1; t2]), p, q, occ) ->
          translate (Test(t1,Test(t2, p, q, occ), q, occ)) env
       | Test(FunApp({f_name="not"}, [t1]), p, q, occ) ->
          VTensor(translate p env, translate q env)*)
       | _ -> failwith "Test format is not supported yet"
     in translate' (Test(normalize t, p, q, occ))

  | Input (tc, pat, p,occ) ->
    let freeVars = freeVars pat in
    let channel = createChannel env in
    let env = channel :: freeVars @ env in
      if p = Nil then
        VForall(freeVars, VPred("msg", [translateTerm tc; translatePattern pat]))
      else
        VForall(freeVars, VLolli(VPred("msg", [translateTerm tc; translatePattern pat]), translate p env))
  | Output (tc, t, p, occ) ->
      if p = Nil then
        VPred("msg", [translateTerm tc; translateTerm t])
      else
        VTensor(VPred("msg", [translateTerm tc; translateTerm t]), translate p env)
  | Let (pat, t, p, q, occ) ->
    let freeVars = freeVarsNoEquals pat in
    let channel = createChannel env in
    let env = channel :: freeVars @ env in
    if (freeVars = []) then begin
     if p = Nil then
       VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VPred("res", [VVar(channel); translatePattern pat])))
     else
       VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VLolli(VPred("res", [VVar(channel); translatePattern pat]), translate p env)))
    end else begin
      if p = Nil then
        VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VForall(freeVars, VPred("res", [VVar(channel); translatePattern pat]))))
      else
        VExists([channel], VTensor(VPred("reduc", [VVar(channel); translateTerm t]), VForall(freeVars, VLolli(VPred("res", [VVar(channel); translatePattern pat]), translate p env))))
    end
  | Event (f,(env_args,_),p,occ) ->
      if p = Nil then
        VPred("event", [translateTerm f])
      else
        VLolli(VPred("event", [translateTerm f]), translate p env)
  | NamedProcess(name, tl, p) ->
      translate p env
  | Phase (i,p,occ) -> (*NOT SUPPORTED*) translate p env
  | LetFilter (bl, f, p, q, occ) -> translate p env
  | Insert (t, p, occ) ->
      if p = Nil then
        VPling(VPred("table", [translateTerm t]))
      else
        VTensor(VPling(VPred("table", [translateTerm t])), translate p env)
  | Get (pat, t, p, Nil, occ) when t = Terms.true_term ->
      if p = Nil then
        VForall(freeVars pat, VPling(VPred("table", [translatePattern pat])))
      else
        VForall(freeVars pat, VLolli(VPling(VPred("table", [translatePattern pat])), translate p env))
  | _ -> failwith "Type not supported"*)




let translateTypet f =
  match f with
    (tl, t) -> (List.iter (fun typet -> Printf.printf "Argument: %s \n" typet.tname) tl); Printf.printf "Result: %s \n \n" t.tname

let rec translateTerm2 t  =
  match t with
    Var b -> VVar(b.vname.orig_name)
  | FunApp(f,l) -> VFun((fixed_or_renamable f.f_name), (List.map (fun e -> translateTerm2 e) l))



(*let translateRewriteRule name r =
  let rec translateHyp = function
      [t] -> [translateTerm2 t]
    | t::ts -> VTensor(translateHyp [t], translateHyp ts) in
  match r with
    (ts, t, ttl) ->
     VTensor(VPling(VForall(freeVarsTermList ts, VLolli(translateHyp ts, VPling(VPred("k", [translateTerm2 t]))))),
       VPling(VForall("channel"::freeVarsTermList ts, VLolli(VPred("reduc", [VVar "channel"; VFun(name, List.map translateTerm2 ts)]), VPred("res", VVar "channel"::[translateTerm2 t])))))
  | _ -> failwith "Rewriterule not matching"*)

let getNumberOfEquations f =
  match f with
    (tl, t) ->
              for i = 1 to List.length tl do
                Printf.printf "args%i" i
              done

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

let rec simplify = function
    VTensor(VPred("one",[]), f) -> f
  | VTensor(f, VPred("one",[])) -> f
  | VTensor(f1, f2) as f ->
    let f' = VTensor(simplify f1, simplify f2) in
    if f = f' then f else simplify f'
  | VLolli(VPred("one",[]), f2) -> f2
  | VLolli(_, VPred("one",[])) -> VPred("one",[])
  | VLolli(f1, f2) as f ->
    let f' = VLolli(simplify f1, simplify f2) in
    if f = f' then f else simplify f'
  | VForall(v, f) -> VForall(v, simplify f)
  | VExists(v, f) -> VExists(v, simplify f)
  | VPling(f) -> VPling(simplify f)
  | VPred(_, _) as f -> f

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

    (* Process *)

    let f = translate (p.proc) env in
    Printf.printf "%s\n" f;

    printStructs (env@types);
    printConsts (consts);
    printFuncs cleanFuncs;
    printMain env;

    Printf.printf "\n";
  | _ -> ()
