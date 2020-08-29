open RustTypes

let createStruct typ = "struct " ^ (String.capitalize typ) ^ " { }"

let showVars x =
    match x with
    | FreeVar(name,typ) -> "let " ^ (String.lowercase name) ^ " = " ^ (String.capitalize typ) ^ " { };"

let showConsts x =
    match x with
    | Const(name,typ) -> "const " ^ (name) ^ " : " ^ (String.capitalize typ) ^ " = " ^(String.capitalize typ)^"{ };"

let rec createArguments lst i =
    match lst with
    | typ::xx -> ("a"^string_of_int i^": &" ^ (String.capitalize typ)) :: (createArguments xx (i+1))
    | [] -> []

let showFuncs x =
    match x with
    | Func(args, name, res) -> "fn " ^name ^  "(" ^ String.concat ", " (createArguments args 0) ^ ") -> " ^ (String.capitalize res) ^ " { "^(String.capitalize res)^"{ } }"
    | _ -> ""

let getType = function
    | FreeVar(name,typ) -> typ
    | Type(typ) -> typ


let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let printFuncs vars =
    List.map (fun x -> Printf.printf "%s\n" (showFuncs x)) vars;
    Printf.printf "\n"

let printConsts consts =
    List.map (fun x -> Printf.printf "%s\n" (showConsts x)) consts;
    Printf.printf "\n"

let printMain vars =
    Printf.printf "%s" "fn main() {\n";
    List.map (fun x -> Printf.printf "%s\n" (showVars x)) vars;
    Printf.printf "%s\n" "}"

let printStructs vars =
    let types = List.map (fun x -> getType x) vars in
    List.map (fun x -> Printf.printf "%s\n" (createStruct x)) (uniq types);
    Printf.printf "\n";