open RustTypes

let createStruct typ = "struct " ^ typ ^ " { }"

let showVars x =
    match x with
    | FreeVar(name,typ) -> "let " ^ name ^ " = " ^ typ ^ " { };"

let getType = function
    | FreeVar(name,typ) -> typ

let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let printFreeVars vars =
    let types = List.map (fun x -> getType x) vars in

    List.map (fun x -> Printf.printf "%s\n" (createStruct x)) (uniq types);
    Printf.printf "%s" "\n";
    Printf.printf "%s" "fn main() {\n";
    List.map (fun x -> Printf.printf "%s\n" (showVars x)) vars;
    Printf.printf "%s\n" "}";

    Printf.printf "\n";