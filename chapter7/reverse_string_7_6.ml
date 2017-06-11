(* functional solution *)
let reverse str = 
let l = Str.split (Str.regexp " ") str in
List.fold_left (fun acc x -> x ^ " " ^ acc) "" l
