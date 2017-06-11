let mapping d = 
match int_of_char d - int_of_char '0' with
| 2 -> ["A";"B";"C"]
| 3 -> ["D";"E";"F"]
| 4 -> ["G";"H";"I"]
| 5 -> ["J";"K";"L"]
| 6 -> ["M";"N";"O"]
| 7 -> ["P";"Q";"R";"S"]
| 8 -> ["T";"U"; "V"]
| 9 -> ["W";"X";"Y";"Z"]
| _ -> [String.make 1 d]

let rec get_mnemonics digits = 
  if digits = "" then [""]
  else 
    let rest = get_mnemonics @@ String.sub digits 1 @@ String.length digits -1 in
    let get_code r = List.map (fun x -> x ^ r) @@ mapping digits.[0] in
    List.fold_left (fun acc each -> get_code each @ acc) [] rest
