type 'a tree = Empty | Node of  'a * 'a tree * 'a tree

(* Exercise 10.1 check for height balancedness*)
let rec is_height_bal t = 
let rec aux t = 
match t with
| Empty -> 0
| Node(_, l, r) -> max (aux l) (aux r) +1
in
match t with
| Empty -> true
| Node(_, l,r) ->  abs (aux l) - (aux r) <= 1 && is_height_bal l && is_height_bal r


let is_height_bal t = 
let rec aux t = 
match t with
| Empty -> Some 0
| Node(_, l, r) -> match aux l, aux r with
		     | Some h1, Some h2 -> if abs (h1-h2) <=1 then Some (max h1 h2+1) else None
		     | _ -> None
in
match aux t with
| Some _ -> true
| _ -> false

