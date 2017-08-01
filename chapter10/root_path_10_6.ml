type 'a tree = Empty | Node of  'a * 'a tree * 'a tree

(* 10.6 Find existence of a root to leaf path with specified sum *)
let rec pathsum t s = 
match t with
| Empty -> s = 0
| Node(x, l, r) -> pathsum l (s-x) || pathsum r (s-x)



(* Variant Find all root to leaf paths with specified sum *)
let rec pathsum t s = 
match t with
| Empty -> []
| Node(x, Empty, Empty) -> if x = s then [[x]] else []
| Node(x, l, r) -> List.map (fun el -> x:: el)
			    (pathsum l (s-x) @ pathsum r (s-x))

