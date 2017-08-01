type 'a tree = Empty | Node of  'a * 'a tree * 'a tree


(* 10.2 check if a tree is symmetric *)
let is_symmetric t = 
let rec aux t1 t2 = 
match t1, t2 with
| Empty, Empty -> true
| Node(x, l, r), Node(y, l',r') when x = y -> aux l r' && aux r l'
| _ -> false
in aux t t
