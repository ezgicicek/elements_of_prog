type 'a tree = Empty | Node of  'a * 'a tree * 'a tree


(* 10.3 find the lowest common ancestor *)
let rec lca t x y =
match t with
| Empty -> None
| Node(a, l, r) ->
   if x = a || y = a  then
     Some a
   else
     match lca l x y, lca r x y with
       | Some n, Some n' -> Some a
       | Some n, None 
       | None, Some n -> Some n
       | _ -> None 


(* Variant find the lowest common ancestor in a BST *)
let rec lca_bst t x y =
match t with
| Empty -> None
| Node(a, l, r) ->
   if x = a || y = a  then
     Some a
   else if x < a && y > a then Some a
   else if x < a && y < a then lca_bst l x y
   else lca_bst r x y 
  
