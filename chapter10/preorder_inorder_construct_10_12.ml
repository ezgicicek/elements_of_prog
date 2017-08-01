type 'a tree = Empty | Node of  'a * 'a tree * 'a tree


(* 10.12 Given an inorder and preorder traversal, construct the tree *)
let rec make_tree ino preo = 
let rec split_at x l =
match l with
| [] -> [], []
| h:: tl -> if h = x then [], tl else 
	      let (l,r) = split_at x tl in (h::l, r)
in
let rec take n l = 
match l with
| [] -> [], []
| h:: tl -> if n = 0 then ([], l) else 
	      let (l,r) = take (n-1) tl in (h::l, r)
in
match preo with
| [] -> Empty
| r:: tl ->
   let ileft, iright = split_at r ino in
   let pleft, pright =  take (List.length ileft) tl in
   Node(r, make_tree ileft pleft, make_tree iright pright)

