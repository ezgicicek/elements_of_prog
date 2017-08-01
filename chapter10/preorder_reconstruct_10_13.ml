type 'a tree = Empty | Node of  'a * 'a tree * 'a tree


(* 10.13 reconstruct binary tree from preorder traversal with markers *)
let rec preorder l = 
  match l with
  | [] -> Empty, []
  | h::tl -> if h <> "O" then 
	       let l, acc' = preorder tl in
	       let r, acc'' = preorder acc' in
	       Node (h, l, r), acc''
	     else Empty, tl
			   

(* Variant: reconstruct binary tree from postorder traversal with markers *)

let postorder l = 
let rec aux l =
  match l with
  | [] -> Empty, []
  | h::tl -> if h <> "O" then 
	       let r, acc' = aux tl in
	       let l, acc'' = aux acc' in
	       Node (h, l, r), acc''
	     else Empty, tl
in aux (List.rev l)
