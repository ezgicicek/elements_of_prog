type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let dq = Queue.create ()
 
let st = (Stack.create ())

let depth_first_traversal t = 
let rec aux t = 
  match t with
  | Node (l, x, r) -> print_int x ; (Stack.push r st); aux l
  | Leaf -> if Stack.is_empty st then () else aux (Stack.pop st) 
in aux t 
