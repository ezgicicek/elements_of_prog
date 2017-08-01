open Core.Std

type star = {x:int; y: int; z: int}

let s_compare s1 s2 = 
  let d1 =  s1.x * s1.x + s2.y * s2.y + s1.z * s1.z in
  let d2 =  s2.x * s2.x + s2.y * s2.y + s2.z * s2.z in
  if d1 < d2 then 1 else if d1 = d2 then 0 else (-11)

let k_closest (n: star list) k =
  (* create a max-heap *)
  let k_heap = Fheap.create s_compare in
  let rec aux k_heap l = 
    if l = [] then k_heap
    else 
    let k_heap = Fheap.add k_heap (List.hd_exn l) in
    if Fheap.length k_heap = k +1 then 
	  aux (snd (Fheap.pop_exn k_heap)) (List.tl_exn l)
    else  aux k_heap (List.tl_exn l)
  in aux k_heap n |> Fheap.to_list
