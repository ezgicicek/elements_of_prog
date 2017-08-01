open Core.Std

let moving_median (n: int list) =
  (* create a min-heap *)
  let max_heap = Fheap.create (fun x y -> if x < y then 1 else if x = y then 0 else (-1)) in
  let min_heap = Fheap.create compare in
  let rec aux min_heap max_heap l = 
    let res min_heap max_heap =
      if Fheap.length min_heap = Fheap.length max_heap 
      then (Fheap.top_exn min_heap +. Fheap.top_exn max_heap)/. 2.0 
      else Fheap.top_exn min_heap in
    if l = [] then []
    else
      let x = (List.hd_exn l) in
      let tl = (List.tl_exn l) in
      let min_heap = 
	if Fheap.is_empty min_heap then Fheap.add min_heap x
	else if x >= Fheap.top_exn min_heap then Fheap.add min_heap x else min_heap in
      let max_heap = if x < Fheap.top_exn min_heap then Fheap.add max_heap x else max_heap in
      if Fheap.length min_heap > Fheap.length max_heap + 1 then
	let (min, min_heap) = Fheap.pop_exn min_heap in
	let max_heap = Fheap.add max_heap min in
	res min_heap max_heap :: aux min_heap max_heap tl
      else if Fheap.length max_heap > Fheap.length min_heap  then
	let (max, max_heap) = Fheap.pop_exn max_heap in
	let min_heap = Fheap.add min_heap max in
	res min_heap max_heap :: aux min_heap max_heap tl
      else 
	res min_heap max_heap :: aux min_heap max_heap tl
  in aux min_heap max_heap (List.map ~f:float_of_int n)

