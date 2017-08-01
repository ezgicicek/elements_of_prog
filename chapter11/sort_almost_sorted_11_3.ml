open Core.Std

let sort_almost_sorted (n: int list) k =
  (* create a min-heap *)
  let max_heap = Fheap.create compare in
  let rec aux m_heap l = 
    if l = [] then Fheap.to_list m_heap
    else 
    let m_heap = Fheap.add m_heap (List.hd_exn l) in
    if Fheap.length m_heap = k +1 then 
      let (min, m_heap) = (Fheap.pop_exn m_heap) in
      min :: aux m_heap (List.tl_exn l)
    else aux m_heap (List.tl_exn l)
  in aux max_heap n


