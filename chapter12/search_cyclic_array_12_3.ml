(* Find the minimum element in a cyclically sorted array *)
let find arr = 
  let rec aux arr i j = 
    if i >= j then i
    else 
      let mid = i + (j-i) /2  in
      if arr.(mid) > arr.(j)
      then aux arr (mid+1) j
      else aux arr i mid
    in aux arr 0 (Array.length arr -1)



(* Variant : find the maximum element in the strictly ascending-descenting chain *)
let find_max arr = 
  let rec aux arr i j = 
    if i >= j then arr.(i)
    else 
      let mid = i + (j-i) /2  in
      if arr.(mid) < arr.(mid+1)
      then aux arr (mid+1) j
      else aux arr i mid
    in aux arr 0 (Array.length arr -1)

