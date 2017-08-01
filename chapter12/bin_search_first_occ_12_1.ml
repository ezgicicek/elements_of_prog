let rec first_occ_search arr x =
  let i = ref 0 in
  let j = ref (Array.length arr -1) in
  let rec aux st en arr = 
    let res = ref (-1) in
    let mid = ref (-1) in 
    while !st <= !en
    do 
      mid := (!st + (!en - !st) / 2);
      Printf.printf "st:%d en:%d mid :%d \n" !st !en !mid;
      if arr.(!mid) = x then (res := !mid; en := !mid -1)
      else if x < arr.(!mid) then en := !mid -1
      else st := !mid + 1
    done; !res
  in aux i j arr


(* Variant: first occurence of the element greater than the key *)
(* Idea: find the last occurence and then the next one is the element
greater than the key *)
let rec last_occ_search arr x =
  let i = ref 0 in
  let j = ref (Array.length arr -1) in
  let rec aux st en arr = 
    let res = ref (-1) in
    let mid = ref (-1) in 
    while !st <= !en
    do 
      mid := (!st + (!en - !st) / 2);
      if arr.(!mid) = x then (res := !mid; st := !mid +1)
      else if x < arr.(!mid) then en := !mid -1
      else st := !mid + 1
    done; !res+1
  in aux i j arr
