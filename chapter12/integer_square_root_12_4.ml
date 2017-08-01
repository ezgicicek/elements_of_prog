
(* Brute force *)
let square_root n = 
let rec aux  i = 
  if i * i <= n then aux (i+1)
  else i
in (aux 1) -1


let square_root n = 
let rec aux i j = 
  if i <= j then
    let mid = i + (j-i)/2 in
    let msq = mid * mid in
    if msq <= n then aux (mid+1) j
    else  aux i (mid-1)
  else j
in aux 0 n


