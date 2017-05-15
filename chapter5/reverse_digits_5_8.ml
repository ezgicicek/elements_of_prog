(* Reverse the digits of a number: 123 -> 321 *)
let reverse n =
  let rec aux n acc = 
    match n with
    | _ when n < 10 -> acc + n
    | _ ->  aux (n / 10) (10 * ((n mod 10) + acc))
  in aux n 0 


(* Version 1: iterative version *)
let reverse n = 
  let res = ref 0 in
  let rest = ref n in
  while !rest <> 0
  do
    res := 10 * !res + !rest mod 10;
    rest := !rest / 10
  done;
!res
