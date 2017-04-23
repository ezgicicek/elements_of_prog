(* Given an array of n integers where A[i] denotes max distance one
can advance from index i, return whether is is possible to advance to
the last location *)

(* Iterative solution *)
let can_reach_end arr =
  let furthest_dist = ref 0 in
  let last = Array.length arr -1 in
  let i = ref 0 in
  while !i <= !furthest_dist && !furthest_dist < last
  do furthest_dist := max !furthest_dist (arr.(!i) + !i);
     i := !i + 1
  done;
  !furthest_dist >= last


(* Version: recursive solution *)
let can_reach_end arr =
  let rec aux arr st en =
    if st = en then true
    else if arr.(st) = 0 then false
    else let max_dist = min (en-st) (arr.(st)) in
	 let i = ref 1 in
	 let res = ref false in
	 while !i <= max_dist && not (!res)
	 do  res := aux arr (st + !i) en;
	     i := !i + 1
	 done; !res
  in aux arr 0 (Array.length arr -1)

open OUnit2

let run_tests () = 
  assert_bool "cannot reach" (not(can_reach_end [|3;2;0;0;2;0;1|]));
  assert_bool "can reach" (can_reach_end [|3;3;1;0;2;0;1|]);
  assert_bool "can reach" (can_reach_end [|3;2;0;1;2;0;5;0;0|]);
  assert_bool "cannot reach"	(not(can_reach_end [|3;2;0;1;2;0;1;0;0|])) ;;
