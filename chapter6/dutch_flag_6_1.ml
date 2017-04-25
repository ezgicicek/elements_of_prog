let swap arr i j =
let tmp = arr.(!i) in
  	 arr.(!i) <- arr.(!j);
  	 arr.(!j) <- tmp

(* Version 1: *)
(* First pass : move all elems. < pivot to the beginning *)
(* Second pass: move all elements > pibot to the end     *)
let dutchFlagPartition ~pivotIndex:x ~array:arr =
  let piv = arr.(x) in
  let len = Array.length arr in 
  let gt = ref (len-1)  in
  let lt = ref 0 in
  while !lt < !gt
  do 
    if arr.(!lt) < piv then lt := !lt + 1
    else 
      begin
	swap arr lt gt;
	gt := !gt-1
      end
  done;
  gt := (len-1);
  while !lt < !gt
  do
    if arr.(!lt) <= piv then lt := !lt + 1
    else 
      begin
	swap arr lt gt;
	gt := !gt-1
      end
  done;
 arr

(* Version 2: *)
(* Single pass : divide array into three  *)
(* leftmost    : [0, smaller] *)
(* middle      : [smaller,equal] *)
(* unclassified: [equal, greater] *)
(* rightmost   : [greater, size-1] *)

let dutchFlagPartition ~pivotIndex:x ~array:arr =
  let piv = arr.(x) in
  let len = Array.length arr in
  let larger = ref (len-1)  in
  let smaller = ref 0 in
  let equal = ref 0 in
  while !equal <= !larger
  do
    Printf.printf "equal:%d larger :%d\n" (!equal) (!larger);
    if arr.(!equal) = piv then equal := !equal + 1
    else if arr.(!equal) < piv then
      begin
	swap arr smaller equal;
	smaller := !smaller + 1;
	equal := !equal + 1
      end
    else
      begin
	swap arr equal larger;
	larger := !larger -1
      end
  done;
  arr
    

 (* Variant 1: Assuming that values of the array are in {0,1,2},
 reorder the array so that identical elements appear together *)

(* Helper function that splits the array into two *)
let two_way_partition arr i j x f =
  while !i <= !j
  do 
    if f arr.(!i) = x  then i := !i + 1
    else 
      begin
	swap arr i j;
	j := !j-1
      end
  done

let id = fun x -> x 

let three_way_partition arr =
  let len = Array.length arr in 
  let gt = ref (len-1)  in
  let lt = ref 0 in
  two_way_partition arr lt gt 0 id;
  gt := (len-1);
  two_way_partition arr lt gt 1 id;
 arr


 (* Variant 2: Assuming that values of the array are in {0,1,2,3},
 reorder the array so that identical elements appear together *)

let four_way_partition arr =
  let len = Array.length arr in 
  let j = ref (len-1)  in
  let i = ref 0 in
  two_way_partition arr i j 0 id;
  j := (len-1);
  two_way_partition arr i j 1 id;
  j := (len-1);
  two_way_partition arr i j 2 id;
 arr


(* Variant 3: Given an array with bool keys like (n,b), reorder so
that elements with false keys appear first *)
let false_first arr =
let j = ref (Array.length arr -1) in
let i = ref 0 in 
two_way_partition arr i j false (fun (x,y) -> y);
arr



(* Variant 4: Given an array with bool keys like (n,b), reorder so
that elements with false keys appear first but elements with true keys
keep their original order *)
let false_first_true_ordered arr =
(* let j = ref (Array.length arr -1) in *)
(* let i = ref 0 in  *)
(* two_way_partition arr i j false (fun (x,y) -> y); *)
(* TODO: reverse the true ordered (I think) *)
arr
