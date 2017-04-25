let swap arr i j =
let tmp = arr.(i) in
  	 arr.(i) <- arr.(j);
  	 arr.(j) <- tmp

(* Remove duplicates from an array and shift all the remaining
elements to left *)
let dedup arr =
let len = (Array.length arr) in
let j = ref 1 in
for i = 1 to len -1
do
  if arr.(!j-1) <> arr.(i)
  then 
    begin
      arr.(!j) <- arr.(i);
      j := !j + 1
    end
done;

for i = !j to len-1
do
  arr.(i) <- 0
done;
arr

(* Version 1: deduplicate in two passes *)
let dedup arr =
let len = (Array.length arr) in
let i = ref 0 in
(* Mark the first duplicate as 0 *)
while !i < len-1
do
  if arr.(!i) = arr.(!i+1)
  then arr.(!i) <- 0;
  i := !i + 1;
						
done;
(* Collect all 0's to the right *)
i := 0;
let j = ref 0 in
while !j < len
do
  if arr.(!j) = 0
  then j := !j + 1
  else if arr.(!i) = 0
  then 
    begin
      swap arr !i !j;
      j := !j + 1;
      i := !i + 1
    end	
  else begin
      j := !j + 1;
      i := !i + 1
    end
		
done;
arr




(* Variant 1: remove all occurences of x from the array and shift the
rest to left *)
let remove arr x =
let len = (Array.length arr) in
let j = ref 1 in
for i = 1 to len -1
do
  if arr.(i) <> x
  then 
    begin
      arr.(!j) <- arr.(i);
      j := !j + 1
    end
done;

for i = !j to len-1
do
  arr.(i) <- 0
done;
(j,arr)
