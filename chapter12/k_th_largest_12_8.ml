let partition arr left right x = 
let swap x y = 
  let tmp = arr.(x) in 
  arr.(x) <- arr.(y);
  arr.(y) <- tmp in
let pivot = arr.(x) in
let pivId = ref left in
swap (!pivId) right;
for i = left to right
do if arr.(i) < pivot
   then swap i (!pivId);
	pivId := !pivId + 1
done; arr
