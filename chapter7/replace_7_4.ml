let remove_bs arr size = 
  let itr = ref 0 in
  for j = 0 to size-1
  do
    if arr.(j) <> 'b' then (arr.(!itr) <- arr.(j); itr := !itr+1)
  done;
  itr := !itr -1;
  (arr, itr)

let replace arr size =
let f = Array.length arr in
let (arr, itr) = remove_bs arr size in
let j = ref (f-1) in
while !itr >= 0
  do
    if arr.(!itr) = 'a'
    then
      begin
	arr.(!j) <- 'd';
	arr.(!j -1) <- 'd';
	j := !j -1;
      end
    else 
	arr.(!j) <- arr.(!itr);
    itr := !itr - 1;
    j := !j -1;	
done;arr

