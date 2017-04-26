(* Buy and sell a stock once so that profit is maximized *)
let stock arr =
let maxPsofar = ref 0 in
let min = ref max_int in
for i = 0 to Array.length arr -1
do
  if arr.(i) < !min then min := arr.(i);
  maxPsofar := max (arr.(i) - !min) !maxPsofar
done;
!maxPsofar

(* Version 1: Brute force solution O(n^2), depicted only for completeness *)
let stock arr =
let maxP = ref 0 in
for sell = 0 to Array.length arr -1
do
  for buy = 0 to sell
  do 
    maxP:= max (arr.(sell) - arr.(buy)) !maxP
  done
done;
!maxP
