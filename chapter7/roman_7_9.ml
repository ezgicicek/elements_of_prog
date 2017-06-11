let get_num r = 
match r with
| 'I' -> 1
| 'V' -> 5
| 'X' -> 10
| 'L' -> 50
| 'C' -> 100
| 'D' -> 500
| 'M' -> 1000
| _ -> failwith "Unknown roman literal"

let roman_to_int r = 
  let size = String.length r in 
  let sum = ref 0 in
  for i = 0 to size -2
  do 
    let num = get_num r.[i] in
    if  num >= get_num r.[i+1]
    then sum := !sum + num
    else sum := !sum - num;
  done;
  if size > 1 then 
    sum := !sum + get_num r.[size-1];
  !sum


(* Variant 1: check validity *)
let is_valid_roman r = 
  let size = String.length r in 
  let res = ref true in
  let valid_cond num nnum = 
    List.fold_left 
      (fun acc x -> (num = x && (nnum = 5 * x || nnum = 10 * x)) || acc) false [1;10;100] in
  for i = 0 to size -2
  do 
    let num = get_num r.[i] in
    let nnum = get_num r.[i+1] in
    if not (valid_cond num nnum) && num < nnum
    then res := false
  done;
  !res

(* Variant 2: convert in to smallest roman number *)
let get_roman n = 
match n with
| 0 -> ""
| 1 -> "I"
| 4 -> "IV"
| 5 -> "V"
| 9 -> "IX"
| 10 -> "X"
| 40 -> "XL"
| 50 -> "L"
| 90 -> "XC"
| 100 -> "C"
| 400 -> "CD"
| 500 -> "D"
| 900 -> "CM"
| 1000 -> "M"
| _ -> failwith "nonvalid int"

let rec int_to_roman n = 
let rec repeat s x = if x = 0 then "" else s ^ repeat s (x-1) in
List.fold_left 
  (fun acc x -> 
   if n /x > 0 then repeat (get_roman x) (n /x) ^ int_to_roman (n mod x)
   else acc) "" [1;4;5;9;10;40;50;90;100;400;500;900;1000]

