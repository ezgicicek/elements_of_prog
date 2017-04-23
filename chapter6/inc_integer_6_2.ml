(* Given an array of digits encoding a decimal number, increment it by one.  *)
(* Note that overflows are not supported. *)

(* Version 1: use an accumulator *)
let plus_one d =
let en = ref (Array.length d -1) in
let beg = ref 0 in
let acc = ref 1 in
while !beg <= !en && !acc <> 0
do 
  let sum = d.(!en) + !acc  in
  d.(!en) <- sum mod 10;
  acc :=  sum / 10;
  en := !en - 1
done;d


(* Version 2: w/o an accumulator *)
let plus_one d =
let en = ref (Array.length d -1) in
d.(!en) <- d.(!en) + 1;
while !en > 0 && d.(!en) = 10
do 
  d.(!en) <- 0;
  d.(!en -1) <- d.(!en -1) + 1;
  en := !en - 1
done;d


(* Variant 1: add two strings encoding numbers given in a base *)
 let add_nums d p base = 
  let i = ref (String.length d -1)
  and j = ref (String.length p -1)
  and carry = ref 0 in
  let get d i = int_of_char (String.get d i) - int_of_char '0' in
  let res = ref "" in
 while !i >= 0 || !j >= 0 do
    if !i >= 0 then carry := (get d (!i)) + !carry;
    if !j >= 0 then carry := (get p (!j)) + !carry;
    res := string_of_int (!carry mod base) ^ !res;
    carry := !carry /base;
    i := !i -1;
    j := !j -1
 done;
 if !carry = 1 then res := "1" ^ !res;
 !res
