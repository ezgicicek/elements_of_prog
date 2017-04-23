(* Given two arrays representing arbitrary precision integers, compute
their product. (Uses grade-school algorithm) *)
let mult_nums p q = 
  let len_p =  Array.length p
  and len_q =  Array.length q
  and carry = ref 0 in
  let res = Array.make (len_p + len_q) 0 in
  for i = len_q-1 downto 0
  do for j = len_p-1 downto 0
     do let mult = p.(j) * q.(i) + !carry + res.(i+j+1)in
	res.(i+j+1) <- mult mod 10;
	carry := mult / 10;
     done;
     res.(i) <- !carry;
     carry := 0
  done; res

 

(* Version 1: without using carry-on bit *)
let mult_nums p q = 
  let len_p = Array.length p
  and len_q = Array.length q  in
  let res = Array.make (len_p + len_q) 0 in
  for i = len_q-1 downto 0
  do for j = len_p-1 downto 0
     do let mult = p.(j) * q.(i) + res.(i+j+1)in
	res.(i+j+1) <- mult mod 10;
	res.(i+j) <- res.(i+j) + mult / 10
     done;
  done; res

 
