(* Get the parity bit of an int *)
let parity n = 
let rec aux n acc =
if n == 0 then acc
else aux (n lsr 1) (acc lxor (n land 1))
in aux n 0 


(* Observation: parity (XY) = parity (X XOR Y) *)

(* Version 0: cached version for an 8-bit binary *)
let parity n = 
  let word_size = 2 in
  let bit_mask = 0xFF in
  let preComp = [|0;1;1;0|] in
  preComp.((n lsr (3 * word_size)) land bit_mask) lxor
  preComp.((n lsr (2 * word_size)) land bit_mask) lxor
  preComp.((n lsr (word_size)) land bit_mask) lxor
  preComp.(n land 0x3) 


(* Version 1: process multiple bits at a time *)
let parity n = 
let x = ref n in
x := !x lxor (!x lsr 32);
x := !x lxor (!x lsr 16);
x := !x lxor (!x lsr  8);
x := !x lxor (!x lsr  4);
x := !x lxor (!x lsr  2);
x := !x lxor (!x lsr  1);
!x land 1

(* Version 2: recursive version of 1 *)
let parity n = 
let x = ref n in
let rec repeat x n =
  if n = 1 then ()
  else 
    begin
      x := !x lxor (!x lsr (n/2));
      repeat x (n/2)
    end in
repeat x 64;
!x land 1

(* Version 3: iterative version *)
let parity n =
  let result = ref 0 in
  let x = ref n in 
  while !x <> 0
  do
    result := !result lxor (!x land 1);
    x := !x lsr 1
done;
!result
