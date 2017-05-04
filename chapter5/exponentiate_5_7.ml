(* Recursively compute x^y *)
let rec expo (x: float) (y: int) : float = 
if y = 0 then 1.0 
else if y < 0 then 1.0 /. (expo x (-y))
else if y land 1 = 0 then
  let half = expo x (y lsr 1) in half *. half
else 
    let half = expo x (y lsr 1) in half *. half *. x


(* Version 1: Iteratively compute x^y *)
let expo (x: float) (y: int) : float = 
let result = ref 1.0 in
let power = ref y in
let base = ref x in
if y < 0 then
  (base := 1.0 /. !base;
   power := -(!power));
while !power <> 0 
do
  if !power land 1 = 1 then
    result := !result *. !base;
  base := !base *. !base;
  power := !power lsr 1
done;
!result
   
