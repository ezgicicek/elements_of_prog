let zero = int_of_char '0'

let rec int_of_string s = 
  let last = String.length s -1 in 
  if s = "" then 0
  else (int_of_char (s.[last]) - zero) + 10 * int_of_string (String.sub s 0 last)


let rec string_of_int n =
if n < 10 then String.make 1 (char_of_int (n + zero))
else string_of_int (n/10) ^ string_of_int (n mod 10)
