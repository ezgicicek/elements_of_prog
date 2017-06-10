let is_palindrome s = 
let remove_nonalpha = Str.global_replace (Str.regexp "[^a-zA-Z]+") "" in
let s = s |> remove_nonalpha |> String.lowercase_ascii in
let size = String.length s in
let i = ref 0 in
let j = ref (size-1) in
let res = ref true in
while !i < !j && !res
do
  if s.[!i] <> s.[!j] then res := false;
  i := !i +1;
  j := !j -1
done;
!res
