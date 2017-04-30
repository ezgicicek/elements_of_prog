(* Check whether a 9x9 sudoku is valid *)

(* Check columns for duplicate *)
let col_check matrix i j r c = 
  let res = ref false in
  let j = ref j in
  while not !res && !j < c
  do 
    let nums =  (Array.make 10 0) in
    for i = i to r-1
    do
      let x = matrix.(i).(!j) in
      if x = 0 then ()
      else if nums.(x) = 0 then nums.(x) <- (-1)
      else res := true
    done;
    j := !j + 1
  done;
  !res

(* Check rows for duplicate *)
let row_check matrix i j r c = 
  let res = ref false in
  let i = ref i in
  while not !res && !i < r
  do 
    let nums =  (Array.make 10 0) in
    for j = j to c-1
    do
      let x = matrix.(!i).(j) in
      if x = 0 then ()
      else if nums.(x) = 0 then nums.(x) <- (-1)
      else res := true
    done;
    i := !i + 1
  done;
  !res

(* Check 3x3 submatrix for duplicate *)
let sub_matrix_check matrix i' j =
  let res = ref false in
  let i = ref i' in
  let nums =  (Array.make 10 0) in
  while not !res && !i < i' + 3
  do 
    for j = j to j+2
    do
      let x = matrix.(!i).(j) in
      Printf.printf "i:%d j:%d x:%d\n" !i j x;
      if x = 0 then ()
      else if nums.(x) = 0 then nums.(x) <- (-1)
      else res := true
    done;
    i := !i + 1
  done;
  !res


let sudoku_check matrix =
  let res = ref false in
  let i = ref 0 in
  while not !res && !i < 9
  do 
  let j = ref 0 in
    while  not !res && !j < 9
    do
      Printf.printf "i:%d j:%d \n" !i !j;
      res := sub_matrix_check matrix !i !j;
      j := !j + 3
    done;
    i := !i + 3
  done;
  !res ||  col_check matrix 0 0 9 9
 ||  row_check matrix 0 0 9 9
	       
