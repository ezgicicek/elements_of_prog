(* Print a matrix in spiral order *)
 let spiral twoDarr =
  let rec aux twoDarr r c i' j' =
    if r <= 0   then ()
    else 
      begin 
	for j = j' to c
	do print_int (twoDarr.(i').(j))
      	done;
	for i = i'+1 to r
	do print_int (twoDarr.(i).(c))
      	done;
	for j = c-1 downto j'
	do print_int (twoDarr.(r).(j))
      	done;
	for i = r-1 downto i'+1
	do print_int (twoDarr.(i).(j'))
      	done;
	aux twoDarr (r-1) (c-1) (i'+1) (j'+1) 
      end in
  aux twoDarr (Array.length twoDarr -1) (Array.length twoDarr.(0) -1) 0 0
      
