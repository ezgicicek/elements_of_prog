open Max_stack_sig

module Max_stack :STACK =
  struct
    type 'a stack = 'a list * ('a * int) list

    let empty () = [], []

    let push x (st, max_st)  = 
    match st, max_st with
      | [], [] -> [x], [(x, 1)]
      | _, (lmax, n)::tl -> 
	 if x = lmax then x::st, (x, n+1)::tl 
	 else if x > lmax then x::st, (x,1)::max_st
	 else x::st, max_st
      | _, [] -> failwith "Invariant breaks"

    let pop (st, max_st)  =
      match st, max_st with
      | [], [] -> None
      | h::tl, (lmax, n)::tl' when h = lmax ->
    	 if n = 1 then Some (tl,tl') else Some (tl, (lmax, n-1):: tl')
      | h::tl, (lmax, n)::_ -> Some (tl, max_st)
      | [], _
      | _, [] -> failwith "Invariant breaks"

    let peek (st,_) =
      match st with
      | [] -> None
      | h::_ -> Some h
			   
    let max (_, max_st) =
    match max_st with
      | [] -> None
      | (h,_)::_ -> Some h
						           
  end
