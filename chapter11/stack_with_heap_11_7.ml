open Core.Std

module Stack = 
struct
  type 'a stack = 'a Fheap.t

  let empty = Fheap.create (fun (x,y) (x',y') -> compare y' y)

  let push st x =
    if Fheap.is_empty st then Fheap.add st (x, 0)
    else let (_, ts) =Fheap.top_exn st in
	 Fheap.add st (x, ts+1)

  let pop st = Fheap.pop_exn st


  let peek st = Fheap.top_exn st
end


module Queue = 
struct
  type 'a queue = 'a Fheap.t

  let empty = Fheap.create (fun (x,y) (x',y') -> compare y y')

  let push st x =
    if Fheap.is_empty st then Fheap.add st (x, 0)
    else let (_, ts) =Fheap.top_exn st in
	 Fheap.add st (x, ts+1)

  let pop st = Fheap.pop_exn st


  let peek st = Fheap.top_exn st
end
