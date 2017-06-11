module type STACK =
  sig
    type 'a stack 
    val empty: unit -> 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val pop  : 'a stack -> 'a stack option
    val peek  : 'a stack -> 'a option
    val max  : 'a stack -> 'a option
end

