(* Reverse an 8-bit binary *)
let reverse x = 
  let word_size = 2 in
  let bit_mask = 0xFF in
  let preComp = [|0;2;1;3 |] in
  preComp.(x land bit_mask) lsl (3 * word_size) lor
  preComp.((x lsr (3 * word_size)) land bit_mask) lor
  preComp.((x lsr (2 * word_size)) land bit_mask)  lsl  word_size lor

  preComp.((x lsr word_size) land bit_mask)  lsl (2 * word_size)
