(* Returns as a boolean bit n of an integer, least significant bit is 0 *)
let nth_bit bit number = number land (1 lsl bit) > 0;;

let set_nth_bit bit number boolean =
  if boolean then
    number lor (1 lsl bit)
  else
    number land (lnot (1 lsl bit));;
  