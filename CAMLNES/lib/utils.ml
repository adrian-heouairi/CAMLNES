(** Various utils, mostly bit manipulation *)

(** Returns as a boolean bit n of an integer, least significant bit is 0 *)
let nth_bit bit number = number land (1 lsl bit) > 0

(** Sets bit n of an integer
    @param bit (int) The number of the bit to set, 0 being the least significant bit
    @param number (int) The number whose bit will be set
    @param boolean (bool) The value to give to the bit
    @return (int) {!number} with {!bit} set to {!boolean}
    *)

(** Returns `number` with its bit `bit` set to `boolean` *)
let set_nth_bit bit number boolean =
  if boolean then number lor (1 lsl bit) else number land lnot (1 lsl bit)
