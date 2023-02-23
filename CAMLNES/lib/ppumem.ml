let memory = Array.make 0x4000 0;;

let read addr = memory.(addr);;

let write addr byte =
  assert (0 <= byte && byte <= 255);
  memory.(addr) <- byte;;


let oam = Array.make 256 0;; (* Sprite memory *)
let oam_read addr = oam.(addr);;
let oam_write addr byte =
  assert (0 <= byte && byte <= 255);
  oam.(addr) <- byte;;