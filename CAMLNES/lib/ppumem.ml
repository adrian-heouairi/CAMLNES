let memory = Array.make 0x4000 0

(* TODO Implement mirroring for reads and writes to addresses >= 0x4000 *)
let read addr = memory.(addr)

let write addr byte =
  assert (0 <= byte && byte <= 255);
  memory.(addr) <- byte

let _OAM = Array.make 256 0

(* Sprite memory *)
let _OAM_read addr = _OAM.(addr)

let _OAM_write addr byte =
  assert (0 <= byte && byte <= 255);
  _OAM.(addr) <- byte
