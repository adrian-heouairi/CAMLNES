let memory = Array.make 0x4000 0

let read_raw addr =
  assert (0 <= addr && addr < 0x4000);
  memory.(addr)

let write_raw addr byte =
  assert (0 <= addr && addr < 0x4000);
  assert (0 <= byte && byte <= 255);
  memory.(addr) <- byte

let resolve_mirror addr =
  let ret = ref addr in

  (* First translation, after this use ret instead of addr *)
  if 0x3000 <= addr && addr <= 0x3EFF then ret := addr - 0x1000;
  if 0x3F20 <= addr && addr <= 0x3FFF then ret := addr mod 32 + 0x3F00;

  (* Nametable mirroring (only for mapper 0)*)
  if Cartridge.cartridge.vertical_mirroring then
    (if 0x2800 <= !ret && !ret <= 0x2FFF then ret := !ret - 0x800)
  else
    (if 0x2400 <= !ret && !ret <= 0x27FF then ret := !ret - 0x400;
    if 0x2C00 <= !ret && !ret <= 0x2FFF then ret := !ret - 0x400);

  (* Palette mirroring *)
  if !ret = 0x3F10 then ret := 0x3F00;
  if !ret = 0x3F14 then ret := 0x3F04;
  if !ret = 0x3F18 then ret := 0x3F08;
  if !ret = 0x3F1C then ret := 0x3F0C;

  !ret

let read addr =
  assert (0 <= addr && addr < 0x4000);
  let real_addr = resolve_mirror addr in
  memory.(real_addr)

let write addr byte =
  assert (0 <= addr && addr < 0x4000);
  assert (0 <= byte && byte <= 255);
  let real_addr = resolve_mirror addr in
  memory.(real_addr) <- byte

let _OAM = Array.make 256 0

(* Sprite memory *)
let _OAM_read addr =
  assert (0 <= addr && addr <= 255);
  if addr mod 4 = 2 then _OAM.(addr) land 0xE3
  else _OAM.(addr)

let _OAM_write addr byte =
  assert (0 <= addr && addr <= 255);
  assert (0 <= byte && byte <= 255);
  _OAM.(addr) <- byte
