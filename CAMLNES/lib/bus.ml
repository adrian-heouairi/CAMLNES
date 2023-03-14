open Ppu_constants

let bus = Array.make 65536 0
let vram_addr = ref (-1)
let scroll_pos = ref (-1)

let read_raw addr = bus.(addr)

let write_raw addr byte =
  assert (0 <= byte && byte <= 255);
  bus.(addr) <- byte

let get_vram_addr_increment () =
  if read_raw _PPUCTRL land 0b100 = 0 then 1 else 32

let read addr =
  let ret = ref (-1) in
  let real_addr = ref addr in
  if 0x2000 <= addr && addr <= 0x3FFF then
    real_addr := addr mod 8 + 0x2000;

  (* TODO Implement OAMDATA reads *)
  if !real_addr = _PPUSTATUS then (vram_addr := -1; scroll_pos := -1);
  if !real_addr = _PPUDATA then (
    ret := Ppumem.read !vram_addr;
    vram_addr := (!vram_addr + (get_vram_addr_increment ())) mod 0x4000
  );

  if !ret <> -1 then !ret else bus.(!real_addr)

let do_OAMDMA msb =
  let bus_start_addr = msb lsl 8 in
  let offset = read_raw _OAMADDR in
  for i = 0 to 255 do
    Ppumem._OAM_write ((i + offset) mod 256) (read_raw (bus_start_addr + i))
  done

let write addr byte =
  assert (0 <= byte && byte <= 255);

  let real_addr = ref addr in
  if 0x2000 <= addr && addr <= 0x3FFF then
    real_addr := addr mod 8 + 0x2000;

  (* TODO Implement OAMDATA writes *)
  if !real_addr = _PPUSCROLL then
    (if !scroll_pos = -1 then scroll_pos := byte lsl 8
    else scroll_pos := !scroll_pos + byte);
  if !real_addr = _PPUADDR then
    (if !vram_addr = -1 then vram_addr := byte lsl 8
    else vram_addr := !vram_addr + byte);
  if !real_addr = _PPUDATA then (
    Ppumem.write !vram_addr byte;
    vram_addr := (!vram_addr + (get_vram_addr_increment ())) mod 0x4000
  );
  if !real_addr = _OAMDMA then do_OAMDMA byte;

  bus.(!real_addr) <- byte
