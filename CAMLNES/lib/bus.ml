(** Represents the CPU memory space, as an array. As the PPU registers are in this space,
    this module also contains the current state of the PPU. Mirroring is applied.
    This module contains special behavior when writing to special addresses.
     *)

open Ppu_constants

let bus = Array.make 65536 0
let controller1 = Array.make 8 0
let controller1_read_counter = ref 0

type _PPU_state = {
  mutable vram_addr : int;
  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable write_toggle_w : bool;
  mutable _PPUDATA_read_buffer : int;
}
(** Represents the internal variables of the PPU. *)

let _PPU_state =
  {
    vram_addr = 0;
    scroll_x = 0;
    scroll_y = 0;
    write_toggle_w = false;
    _PPUDATA_read_buffer = 0;
  }

let reset_PPU_state () =
  _PPU_state.vram_addr <- 0;
  _PPU_state.scroll_x <- 0;
  _PPU_state.scroll_y <- 0;
  _PPU_state.write_toggle_w <- false;
  _PPU_state._PPUDATA_read_buffer <- 0

(** Applies mirroring of addresses
    @param addr the address
    @return the real address
*)
let resolve_mirror addr =
  let ret = ref addr in
  if 0x0800 <= addr && addr <= 0x1FFF then ret := addr mod 0x800;
  if 0x2008 <= addr && addr <= 0x3FFF then ret := (addr mod 8) + 0x2000;
  !ret

(** Reads a byte from memory, with mirroring, but without triggering
    special behavior when reading a special address
    *)
let read_raw addr =
  assert (0 <= addr && addr <= 65535);
  bus.(resolve_mirror addr)

(** Writes a byte to memory, with mirroring, but without triggering
    special behavior when writing to a special address
    *)
let write_raw addr byte =
  assert (0 <= addr && addr <= 65535);
  assert (0 <= byte && byte <= 255);
  bus.(resolve_mirror addr) <- byte

(** Sets bit `bit` (0 is LSB) of the byte at address `addr` to `boolean` with mirroring
    and without triggering special behavior *)
let set_nth_bit_raw addr bit boolean =
  assert (0 <= addr && addr <= 65535);
  assert (0 <= bit && bit <= 7);
  bus.(resolve_mirror addr) <-
    Utils.set_nth_bit bit bus.(resolve_mirror addr) boolean

(** Returns bit `bit` (0 is LSB) of the byte at address `addr` *)
let get_nth_bit_raw addr bit =
  assert (0 <= addr && addr <= 65535);
  assert (0 <= bit && bit <= 7);
  Utils.nth_bit bit bus.(resolve_mirror addr)

(** Returns the amount by which the VRAM address should be incremented after
    each read or write *)
let get_vram_addr_increment () =
  if read_raw _PPUCTRL land 0b100 = 0 then 1 else 32

(** Reads a byte from memory with mirroring and special behavior for PPU registers, etc. *)
let read addr =
  assert (0 <= addr && addr <= 65535);
  let ret = ref (-1) in
  let real_addr = resolve_mirror addr in

  (* TODO Implement OAMDATA reads *)
  if real_addr = _PPUSTATUS then (
    _PPU_state.write_toggle_w <- false;
    ret := bus.(real_addr);
    write_raw real_addr (Utils.set_nth_bit 7 !ret false));
  if real_addr = _PPUDATA then (
    if _PPU_state.vram_addr <= 0x3EFF then
      ret := _PPU_state._PPUDATA_read_buffer
    else ret := Ppumem.read _PPU_state.vram_addr;
    _PPU_state._PPUDATA_read_buffer <- Ppumem.read _PPU_state.vram_addr;
    _PPU_state.vram_addr <-
      (_PPU_state.vram_addr + get_vram_addr_increment ()) mod 0x4000);

  if real_addr = 0x4016 then (
    if !controller1_read_counter >= 8 then ret := 1
    else ret := controller1.(!controller1_read_counter);
    ret := !ret lor 0x40;
    (* TODO do this better *)
    controller1_read_counter := !controller1_read_counter + 1);
  if real_addr = 0x4017 then ret := 0x40;

  if !ret <> -1 then !ret else bus.(real_addr)

(** Launched when OAMDMA (copy of memory zone to sprite memory) is triggered *)
let do_OAMDMA msb =
  let bus_start_addr = msb lsl 8 in
  let offset = read_raw _OAMADDR in
  for i = 0 to 255 do
    Ppumem._OAM_write ((i + offset) mod 256) (read_raw (bus_start_addr + i))
  done

(** Writes a byte in the CPU address space, with mirroring and special behavior
    for PPU registers, etc. *)
let write addr byte =
  assert (0 <= addr && addr <= 65535);
  assert (0 <= byte && byte <= 255);

  let real_addr = resolve_mirror addr in

  (* TODO Implement OAMDATA writes *)
  if real_addr = _PPUSCROLL then (
    if not _PPU_state.write_toggle_w then _PPU_state.scroll_x <- byte
    else _PPU_state.scroll_y <- byte;
    _PPU_state.write_toggle_w <- not _PPU_state.write_toggle_w);
  if real_addr = _PPUADDR then (
    if not _PPU_state.write_toggle_w then (
      _PPU_state.vram_addr <- (byte lsl 8) mod 0x4000;
      set_nth_bit_raw _PPUCTRL 1 (Utils.nth_bit 3 byte);
      set_nth_bit_raw _PPUCTRL 0 (Utils.nth_bit 2 byte))
    else _PPU_state.vram_addr <- (_PPU_state.vram_addr + byte) mod 0x4000;
    _PPU_state.write_toggle_w <- not _PPU_state.write_toggle_w);
  if real_addr = _PPUDATA then (
    Ppumem.write _PPU_state.vram_addr byte;
    _PPU_state.vram_addr <-
      (_PPU_state.vram_addr + get_vram_addr_increment ()) mod 0x4000);
  if real_addr = _OAMDMA then do_OAMDMA byte;

  if real_addr = 0x4016 && byte land 1 = 0 then controller1_read_counter := 0;

  bus.(real_addr) <- byte
