open Utils
open Ppu_constants

(* PPUCTRL *)
let get_base_nametable_addr () =
  match Bus.read_raw _PPUCTRL land 0b11 with
  | 0 -> 0x2000
  | 1 -> 0x2400
  | 2 -> 0x2800
  | 3 -> 0x2C00
  | _ -> failwith "Error in get_base_nametable_addr"

(*let get_vram_addr_increment () =
  if Bus.read_raw _PPUCTRL land 0b100 = 0 then 1 else 32*)

let get_sprite_pattern_table_addr () =
  if Bus.read_raw _PPUCTRL land 0b1000 = 0 then 0x0000 else 0x1000

let get_background_pattern_table_addr () =
  if Bus.read_raw _PPUCTRL land 0b1_0000 = 0 then 0x0000 else 0x1000

(* Returns false if sprite size is 8x16 *)
let get_sprite_size_is_8x8 () = Bus.read_raw _PPUCTRL land 0b10_0000 = 0
let get_master_slave_select () = failwith "Not implemented"
let get_generate_NMI () = Bus.read_raw _PPUCTRL land 0b1000_0000 > 0

(* PPUMASK *)
let get_grayscale () = nth_bit 0 @@ Bus.read_raw _PPUMASK
let get_show_background_at_left () = nth_bit 1 @@ Bus.read_raw _PPUMASK
let get_show_sprites_at_left () = nth_bit 2 @@ Bus.read_raw _PPUMASK
let get_show_background () = nth_bit 3 @@ Bus.read_raw _PPUMASK
let get_show_sprites () = nth_bit 4 @@ Bus.read_raw _PPUMASK
let get_emphasize_red () = nth_bit 5 @@ Bus.read_raw _PPUMASK
let get_emphasize_green () = nth_bit 6 @@ Bus.read_raw _PPUMASK
let get_emphasize_blue () = nth_bit 7 @@ Bus.read_raw _PPUMASK

(* PPUSTATUS *)
let set_sprite_zero_hit boolean =
  Bus.write_raw _PPUSTATUS @@ set_nth_bit 6 (Bus.read_raw _PPUSTATUS) boolean

let set_vblank_started boolean =
  Bus.write_raw _PPUSTATUS @@ set_nth_bit 7 (Bus.read_raw _PPUSTATUS) boolean

type draw = {
  mutable x : int;
  mutable y : int;
  screen : int array array
}

let draw = {
  x = 0;
  y = 0;
  screen = Array.make 240 (Array.make 256 0)
}

let draw_background_pixel () = draw.screen.(draw.y).(draw.x) <- 1

let draw_foreground_pixel () = ()

let draw_next_pixel () =
  if draw.x = 0 && draw.y = 0 then set_vblank_started false;
  draw_background_pixel ();
  if draw.x = 255 && draw.y = 239 then (
    set_vblank_started true;
    if get_generate_NMI () then Cpu.state.nmi <- true
  );
  draw.x <- (draw.x + 1) mod 256;
  if draw.x = 0 then draw.y <- (draw.y + 1) mod 240
