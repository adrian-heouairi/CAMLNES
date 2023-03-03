open Utils;;
open Ppu_constants;;

(* PPUCTRL *)
let get_base_nametable_addr () =
  match Bus.read _PPUCTRL land 0b11 with
    | 0 -> 0x2000
    | 1 -> 0x2400
    | 2 -> 0x2800
    | 3 -> 0x2C00
    | _ -> failwith "Error in get_base_nametable_addr";;

let get_vram_addr_increment () =
  if Bus.read _PPUCTRL land 0b100 = 0 then 1
  else 32;;

let get_sprite_pattern_table_addr () =
  if Bus.read _PPUCTRL land 0b1000 = 0 then 0x0000
  else 0x1000;;

let get_background_pattern_table_addr () =
  if Bus.read _PPUCTRL land 0b1_0000 = 0 then 0x0000
  else 0x1000;;

(* Returns false if sprite size is 8x16 *)
let get_sprite_size_is_8x8 () =
  Bus.read _PPUCTRL land 0b10_0000 = 0;;

let get_master_slave_select () = failwith "Not implemented";;

let get_generate_NMI () =
  Bus.read _PPUCTRL land 0b1000_0000 > 0;;

(* PPUMASK *)
let get_grayscale () =
  nth_bit 0 @@ Bus.read _PPUMASK;;

let get_show_background_at_left () =
  nth_bit 1 @@ Bus.read _PPUMASK;;

let get_show_sprites_at_left () =
  nth_bit 2 @@ Bus.read _PPUMASK;;

let get_show_background () =
  nth_bit 3 @@ Bus.read _PPUMASK;;

let get_show_sprites () =
  nth_bit 4 @@ Bus.read _PPUMASK;;

let get_emphasize_red () =
  nth_bit 5 @@ Bus.read _PPUMASK;;

let get_emphasize_green () =
  nth_bit 6 @@ Bus.read _PPUMASK;;

let get_emphasize_blue () =
  nth_bit 7 @@ Bus.read _PPUMASK;;

(* PPUSTATUS *)
let set_sprite_zero_hit boolean =
  Bus.write _PPUSTATUS @@ set_nth_bit 6 (Bus.read _PPUSTATUS) boolean;;

let set_vblank_started boolean =
  Bus.write _PPUSTATUS @@ set_nth_bit 7 (Bus.read _PPUSTATUS) boolean;;