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
  screen : int array array;
  fg : int array array;
  bigarray : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let draw = {
  x = 0;
  y = 0;
  screen = Array.make_matrix 240 256 0;
  fg = Array.make_matrix 240 256 (-1);
  bigarray = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (256 * 240 * 3)
}

let colors = [| 0x747474; 0x24188c; 0x0000a8; 0x44009c; 0x8c0074; 0xa80010;
0xa40000; 0x7c0800; 0x402c00; 0x004400; 0x005000; 0x003c14; 0x183c5c; 0x000000;
0x000000; 0x000000; 0xbcbcbc; 0x0070ec; 0x2038ec; 0x8000f0; 0xbc00bc; 0xe40058;
0xd82800; 0xc84c0c; 0x887000; 0x009400; 0x00a800; 0x009038; 0x008088; 0x000000;
0x000000; 0x000000; 0xfcfcfc; 0x3cbcfc; 0x5c94fc; 0xcc88fc; 0xf478fc; 0xfc74b4;
0xfc7460; 0xfc9838; 0xf0bc3c; 0x80d010; 0x4cdc48; 0x58f898; 0x00e8d8; 0x787878;
0x000000; 0x000000; 0xfcfcfc; 0xa8e4fc; 0xc4d4fc; 0xd4c8fc; 0xfcc4fc; 0xfcc4d8;
0xfcbcb0; 0xfcd8a8; 0xfce4a0; 0xe0fca0; 0xa8f0bc; 0xb0fccc; 0x9cfcf0; 0xc4c4c4;
0x000000; 0x000000; |]

let get_CHR_tile table_addr number =
  let start = table_addr + number * 16 in
  let tile = Array.make_matrix 8 8 0 in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let bit1 = nth_bit (7 - j) (Ppumem.read (start + i)) in
      let bit2 = nth_bit (7 - j) (Ppumem.read (start + 8 + i)) in
      let res =
        if (not bit1) && not bit2 then 0
        else if bit1 && not bit2 then 1
        else if (not bit1) && bit2 then 2
        else 3
      in tile.(i).(j) <- res
    done;
  done;

  tile

let get_CHR_tile_colors sprite_palette palette_number table_addr number =
  let palette_start = if sprite_palette then 0x3F10 else 0x3F00 in
  let tile = get_CHR_tile table_addr number in
  let tile_colors = Array.make_matrix 8 8 0x1D in
  for i = 0 to 7 do
    for j = 0 to 7 do
      if tile.(i).(j) = 0 && sprite_palette then tile_colors.(i).(j) <- -1
      else if tile.(i).(j) = 0 then tile_colors.(i).(j) <- Ppumem.read 0x3F00
      else tile_colors.(i).(j) <-
        Ppumem.read (palette_start + palette_number * 4 + tile.(i).(j))
    done
  done;

  tile_colors

let render_sprites () =
  for i = 0 to 239 do
    for j = 0 to 255 do
      draw.fg.(i).(j) <- -1
    done
  done;

  for i = 63 downto 0 do
    let y_pos =
      if Ppumem._OAM_read (i * 4) = 255 then 255 else (Ppumem._OAM_read (i * 4)) + 1 in
    let tile_number = Ppumem._OAM_read (i * 4 + 1) in
    let palette_number = Ppumem._OAM_read (i * 4 + 2) land 0b11 in
    let x_pos = Ppumem._OAM_read (i * 4 + 3) in

    let tile_colors = get_CHR_tile_colors
      true palette_number (get_sprite_pattern_table_addr ()) tile_number in

    for m = 0 to 7 do
      for n = 0 to 7 do
        try
          draw.fg.(y_pos + m).(x_pos + n) <- tile_colors.(m).(n)
        with _ -> ()
      done
    done

  done

let screen_to_bigarray () =
  for y = 0 to 239 do
    for x = 0 to 255 do
      let rgb_color = colors.(draw.screen.(y).(x)) in
      draw.bigarray.{(y * 256 + x) * 3} <- rgb_color lsr 16;
      draw.bigarray.{(y * 256 + x) * 3 + 1} <- rgb_color lsr 8;
      draw.bigarray.{(y * 256 + x) * 3 + 2} <- rgb_color
    done
  done

let draw_next_pixel () =
  if draw.x = 0 && draw.y = 0 then (
    set_vblank_started false;

    render_sprites ()
  );

  if draw.fg.(draw.y).(draw.x) <> -1 then draw.screen.(draw.y).(draw.x) <- draw.fg.(draw.y).(draw.x)
  else draw.screen.(draw.y).(draw.x) <- 0x1D; (* 0x1D is black *)

  if draw.x = 255 && draw.y = 239 then (
    screen_to_bigarray ();

    set_vblank_started true;
    if get_generate_NMI () then Cpu.state.nmi <- true
  );
    
  draw.x <- (draw.x + 1) mod 256;
  if draw.x = 0 then draw.y <- (draw.y + 1) mod 240
