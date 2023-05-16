open Utils
open Ppu_constants

(* PPUCTRL *)
let get_base_nametable_pixel_offsets () =
  match Bus.read_raw _PPUCTRL land 0b11 with
  | 0 -> (0, 0)
  | 1 -> (256, 0)
  | 2 -> (0, 240)
  | 3 -> (256, 240)
  | _ -> failwith ""

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

let transparent_pixel = 1000

type draw = {
  mutable x : int;
  mutable y : int;
  fg : int array array;
  bg : int array array;
  bigarray : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let draw = {
  x = 0;
  y = 0;
  fg = Array.make_matrix 240 256 transparent_pixel;
  bg = Array.make_matrix 480 512 transparent_pixel;
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

(* Returns a 8x8 array containing constant transparent_pixel or a color between 0 and 63 *)
let write_CHR_tile_colors sprite_palette palette_number table_addr number flip_horz flip_vert
array i_offset j_offset color_transform_fun =
  let palette_start = if sprite_palette then 0x3F10 else 0x3F00 in
  let start = table_addr + number * 16 in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let bit1 = nth_bit (7 - j) (Ppumem.read (start + i)) in
      let bit2 = nth_bit (7 - j) (Ppumem.read (start + 8 + i)) in
      let res =
        if (not bit1) && not bit2 then 0
        else if bit1 && not bit2 then 1
        else if (not bit1) && bit2 then 2
        else 3 in
      
      let color = if res = 0 then transparent_pixel
      else Ppumem.read (palette_start + palette_number * 4 + res) in
      let color = color_transform_fun color in

      let flip_i = if flip_vert then 7 - i else i in
      let flip_j = if flip_horz then 7 - j else j in
      try array.(i_offset + flip_i).(j_offset + flip_j) <- color
    with _ -> ()
    done;
  done

let sprite_color_transform i behind_bg color =
  if color = transparent_pixel then transparent_pixel
  else (
    let ret = ref color in
    if i = 0 then ret := !ret + 64;
    if behind_bg then ret := - !ret;
    !ret
  )

(* Modifies 256x240 draw.fg to contain: constant transparent_pixel for any
   pixel not containing a sprite or containing a sprite which is transparent at
   that pixel. Every other pixel contains a color between 0 and 63. 
   For sprite 0 we do color = color + 64. Then the color
   is set to negative for sprites that must be behind the background. *)
let render_sprites () =
  for i = 0 to 239 do
    for j = 0 to 255 do
      draw.fg.(i).(j) <- transparent_pixel
    done
  done;

  for i = 63 downto 0 do
    let y_pos =
      if Ppumem._OAM_read (i * 4) = 255 then 255 else (Ppumem._OAM_read (i * 4)) + 1 in
    let tile_number = Ppumem._OAM_read (i * 4 + 1) in
    let palette_number = Ppumem._OAM_read (i * 4 + 2) land 0b11 in
    let behind_bg = nth_bit 5 (Ppumem._OAM_read (i * 4 + 2)) in
    let flip_horz = nth_bit 6 (Ppumem._OAM_read (i * 4 + 2)) in
    let flip_vert = nth_bit 7 (Ppumem._OAM_read (i * 4 + 2)) in
    let x_pos = Ppumem._OAM_read (i * 4 + 3) in

    write_CHR_tile_colors
      true palette_number (get_sprite_pattern_table_addr ()) tile_number flip_horz flip_vert
      draw.fg y_pos x_pos (sprite_color_transform i behind_bg)
  done

let render_background () =
  for i = 0 to 29 do
    for j = 0 to 31 do
      let tile_number = Ppumem.read (0x2000 + i * 32 + j) in
      let attr_table_x = j / 4 in
      let attr_table_y = i / 4 in
      let attr_table_byte = Ppumem.read (0x23C0 + 8 * attr_table_y + attr_table_x) in
      let right = j mod 4 >= 2 in
      let bottom = i mod 4 >= 2 in
      let palette = ref 0 in
      (if not right && not bottom then palette := attr_table_byte land 0b11
      else if right && not bottom then palette := (attr_table_byte land 0b1100) lsr 2
      else if not right && bottom then palette := (attr_table_byte land 0b11_0000) lsr 4
      else palette := (attr_table_byte land 0b1100_0000) lsr 6;);

      write_CHR_tile_colors
        false !palette (get_background_pattern_table_addr ()) tile_number false false
        draw.bg (i * 8) (j * 8) Fun.id
    done
  done

let write_to_bigarray color =
  let rgb_color = colors.(color) in
  draw.bigarray.{(draw.y * 256 + draw.x) * 3} <- rgb_color lsr 16;
  draw.bigarray.{(draw.y * 256 + draw.x) * 3 + 1} <- rgb_color lsr 8;
  draw.bigarray.{(draw.y * 256 + draw.x) * 3 + 2} <- rgb_color

let draw_next_pixel () =
  if draw.x = 0 && draw.y = 0 then (
    set_vblank_started false;
    set_sprite_zero_hit false;

    render_sprites ();
    render_background ()
  );

  let (x_offset, y_offset) = get_base_nametable_pixel_offsets () in
  let x_offset = x_offset + Bus._PPU_state.scroll_x in
  let y_offset = y_offset + Bus._PPU_state.scroll_y in

  let bg_px = draw.bg.((y_offset + draw.y) mod 480).((x_offset + draw.x) mod 512) in
  let fg_px = draw.fg.(draw.y).(draw.x) in

  if 64 <= abs fg_px && abs fg_px <= 127 then
    (if bg_px <> transparent_pixel then set_sprite_zero_hit true);

  if fg_px = transparent_pixel && bg_px = transparent_pixel then
    write_to_bigarray (Ppumem.read 0x3F00)
  else if fg_px = transparent_pixel then
    write_to_bigarray bg_px
  else if bg_px = transparent_pixel then
    write_to_bigarray (abs fg_px mod 64)
  else ( (* Both fg and bg pixels are not transparent *)
    if fg_px < 0 then write_to_bigarray bg_px
    else write_to_bigarray (abs fg_px mod 64)
  );



  if draw.x = 255 && draw.y = 239 then (
    set_vblank_started true;
    if get_generate_NMI () then Cpu.state.nmi <- true
  );
    
  draw.x <- (draw.x + 1) mod 256;
  if draw.x = 0 then draw.y <- (draw.y + 1) mod 240
