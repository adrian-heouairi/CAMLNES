let memory = Array.make 0x4000 0;;

let read addr = memory.(addr);;

let write addr byte =
  assert (0 <= byte && byte <= 255);
  memory.(addr) <- byte;;

(* These addresses refer to CPU memory space *)
let _PPUCTRL = 0x2000;;
let _PPUMASK = 0x2001;;
let _PPUSTATUS = 0x2002;;
let _OAMADDR = 0x2003;;
let _OAMDATA = 0x2004;;
let _PPUSCROLL = 0x2005;;
let _PPUADDR = 0x2006;;
let _PPUDATA = 0x2007;;
let _OAMDMA = 0x4014;;

(* PPUCTRL *)
let get_base_nametable_addr () =
  match Bus.read _PPUCTRL land 0b11 with
    | 1 -> 0x2000
    | 2 -> 0x2400
    | 3 -> 0x2800
    | 4 -> 0x2C00
    | _ -> failwith "Error in base_nametable_addr";;

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