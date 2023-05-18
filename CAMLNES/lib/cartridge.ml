type cartridge = {
  header : int array;
  mutable number_of_PRG_banks : int;
  mutable number_of_CHR_banks : int;
  _PRG_banks : int array array;
  _CHR_banks : int array array;
  mutable vertical_mirroring : bool;
}

let cartridge =
  {
    header = Array.make 16 0;
    number_of_PRG_banks = -1;
    number_of_CHR_banks = -1;
    _PRG_banks = Array.make 32 [||];
    (* 32 is arbitrary *)
    _CHR_banks = Array.make 32 [||];
    vertical_mirroring = false;
  }

let parse_nes_file path =
  try
    let in_channel = open_in_bin path in
    for i = 0 to 15 do
      cartridge.header.(i) <- input_byte in_channel
    done;

    (* Verify the file follows the iNES format *)
    assert (
      cartridge.header.(0) = 0x4E
      && cartridge.header.(1) = 0x45
      && cartridge.header.(2) = 0x53
      && cartridge.header.(3) = 0x1A);

    cartridge.number_of_PRG_banks <- cartridge.header.(4);
    cartridge.number_of_CHR_banks <- cartridge.header.(5);

    cartridge.vertical_mirroring <- Utils.nth_bit 0 cartridge.header.(6);

    if cartridge.header.(6) land 0b0000_0100 > 0 then
      (* Skip the trainer if there is one *)
      seek_in in_channel (16 + 512);

    (* Get the PRG banks *)
    for i = 0 to cartridge.number_of_PRG_banks - 1 do
      let bank = Array.make 16384 0 in
      for j = 0 to 16383 do
        bank.(j) <- input_byte in_channel
      done;
      cartridge._PRG_banks.(i) <- bank
    done;

    (* Get the CHR banks *)
    for i = 0 to cartridge.number_of_CHR_banks - 1 do
      let bank = Array.make 8192 0 in
      for j = 0 to 8191 do
        bank.(j) <- input_byte in_channel
      done;
      cartridge._CHR_banks.(i) <- bank
    done
  with exc -> raise exc
