type cartridge = {
  header : int array;
  mutable number_of_PRG_banks : int;
  mutable number_of_CHR_banks : int;
  _PRG_banks : int array array;
  _CHR_banks : int array array;
}

let cartridge =
  {
    header = Array.make 16 0;
    number_of_PRG_banks = -1;
    number_of_CHR_banks = -1;
    _PRG_banks = Array.make 32 [||];
    (* 32 is arbitrary *)
    _CHR_banks = Array.make 32 [||];
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

let load_PRG_bank addr _PRG_bank =
  (* Bank is necessarily of size 16384 *)
  for i = addr to addr + 16383 do
    Bus.write i _PRG_bank.(i - addr)
  done

let insert_PRG () =
  if cartridge.number_of_PRG_banks = 1 then (
    load_PRG_bank 0x8000 cartridge._PRG_banks.(0);
    load_PRG_bank 0xC000 cartridge._PRG_banks.(0))
  else if cartridge.number_of_PRG_banks = 2 then (
    load_PRG_bank 0x8000 cartridge._PRG_banks.(0);
    load_PRG_bank 0xC000 cartridge._PRG_banks.(1))
  else failwith "Unsupported cartridge"

let insert_CHR () =
  if cartridge.number_of_CHR_banks = 1 then
    for i = 0 to 8191 do
      Ppumem.write i cartridge._CHR_banks.(0).(i)
    done
  else failwith "Unsupported cartridge"
