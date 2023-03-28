let load_PRG_bank addr _PRG_bank =
  (* Bank is necessarily of size 16384 *)
  for i = addr to addr + 16383 do
    Bus.write_raw i _PRG_bank.(i - addr)
  done

let insert_PRG () =
  if Cartridge.cartridge.number_of_PRG_banks = 1 then (
    load_PRG_bank 0x8000 Cartridge.cartridge._PRG_banks.(0);
    load_PRG_bank 0xC000 Cartridge.cartridge._PRG_banks.(0))
  else if Cartridge.cartridge.number_of_PRG_banks = 2 then (
    load_PRG_bank 0x8000 Cartridge.cartridge._PRG_banks.(0);
    load_PRG_bank 0xC000 Cartridge.cartridge._PRG_banks.(1))
  else failwith "Unsupported cartridge"

let insert_CHR () =
  if Cartridge.cartridge.number_of_CHR_banks = 1 then
    for i = 0 to 8191 do
      Ppumem.write_raw i Cartridge.cartridge._CHR_banks.(0).(i)
    done
  else failwith "Unsupported cartridge"

let init game_path =
  for i = 0 to 65535 do
    Bus.bus.(i) <- 0
  done;
  (* Also disables NMI *)
  Cpu.reset_state ();
  Cartridge.parse_nes_file game_path;
  insert_PRG ();
  Cpu.state.program_counter <- Bus.read 0xFFFC + (Bus.read 0xFFFD * 256);

  (* Prepare PPU here *)
  insert_CHR ()
