let init game_path =
  for i = 0 to 65535 do Bus.write i 0 done;
  Cartridge.parse_nes_file game_path;
  Cartridge.insert_PRG ();
  Cpu.state.program_counter <- Bus.read 0xFFFC + Bus.read 0xFFFD * 256;;

  (* Prepare PPU here *)
