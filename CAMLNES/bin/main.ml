open CAMLNES;;

(*open Tsdl;;*)

Cpu.enable_logging "cpu-main.log";;
Init.init Sys.argv.(1);

while true do
  (* VBLank start *)
  for _ = 1 to 700 do Cpu.run_next_instruction () done;
  (* VBLank end *)
  for i = 1 to 61440 do (* There are 256 * 240 = 61440 pixels *)
    Ppu.draw_next_pixel ();
    if i mod 8 = 0 then Cpu.run_next_instruction ()
  done;
  Unix.sleepf (30.0 /. 60.0);
  Printf.printf "%d %d\n%!" Ppu.draw.screen.(0).(0) Ppu.draw.screen.(0).(1);
done
