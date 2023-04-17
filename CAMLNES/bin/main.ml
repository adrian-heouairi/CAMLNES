(* main.ml is inspired from https://github.com/DuoSRX/ocamnes/blob/master/bin/main.ml *)

open CAMLNES;;

open Tsdl;;

let sdl_wrapper arg = match arg with
  | Ok x -> x
  | Error (`Msg msg) -> print_endline ("SDL error: " ^ msg); exit 1;;

sdl_wrapper @@ Sdl.init Sdl.Init.video;;
let window = sdl_wrapper @@ Sdl.create_window ~w:256 ~h:240 "CAMLNES" Sdl.Window.(opengl + shown);;
let renderer = sdl_wrapper @@
  Sdl.create_renderer window ~flags:Sdl.Renderer.accelerated;;
let texture = sdl_wrapper @@
  Sdl.create_texture renderer Sdl.Pixel.format_rgb24 Sdl.Texture.access_streaming ~w:256 ~h:240;;

(*Cpu.enable_logging "cpu-main.log";;*)
Init.init Sys.argv.(1);

while true do
  Printf.printf "%d %d\n%!" Ppu.draw.screen.(0).(0) Ppu.draw.screen.(0).(1);
  (* VBLank start *)
  for _ = 1 to 700 do Cpu.run_next_instruction () done;
  (* VBLank end *)
  for i = 1 to 61440 do (* There are 256 * 240 = 61440 pixels *)
    Ppu.draw_next_pixel ();
    if i mod 8 = 0 then Cpu.run_next_instruction ()
  done;
  
  Unix.sleepf (1.0 /. 60.0);
  
  sdl_wrapper @@ Sdl.update_texture texture None Ppu.draw.bigarray (256 * 3);
  sdl_wrapper @@ Sdl.render_clear renderer;
  sdl_wrapper @@ Sdl.render_copy renderer texture;
  Sdl.render_present renderer
done
