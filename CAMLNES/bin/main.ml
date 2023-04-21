(* main.ml is inspired from https://github.com/DuoSRX/ocamnes/blob/master/bin/main.ml *)

open CAMLNES;;

open Tsdl;;

Printexc.record_backtrace true;;

let sdl_wrapper arg = match arg with
  | Ok x -> x
  | Error (`Msg msg) -> print_endline ("SDL error: " ^ msg); exit 1;;

sdl_wrapper @@ Sdl.init Sdl.Init.video;;
let window = sdl_wrapper @@ Sdl.create_window ~w:768 ~h:720 "CAMLNES" Sdl.Window.(opengl + shown + resizable);;
let renderer = sdl_wrapper @@
  Sdl.create_renderer window ~flags:Sdl.Renderer.accelerated;;
let texture = sdl_wrapper @@
  Sdl.create_texture renderer Sdl.Pixel.format_rgb24 Sdl.Texture.access_streaming ~w:256 ~h:240;;

let event = Sdl.Event.create ();;
let key_scancode ev = Sdl.Scancode.enum Sdl.Event.(get ev keyboard_scancode);;
let quit = ref false;;
let pause = ref false;;

(*Cpu.enable_logging "/tmp/cpu-main.log";;*)
Init.init Sys.argv.(1);

Printf.printf "\n%!";;

while not !quit do
  if not !pause then (
    (*Printf.printf "%d %d\n%!" Ppu.draw.screen.(0).(0) Ppu.draw.screen.(0).(1);*)
    (* VBLank start *)
    for _ = 1 to 700 do Cpu.run_next_instruction () done;
    (* VBLank end *)
    for i = 1 to 61440 do (* There are 256 * 240 = 61440 pixels *)
      Ppu.draw_next_pixel ();
      if i mod 8 = 0 then Cpu.run_next_instruction ()
    done;
    
    sdl_wrapper @@ Sdl.update_texture texture None Ppu.draw.bigarray (256 * 3);
    sdl_wrapper @@ Sdl.render_clear renderer;
    sdl_wrapper @@ Sdl.render_copy renderer texture;
    Sdl.render_present renderer;
  );

  Unix.sleepf (0.25 /. 60.0);

  while Sdl.poll_event (Some event) do
    match Sdl.Event.(enum (get event typ)) with
      | `Quit -> quit := true
      | `Key_down when key_scancode event = `Escape -> quit := true
      | `Key_down when key_scancode event = `P -> pause := not !pause
      | `Key_down when key_scancode event = `R -> Init.init Sys.argv.(1)
      | `Key_down when key_scancode event = `O ->
        for i = 0 to 63 do
          Printf.printf "Sprite %d: %d 0x%02X 0x%02X %d %!" i
            (Ppumem._OAM_read (i * 4)) (Ppumem._OAM_read (i * 4 + 1))
            (Ppumem._OAM_read (i * 4 + 2)) (Ppumem._OAM_read (i * 4 + 3))
        done;
        Printf.printf "\n%!"
        | `Key_down when key_scancode event = `S ->
          let tile = Ppu.get_CHR_tile (Ppu.get_sprite_pattern_table_addr ()) (Ppumem._OAM_read 1) false false in
          (*let tile = Ppu.get_CHR_tile 0 0xA2 in*)
          for i = 0 to 7 do
            for j = 0 to 7 do
              if tile.(i).(j) = 0 then print_char '.'
              else print_int tile.(i).(j)
            done;
            print_newline ()
          done

        | `Key_down when key_scancode event = `C ->
          let tile_colors = Ppu.get_CHR_tile_colors true (Ppumem._OAM_read 2 land 0b11) (Ppu.get_sprite_pattern_table_addr ()) (Ppumem._OAM_read 1) false false in
          for i = 0 to 7 do
            for j = 0 to 7 do
              if tile_colors.(i).(j) = -1 then print_string ".. "
              else Printf.printf "%02X " tile_colors.(i).(j)
            done;
            print_newline ()
          done

        | `Key_down when key_scancode event = `V ->
          for i = 0 to 7 do
            let start = 0x3F00 + 4 * i in
            Printf.printf "0x%02X 0x%02X 0x%02X 0x%02X\n%!" (Ppumem.read start)
            (Ppumem.read (start + 1)) (Ppumem.read (start + 2)) (Ppumem.read (start + 3))
          done

      (*| `Key_down when key_scancode e = `Apostrophe -> Debugger.break_on_step := true
      | `Key_up when key_scancode e = `S -> save_screenshot nes.ppu.frame_content
      | `Key_up -> update_input ~down:false (key_scancode e)
      | `Key_down -> update_input ~down:true (key_scancode e)*)
      | _ -> ()
  done;
done
