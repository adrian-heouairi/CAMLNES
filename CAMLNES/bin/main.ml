(** First we create the required variables for graphical rendering to an SDL window,
    then we launch the main loop of the program that runs the CPU and PPU for one
    frame each iteration. We render to the window at the end of each frame. The
    main loop also includes keyboard input handling. *)

(* main.ml is inspired from https://github.com/DuoSRX/ocamnes/blob/master/bin/main.ml *)

open CAMLNES
open Tsdl;;

Printexc.record_backtrace true

let sdl_wrapper arg =
  match arg with
  | Ok x -> x
  | Error (`Msg msg) ->
      print_endline ("SDL error: " ^ msg);
      exit 1
;;

sdl_wrapper @@ Sdl.init Sdl.Init.video

let window =
  sdl_wrapper
  @@ Sdl.create_window ~w:768 ~h:720 "CAMLNES"
       Sdl.Window.(opengl + shown + resizable)

let renderer =
  sdl_wrapper @@ Sdl.create_renderer window ~flags:Sdl.Renderer.accelerated

let texture =
  sdl_wrapper
  @@ Sdl.create_texture renderer Sdl.Pixel.format_rgb24
       Sdl.Texture.access_streaming ~w:256 ~h:240

let event = Sdl.Event.create ()
let key_scancode ev = Sdl.Scancode.enum Sdl.Event.(get ev keyboard_scancode)
let quit = ref false
let pause = ref false

(** Modifies the keys currently pressed, which are given to the CPU.
    @param keycode the key which has had a down or up event
    @param down 0 if up, 1 if down
    *)
let update_input keycode ~down =
  match keycode with
  | `L -> Bus.controller1.(0) <- down
  | `K -> Bus.controller1.(1) <- down
  | `Backspace -> Bus.controller1.(2) <- down
  | `Return -> Bus.controller1.(3) <- down
  | `W -> Bus.controller1.(4) <- down
  | `Z -> Bus.controller1.(4) <- down
  | `Up -> Bus.controller1.(4) <- down
  | `S -> Bus.controller1.(5) <- down
  | `Down -> Bus.controller1.(5) <- down
  | `A -> Bus.controller1.(6) <- down
  | `Q -> Bus.controller1.(6) <- down
  | `Left -> Bus.controller1.(6) <- down
  | `D -> Bus.controller1.(7) <- down
  | `Right -> Bus.controller1.(7) <- down
  | _ -> ()
;;

(*Cpu.enable_logging "/tmp/cpu-main.log";;*)
Init.init Sys.argv.(1);

Printf.printf "\n%!"
;;

while not !quit do
  if not !pause then (
    let before = Int32.to_int (Sdl.get_ticks ()) in
    (*Printf.printf "%d %d\n%!" Ppu.draw.screen.(0).(0) Ppu.draw.screen.(0).(1);*)
    (* VBLank start *)
    for _ = 1 to 700 do
      Cpu.run_next_instruction ()
    done;
    (* VBLank end *)
    for i = 1 to 61440 do
      (* There are 256 * 240 = 61440 pixels *)
      Ppu.draw_next_pixel ();
      if i mod 8 = 0 then Cpu.run_next_instruction ()
    done;

    sdl_wrapper @@ Sdl.update_texture texture None Ppu.draw.bigarray (256 * 3);
    sdl_wrapper @@ Sdl.render_clear renderer;
    sdl_wrapper @@ Sdl.render_copy renderer texture;
    Sdl.render_present renderer;

    let after = Int32.to_int (Sdl.get_ticks ()) in
    let difference = after - before in
    if difference < 13 then
      Unix.sleepf (Int.to_float (13 - difference) /. 1000.0));

  (*Unix.sleepf (0.25 /. 60.0);*)
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(enum (get event typ)) with
    | `Quit -> quit := true
    | `Key_down when key_scancode event = `Escape -> quit := true
    | `Key_down when key_scancode event = `P -> pause := not !pause
    | `Key_down when key_scancode event = `R -> Init.init Sys.argv.(1)
    | `Key_down when key_scancode event = `Equals ->
        Cpu.enable_logging "/tmp/cpu-main.log"
    | `Key_down when key_scancode event = `O ->
        for i = 0 to 63 do
          Printf.printf "Sprite %d: %d 0x%02X 0x%02X %d %!" i
            (Ppumem._OAM_read (i * 4))
            (Ppumem._OAM_read ((i * 4) + 1))
            (Ppumem._OAM_read ((i * 4) + 2))
            (Ppumem._OAM_read ((i * 4) + 3))
        done;
        Printf.printf "\n%!"
    | `Key_down when key_scancode event = `V ->
        for i = 0 to 7 do
          let start = 0x3F00 + (4 * i) in
          Printf.printf "0x%02X 0x%02X 0x%02X 0x%02X\n%!" (Ppumem.read start)
            (Ppumem.read (start + 1))
            (Ppumem.read (start + 2))
            (Ppumem.read (start + 3))
        done
    | `Key_down when key_scancode event = `B ->
        for i = 0 to 29 do
          for j = 0 to 31 do
            Printf.printf "%02X " (Ppumem.read (0x2000 + (i * 32) + j))
          done;
          Printf.printf "\n%!"
        done
    | `Key_down when key_scancode event = `M ->
        for i = 0 to 7 do
          for j = 0 to 7 do
            let attr_byte = Ppumem.read (0x23C0 + (8 * i) + j) in
            for n = 7 downto 0 do
              if Utils.nth_bit n attr_byte then print_int 1 else print_int 0
            done;
            print_char ' '
          done;
          print_newline ()
        done
    | `Key_up -> update_input ~down:0 (key_scancode event)
    | `Key_down -> update_input ~down:1 (key_scancode event)
    | _ -> ()
  done
done
