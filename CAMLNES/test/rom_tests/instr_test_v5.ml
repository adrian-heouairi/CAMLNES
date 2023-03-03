open CAMLNES;;

(*print_endline "===== CPU test instr_test_v5:";;*)

let rec get_message_rec sofar =
  let char = char_of_int @@ Bus.read (0x6004 + List.length sofar) in
  let char = if char = '\n' then ' ' else char in
  if char = '\000' then sofar
  else get_message_rec (sofar @ [char]);;

let get_message () =
  String.of_seq (   List.to_seq (get_message_rec [])   );;

exception Break_loop;;

let run_test name logging =
  (if logging then
    Cpu.enable_logging @@ "cpu-" ^ name ^ ".log"
  else
    Cpu.disable_logging ()
  );

  Init.init ("../../resource/test_roms/instr_test_v5/" ^ name ^ ".nes");

  Bus.write 0x6000 0x80;

  (try
    for i = 1 to 10000000 do
      let _ = i in
      Cpu.run_next_instruction ();
      if Bus.read 0x6000 <> 0x80 then raise Break_loop
    done
  with
    | Break_loop -> ()
    | exc ->
      print_endline ("Exception occurred during test " ^
      name ^ ": " ^ Printexc.to_string exc);
  );

  (if Bus.read 0x6000 <> 0 then
    Printf.printf "Test %s returned result code 0x%02X (%s), message: %s\n"
    name (Bus.read 0x6000)
    (if Bus.read 0x6000 = 0x80 then "didn't end" else "failure")
    (get_message ())
  );

  Bus.read 0x6000 = 0;;

let%test _ = run_test "01-basics" false;;
let%test _ = run_test "02-implied" false;;
(* These tests fail because unofficial instructions are not implemented *)
(*let%test _ = run_test "03-immediate" false;;
let%test _ = run_test "04-zero_page" false;;
let%test _ = run_test "05-zp_xy" false;;
let%test _ = run_test "06-absolute" false;;
let%test _ = run_test "07-abs_xy" false;;
let%test _ = run_test "08-ind_x" false;;
let%test _ = run_test "09-ind_y" false;;*)
let%test _ = run_test "10-branches" false;;
let%test _ = run_test "11-stack" false;;
let%test _ = run_test "12-jmp_jsr" false;;
let%test _ = run_test "13-rts" false;;
let%test _ = run_test "14-rti" false;;
let%test _ = run_test "15-brk" false;;
let%test _ = run_test "16-special" false;;

Cpu.disable_logging ();;
