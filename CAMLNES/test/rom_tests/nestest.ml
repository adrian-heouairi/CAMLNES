open CAMLNES;;

(*print_endline "===== CPU test nestest:";;*)

Cpu.enable_logging "cpu-nestest.log";;
Init.init "../../resource/test_roms/nestest/nestest.nes";;
Cpu.state.program_counter <- 0xC000;;

try
  for _ = 1 to 5003 do
    Cpu.run_next_instruction ()
  done
with exc ->
  print_endline
    ("Exception occurred during test nestest: " ^ Printexc.to_string exc)

(* TODO: Support Windows *)
let return_code =
  Sys.command
    "diff ../../resource/cpu_logs/good-nestest.log cpu-nestest.log > \
     cpu-nestest-diff.log"

let%test "nestest" = return_code = 0;;

Cpu.disable_logging ()
