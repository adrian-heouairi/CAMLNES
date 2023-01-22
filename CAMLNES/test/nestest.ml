open CAMLNES;;

print_endline "===== CPU test nestest:";;

Cpu.enable_logging "nestest-cpu.log";;

Init.init "../../../test/nestest/nestest.nes";;

Cpu.state.program_counter <- 0xC000;;

try
  for i = 1 to 5003 do
    let _ = i in
    Cpu.run_next_instruction ()
  done
with exc -> print_endline ("Exception occurred during test: " ^ Printexc.to_string exc);;

(* TODO: Support Windows *)
(* This will print the line number where differences start to appear *)
Sys.command ("diff ../../../test/nestest/nestest-cpu-good.log nestest-cpu.log " ^
  "> nestest-cpu-diff.log; [ $? = 0 ] && echo Test passed || head -2 nestest-cpu-diff.log")
