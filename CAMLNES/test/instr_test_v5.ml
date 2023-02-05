open CAMLNES;;

print_endline "===== CPU test instr_test_v5:";;

let tests = [| "01-basics"; "02-implied"; "03-immediate"; "04-zero_page"; "05-zp_xy";
  "06-absolute"; "07-abs_xy"; "08-ind_x"; "09-ind_y"; "10-branches"; "11-stack";
  "12-jmp_jsr"; "13-rts"; "14-rti"; "15-brk"; "16-special"; |];;

let rec get_message sofar =
  let char = char_of_int @@ Bus.read (0x6004 + List.length sofar) in
  let char = if char = '\n' then ' ' else char in
  (*print_char char;*)
  if char = '\000' then sofar
  else get_message (sofar @ [char]);;

exception Break_loop;;

for test_number = 0 to Array.length tests - 1 do
  print_string (tests.(test_number) ^ ": ");

  (* Generate a log for a specific test *)
  (*Cpu.disable_logging ();
  (if tests.(test_number) = "15-brk" then Cpu.enable_logging @@ tests.(test_number) ^ "-cpu.log");*)

  Init.init ("../../../test/instr_test_v5/" ^ tests.(test_number) ^ ".nes");

  Bus.write 0x6000 0x80;

  ( try
    for i = 1 to 10000000 do
      (*printf "%d:%02X " i @@ Bus.read 0x6001;*)
      let _ = i in
      Cpu.run_next_instruction ();
      if Bus.read 0x6000 <> 0x80 then raise Break_loop
    done
  with
    Break_loop ->
      Printf.printf "Test ended with result code: 0x%02X (%s); " (Bus.read 0x6000)
      (if Bus.read 0x6000 = 0 then "Success" else "Failure");
    | exc ->
      print_string ("Exception occurred during test: " ^ Printexc.to_string exc ^ "; ");
  );

  (if Bus.read 0x6000 = 0x80 then print_string "Test didn't end; ");

  (*print_endline @@ Int.to_string @@ Bus.read 0x6008*)

  print_string "Message: ";
  List.iter (Printf.printf "%c") (get_message []);
  (* Alternative: print_endline @@ String.of_seq @@ List.to_seq @@ get_message [];; *)
  print_newline ();
  
done
