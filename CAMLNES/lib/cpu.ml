open Cpu_instructions

type cpu_state = {
  mutable accumulator : int;
  mutable index_register_X : int;
  mutable index_register_Y : int;
  mutable program_counter : int;
  mutable stack_pointer : int;

  (* The status register (P) *)
  mutable carry_flag : bool; (* Bit 0 *)
  mutable zero_flag : bool;
  mutable interrupt_disable_flag : bool;
  mutable decimal_mode_flag : bool;
  (* Bit 4 *)
  (* Bit 5: always 1 *)
  mutable overflow_flag : bool;
  mutable negative_flag : bool; (* Bit 7 *)

  mutable nmi : bool;
  mutable nmi_launched : bool;
};;

let state = {
  accumulator = 0;
  index_register_X = 0;
  index_register_Y = 0;
  program_counter = -1; (* Set this to what is contained in $FFFC in the cartridge *)
  stack_pointer = 0xFD;

  carry_flag = false;
  zero_flag = false;
  interrupt_disable_flag = true;
  decimal_mode_flag = false;
  overflow_flag = false;
  negative_flag = false;

  nmi = false;
  nmi_launched = false;
};;

let reset_state () =
  state.accumulator <- 0;
  state.index_register_X <- 0;
  state.index_register_Y <- 0;
  state.program_counter <- -1;
  state.stack_pointer <- 0xFD;

  state.carry_flag <- false;
  state.zero_flag <- false;
  state.interrupt_disable_flag <- true;
  state.decimal_mode_flag <- false;
  state.overflow_flag <- false;
  state.negative_flag <- false;

  state.nmi <- false;
  state.nmi_launched <- false;;

type cpu_logging = {
  mutable logging : bool;
  mutable log : out_channel
}

let logging = {
  logging = false;
  log = stdout
}

(*let log_filename = Sys.argv.(0) ^ "-cpu.log";;*)

let enable_logging filename =
  logging.logging <- true;
  if filename = "" then
    logging.log <- stdout
  else
    logging.log <- open_out_bin filename;;

let disable_logging () =
  logging.logging <- false;
  logging.log <- stdout;;

let status_to_byte () =
  Bool.to_int state.carry_flag * 1 +
  Bool.to_int state.zero_flag * 2 +
  Bool.to_int state.interrupt_disable_flag * 4 +
  Bool.to_int state.decimal_mode_flag * 8 +
  (* Bit 4 has to be 0 when pushing to stack because of IRQ or NMI and 1 for PHP and BRK *)
  1 * 32 +
  Bool.to_int state.overflow_flag * 64 +
  Bool.to_int state.negative_flag * 128;;

let byte_to_status byte =
  state.carry_flag <- byte land 1 > 0;
  state.zero_flag <- byte land 2 > 0;
  state.interrupt_disable_flag <- byte land 4 > 0;
  state.decimal_mode_flag <- byte land 8 > 0;
  

  state.overflow_flag <- byte land 64 > 0;
  state.negative_flag <- byte land 128 > 0;;

let stack_push byte =
  Bus.write (0x0100 lor state.stack_pointer) byte;
  state.stack_pointer <- state.stack_pointer - 1;
  if state.stack_pointer = -1 then state.stack_pointer <- 255;;

let stack_pull () =
  state.stack_pointer <- (state.stack_pointer + 1) mod 256;
  Bus.read (0x0100 lor state.stack_pointer);;

(* Returns as a boolean bit n of an integer, LSB is 0 *)
let get_nth_bit bit number = number land (1 lsl bit) > 0;;

let set_zero_and_negative_flags byte =
  state.zero_flag <- byte = 0;
  state.negative_flag <- byte > 127;;

let branch byte =
  let offset = if byte < 128 then byte else byte - 256 in
  state.program_counter <- state.program_counter + offset;;

let _ADC (calculated_addr, byte_at_addr) =
  let addition = state.accumulator + byte_at_addr + Bool.to_int state.carry_flag in
  state.carry_flag <- addition > 255;
  state.overflow_flag <- (lnot (state.accumulator lxor byte_at_addr)) land (state.accumulator lxor addition) land 0x80 > 0;
  let addition = addition mod 256 in
  set_zero_and_negative_flags addition;
  state.accumulator <- addition;;

let _AND (calculated_addr, byte_at_addr) =
  state.accumulator <- state.accumulator land byte_at_addr;
  state.zero_flag <- state.accumulator = 0;
  state.negative_flag <- state.accumulator > 127

let _ASL (calculated_addr, byte_at_addr) =
  let value = if calculated_addr = -1 then state.accumulator else byte_at_addr in
  let () = state.carry_flag <- value land 128 > 0 in
  let value2 = (value * 2) land 255 in
  let () = state.zero_flag <- value2 = 0 in
  let () = state.negative_flag <- value2 > 127 in
  if calculated_addr = -1 then state.accumulator <- value2 else Bus.write calculated_addr value2

let _BCC (calculated_addr, byte_at_addr) =
  if not state.carry_flag then branch byte_at_addr;;

let _BCS (calculated_addr, byte_at_addr) = if state.carry_flag then branch byte_at_addr;;

let _BEQ (calculated_addr, byte_at_addr) = if state.zero_flag then branch byte_at_addr;;

let _BIT (calculated_addr, byte_at_addr) =
  let and_result = state.accumulator land byte_at_addr in
  let () = state.zero_flag <- and_result = 0 in
  let () = state.overflow_flag <- byte_at_addr land 64 > 0 in
  state.negative_flag <- get_nth_bit 7 byte_at_addr;;

let _BMI (calculated_addr, byte_at_addr) = if state.negative_flag then branch byte_at_addr;;

let _BNE (calculated_addr, byte_at_addr) = if not state.zero_flag then branch byte_at_addr;;

let _BPL (calculated_addr, byte_at_addr) = if not state.negative_flag then branch byte_at_addr;;

let _BRK (calculated_addr, byte_at_addr) = (* Bit 4 to 1 *)
  (* BRK must work even when the interrupt disable flag is set according to blargg's instr_test_v5 *)
  let pc = (state.program_counter + 2) mod 65536 in
  stack_push (pc lsr 8);
  stack_push (pc land 255);
  stack_push (status_to_byte () lor 0b0011_0000);
  state.interrupt_disable_flag <- true;
  state.program_counter <- Bus.read 0xFFFE + Bus.read 0xFFFF * 256;;

let _BVC (calculated_addr, byte_at_addr) = if not state.overflow_flag then branch byte_at_addr;;

let _BVS (calculated_addr, byte_at_addr) = if state.overflow_flag then branch byte_at_addr;;

let _CLC (calculated_addr, byte_at_addr) = state.carry_flag <- false;;

let _CLD (calculated_addr, byte_at_addr) = state.decimal_mode_flag <- false;;

let _CLI (calculated_addr, byte_at_addr) = state.interrupt_disable_flag <- false;;

let _CLV (calculated_addr, byte_at_addr) = state.overflow_flag <- false;;

let compare byte1 byte2 =
  let result = (byte1 - byte2) land 255 in
  state.carry_flag <- byte1 >= byte2;
  state.zero_flag <- byte1 = byte2;
  state.negative_flag <- result > 127;;

let _CMP (calculated_addr, byte_at_addr) =
  compare state.accumulator byte_at_addr;;

let _CPX (calculated_addr, byte_at_addr) =
  compare state.index_register_X byte_at_addr;;

let _CPY (calculated_addr, byte_at_addr) =
  compare state.index_register_Y byte_at_addr;;

let _DEC (calculated_addr, byte_at_addr) =
  let result = if byte_at_addr = 0 then 255 else byte_at_addr - 1 in
  Bus.write calculated_addr result;
  state.zero_flag <- result = 0;
  state.negative_flag <- get_nth_bit 7 result;;

let _DEX (calculated_addr, byte_at_addr) =
  state.index_register_X <- if state.index_register_X = 0 then 255 else state.index_register_X - 1;
  state.zero_flag <- state.index_register_X = 0;
  state.negative_flag <- get_nth_bit 7 state.index_register_X;;

let _DEY (calculated_addr, byte_at_addr) =
  state.index_register_Y <- if state.index_register_Y = 0 then 255 else state.index_register_Y - 1;
  state.zero_flag <- state.index_register_Y = 0;
  state.negative_flag <- get_nth_bit 7 state.index_register_Y;;

let _EOR (calculated_addr, byte_at_addr) =
  state.accumulator <- state.accumulator lxor byte_at_addr;
  state.zero_flag <- state.accumulator = 0;
  state.negative_flag <- get_nth_bit 7 state.accumulator;;

let _INC (calculated_addr, byte_at_addr) =
  let result = (byte_at_addr + 1) mod 256 in
  Bus.write calculated_addr result;
  state.zero_flag <- result = 0;
  state.negative_flag <- get_nth_bit 7 result;;

let _INX (calculated_addr, byte_at_addr) =
  state.index_register_X <- (state.index_register_X + 1) mod 256;
  state.zero_flag <- state.index_register_X = 0;
  state.negative_flag <- get_nth_bit 7 state.index_register_X;;

let _INY (calculated_addr, byte_at_addr) =
  state.index_register_Y <- (state.index_register_Y + 1) mod 256;
  state.zero_flag <- state.index_register_Y = 0;
  state.negative_flag <- get_nth_bit 7 state.index_register_Y;;

let _JMP (calculated_addr, byte_at_addr) =
  (*Printf.printf "JMP address: %02X %02X " (Bus.read 0x02FF) (Bus.read 0x0200);
  Printf.printf "JMP calculated address: %04X\n" calculated_addr;*)
  state.program_counter <- calculated_addr;;

let _JSR (calculated_addr, byte_at_addr) =
  let pc = if state.program_counter = 0 then 0xFFFF else state.program_counter + 2 in
  stack_push (pc lsr 8);
  stack_push (pc land 255);
  state.program_counter <- calculated_addr;;

let _LDA (calculated_addr, byte_at_addr) =
  state.accumulator <- byte_at_addr;
  state.zero_flag <- byte_at_addr = 0;
  state.negative_flag <- get_nth_bit 7 byte_at_addr;;

let _LDX (calculated_addr, byte_at_addr) =
  state.index_register_X <- byte_at_addr;
  state.zero_flag <- byte_at_addr = 0;
  state.negative_flag <- get_nth_bit 7 byte_at_addr;;

let _LDY (calculated_addr, byte_at_addr) =
  state.index_register_Y <- byte_at_addr;
  state.zero_flag <- byte_at_addr = 0;
  state.negative_flag <- get_nth_bit 7 byte_at_addr;;

let _LSR (calculated_addr, byte_at_addr) =
  state.carry_flag <- get_nth_bit 0 byte_at_addr;
  state.negative_flag <- false;
  let result = byte_at_addr lsr 1 in
  state.zero_flag <- result = 0;
  if calculated_addr = -1 then
    state.accumulator <- result
  else
    Bus.write calculated_addr result;;


let _NOP (calculated_addr, byte_at_addr) = ()

let _ORA (calculated_addr, byte_at_addr) =
  state.accumulator <- state.accumulator lor byte_at_addr;
  state.zero_flag <- state.accumulator = 0;
  state.negative_flag <- get_nth_bit 7 state.accumulator;;

let _PHA (calculated_addr, byte_at_addr) = stack_push state.accumulator;;

let _PHP (calculated_addr, byte_at_addr) = (* Bit 4 to 1 *)
  stack_push @@ status_to_byte () lor 0b0011_0000;;

let _PLA (calculated_addr, byte_at_addr) =
  state.accumulator <- stack_pull ();
  set_zero_and_negative_flags state.accumulator;;

let _PLP (calculated_addr, byte_at_addr) = byte_to_status @@ stack_pull ();;

let _ROL (calculated_addr, byte_at_addr) =
  let result = byte_at_addr lsl 1 land 255 lor Bool.to_int state.carry_flag in
  state.carry_flag <- get_nth_bit 7 byte_at_addr;
  set_zero_and_negative_flags result;
  if calculated_addr = -1 then
    state.accumulator <- result
  else
    Bus.write calculated_addr result;;

let _ROR (calculated_addr, byte_at_addr) =
  let result = byte_at_addr lsr 1 lor (Bool.to_int state.carry_flag * 128) in
  state.carry_flag <- get_nth_bit 0 byte_at_addr;
  set_zero_and_negative_flags result;
  if calculated_addr = -1 then
    state.accumulator <- result
  else
    Bus.write calculated_addr result;;

let _RTI (calculated_addr, byte_at_addr) =
  byte_to_status (stack_pull ());
  let first_byte = stack_pull () in
  state.program_counter <- first_byte + stack_pull () * 256;;

let _RTS (calculated_addr, byte_at_addr) =
  let first_byte = stack_pull () in
  state.program_counter <- (first_byte + stack_pull () * 256) mod 65536;;

let _SBC (calculated_addr, byte_at_addr) =
  _ADC (-1, byte_at_addr lxor 255);;

let _SEC (calculated_addr, byte_at_addr) = state.carry_flag <- true;;

let _SED (calculated_addr, byte_at_addr) = state.decimal_mode_flag <- true;;

let _SEI (calculated_addr, byte_at_addr) = state.interrupt_disable_flag <- true;;

let _STA (calculated_addr, byte_at_addr) = Bus.write calculated_addr state.accumulator;;

let _STX (calculated_addr, byte_at_addr) = Bus.write calculated_addr state.index_register_X;;

let _STY (calculated_addr, byte_at_addr) = Bus.write calculated_addr state.index_register_Y;;

let _TAX (calculated_addr, byte_at_addr) =
  set_zero_and_negative_flags state.accumulator;
  state.index_register_X <- state.accumulator;;

let _TAY (calculated_addr, byte_at_addr) =
  set_zero_and_negative_flags state.accumulator;
  state.index_register_Y <- state.accumulator;;

let _TSX (calculated_addr, byte_at_addr) =
  set_zero_and_negative_flags state.stack_pointer;
  state.index_register_X <- state.stack_pointer;;

let _TXA (calculated_addr, byte_at_addr) =
  set_zero_and_negative_flags state.index_register_X;
  state.accumulator <- state.index_register_X;;

let _TXS (calculated_addr, byte_at_addr) = state.stack_pointer <- state.index_register_X;;

let _TYA (calculated_addr, byte_at_addr) =
  set_zero_and_negative_flags state.index_register_Y;
  state.accumulator <- state.index_register_Y;;

(* TODO: Unimplemented *)
(* Unofficial instructions *)
let _AHX (calculated_addr, byte_at_addr) = ();;
let _ALR (calculated_addr, byte_at_addr) =
  _AND (calculated_addr, byte_at_addr);
  _LSR (-1, state.accumulator);;
let _ANC (calculated_addr, byte_at_addr) =
  state.accumulator <- state.accumulator land byte_at_addr;
  state.negative_flag <- get_nth_bit 7 state.accumulator;
  state.carry_flag <- state.negative_flag;;
let _ARR (calculated_addr, byte_at_addr) =
  _AND (calculated_addr, byte_at_addr);
  state.overflow_flag <- (get_nth_bit 6 state.accumulator) <> (get_nth_bit 7 state.accumulator);
  let carry = get_nth_bit 7 state.accumulator in
  state.accumulator <- state.accumulator lsr 1;
  state.accumulator <- state.accumulator lor ((Bool.to_int state.carry_flag) lsl 7);
  state.carry_flag <- carry;
  set_zero_and_negative_flags state.accumulator;;
let _AXS (calculated_addr, byte_at_addr) =
  let current_accumulator = state.accumulator in
  _TXA (-1, -1);
  _AND (-1, current_accumulator);
  state.carry_flag <- true;
  _SBC (calculated_addr, byte_at_addr);
  _TAX (-1, -1);
  state.accumulator <- current_accumulator;;
let _DCP (calculated_addr, byte_at_addr) = ();;
let _ISC (calculated_addr, byte_at_addr) = ();;
let _LAS (calculated_addr, byte_at_addr) = ();;
let _LAX (calculated_addr, byte_at_addr) =
  _ORA (0, 0xFF); (* 0xEE for some 6502 CPUs, but 0xFF for the NES *)
  _AND (calculated_addr, byte_at_addr);
  _TAX (-1, -1);;
let _RLA (calculated_addr, byte_at_addr) = ();;
let _RRA (calculated_addr, byte_at_addr) = ();;
let _SAX (calculated_addr, byte_at_addr) = ();;
let _SHX (calculated_addr, byte_at_addr) = ();;
let _SHY (calculated_addr, byte_at_addr) = ();;
let _SLO (calculated_addr, byte_at_addr) = ();;
let _SRE (calculated_addr, byte_at_addr) = ();;
let _STP (calculated_addr, byte_at_addr) = ();;
let _TAS (calculated_addr, byte_at_addr) = ();;
let _XAA (calculated_addr, byte_at_addr) = ();;

let get_instruction instruction =
  match instruction with
  ADC -> _ADC
  | AND -> _AND
  | ASL -> _ASL
  | BCC -> _BCC
  | BCS -> _BCS
  | BEQ -> _BEQ
  | BIT -> _BIT
  | BMI -> _BMI
  | BNE -> _BNE
  | BPL -> _BPL
  | BRK -> _BRK
  | BVC -> _BVC
  | BVS -> _BVS
  | CLC -> _CLC
  | CLD -> _CLD
  | CLI -> _CLI
  | CLV -> _CLV
  | CMP -> _CMP
  | CPX -> _CPX
  | CPY -> _CPY
  | DEC -> _DEC
  | DEX -> _DEX
  | DEY -> _DEY
  | EOR -> _EOR
  | INC -> _INC
  | INX -> _INX
  | INY -> _INY
  | JMP -> _JMP
  | JSR -> _JSR
  | LDA -> _LDA
  | LDX -> _LDX
  | LDY -> _LDY
  | LSR -> _LSR
  | NOP -> _NOP
  | ORA -> _ORA
  | PHA -> _PHA
  | PHP -> _PHP
  | PLA -> _PLA
  | PLP -> _PLP
  | ROL -> _ROL
  | ROR -> _ROR
  | RTI -> _RTI
  | RTS -> _RTS
  | SBC -> _SBC
  | SEC -> _SEC
  | SED -> _SED
  | SEI -> _SEI
  | STA -> _STA
  | STX -> _STX
  | STY -> _STY
  | TAX -> _TAX
  | TAY -> _TAY
  | TSX -> _TSX
  | TXA -> _TXA
  | TXS -> _TXS
  | TYA -> _TYA

  | AHX -> _AHX
  | ALR -> _ALR
  | ANC -> _ANC
  | ARR -> _ARR
  | AXS -> _AXS
  | DCP -> _DCP
  | ISC -> _ISC
  | LAS -> _LAS
  | LAX -> _LAX
  (*| NOP -> _NOP*)
  | RLA -> _RLA
  | RRA -> _RRA
  | SAX -> _SAX
  (*| SBC -> _SBC*)
  | SHX -> _SHX
  | SHY -> _SHY
  | SLO -> _SLO
  | SRE -> _SRE
  | STP -> _STP
  | TAS -> _TAS
  | XAA -> _XAA;;

let resolve_addr addr_mode following_byte_1 following_byte_2 =
  match addr_mode with
  Immediate -> (-1, following_byte_1)
  | Implicit -> (-1, -1)
  | Absolute -> (following_byte_2 * 256 + following_byte_1, Bus.read (following_byte_2 * 256 + following_byte_1))
  | AbsoluteX -> (following_byte_2 * 256 + following_byte_1 + state.index_register_X, Bus.read (following_byte_2 * 256 + following_byte_1 + state.index_register_X))
  | AbsoluteY ->
    let addr = (following_byte_2 * 256 + following_byte_1 + state.index_register_Y) mod 65536 in
    (addr, Bus.read addr)
  | ZeroPage -> (following_byte_1, Bus.read following_byte_1)
  | ZeroPageX -> ((following_byte_1 + state.index_register_X) mod 256, Bus.read ((following_byte_1 + state.index_register_X) mod 256))
  | ZeroPageY -> ((following_byte_1 + state.index_register_Y) mod 256, Bus.read ((following_byte_1 + state.index_register_Y) mod 256))
  | Accumulator -> (-1, state.accumulator)
  | Relative -> (-1, following_byte_1)
  | IndirectX ->
      let addr = (Bus.read ((following_byte_1 + state.index_register_X) mod 256) + Bus.read ((following_byte_1 + state.index_register_X + 1) mod 256) * 256) mod 65536 in
      (addr, Bus.read addr)
  | IndirectY ->
      let addr = (Bus.read following_byte_1 + Bus.read ((following_byte_1 + 1) mod 256) * 256 + state.index_register_Y) mod 65536 in
      (addr, Bus.read addr)
  | Indirect ->
      if following_byte_1 = 0xFF then
        (Bus.read (following_byte_1 + following_byte_2 * 256) + Bus.read (following_byte_2 * 256) * 256, -1)
      else
        (Bus.read (following_byte_1 + following_byte_2 * 256) + Bus.read (following_byte_1 + following_byte_2 * 256 + 1) * 256, -1);;

let run_next_instruction () =
  if state.nmi && not state.nmi_launched then ( (* TODO: I can't figure out why the program doesn't work when I move the parenthese at the beginning of the line *)
    state.nmi_launched <- true;

    stack_push (state.program_counter lsr 8);
    stack_push (state.program_counter land 255);
    stack_push (status_to_byte ());
    state.interrupt_disable_flag <- true;
    state.program_counter <- Bus.read 0xFFFA + Bus.read 0xFFFB * 256
  );

  let opcode = Bus.read state.program_counter in
  let following_byte_1 = Bus.read ((state.program_counter + 1) mod 65536) in
  let following_byte_2 = Bus.read ((state.program_counter + 2) mod 65536) in
  
  let (instruction, addr_mode) = Cpu_instructions.decode_instruction opcode in
  
  let instruction_size = Cpu_instructions.get_instruction_size addr_mode in

  let () = (
    if logging.logging then
      try
        Printf.fprintf logging.log "%04X  " state.program_counter;
        (match instruction_size with
          1 -> Printf.fprintf logging.log "%02X        " opcode
          | 2 -> Printf.fprintf logging.log "%02X %02X     " opcode following_byte_1
          | _ -> Printf.fprintf logging.log "%02X %02X %02X  " opcode following_byte_1 following_byte_2);
        Printf.fprintf logging.log "%s  " @@ Cpu_instructions.instruction_to_string instruction;
        Printf.fprintf logging.log "A:%02X X:%02X Y:%02X P:%02X SP:%02X" state.accumulator state.index_register_X state.index_register_Y (status_to_byte ()) state.stack_pointer;
        (*Printf.fprintf logging.log " S:%02X-%02X-%02X-%02X-%02X" (Bus.read 0x1FF) (Bus.read 0x1FE) (Bus.read 0x1FD) (Bus.read 0x1FC) (Bus.read 0x1FB);*)
        Printf.fprintf logging.log "\n";
        flush logging.log;
      with exc -> raise exc
  ) in

  let resolved_addr = resolve_addr addr_mode following_byte_1 following_byte_2 in

  let instruction_function = get_instruction instruction in

  let () = instruction_function resolved_addr in (* Run instruction *)

  (if instruction = RTI && state.nmi_launched then state.nmi_launched <- false);

  match instruction with
    JMP | RTI | BRK | JSR -> ()
    | _ -> state.program_counter <- (state.program_counter + instruction_size) mod 65536;;
