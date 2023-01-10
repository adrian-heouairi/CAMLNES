open Opcodes

let bus_temp = Array.make 0xFFFF 0;;

type cpu_state = {
  mutable accumulator : int;
  mutable index_register_X : int;
  mutable index_register_Y : int;
  mutable program_counter : int;
  mutable stack_pointer : int;

  mutable carry_flag : bool;
  mutable zero_flag : bool;
  mutable interrupt_disable_flag : bool;
  mutable decimal_mode_flag : bool;
  mutable break_command_flag : bool;
  mutable overflow_flag : bool;
  mutable negative_flag : bool
};;

let cpu_state = {
  accumulator = 0;
  index_register_X = 0;
  index_register_Y = 0;
  program_counter = 0x8000; (* TODO: set this to what is contained in $FFFC in the cartridge *)
  stack_pointer = 0xFF;

  carry_flag = false;
  zero_flag = false;
  interrupt_disable_flag = true;
  decimal_mode_flag = false;
  break_command_flag = false;
  overflow_flag = false;
  negative_flag = false;
};;

let _ADC (calculated_address, byte_at_address) =
  let carry = if cpu_state.carry_flag then 1 else 0 in
  let temp = cpu_state.accumulator + byte_at_address + carry in
  let () = cpu_state.carry_flag <- temp > 255 in
  let () = cpu_state.zero_flag <- temp = 0 in
  let temp = temp mod 256 in
  let () = cpu_state.overflow_flag <- ((cpu_state.accumulator lxor temp) land (byte_at_address lxor temp) land 0x80) <> 0 in
  let () = cpu_state.negative_flag <- temp > 127 in
  cpu_state.accumulator <- temp;;

let _AND (calculated_address, byte_at_address) = ()

let _ASL (calculated_address, byte_at_address) = ()

let _BCC (calculated_address, byte_at_address) = ()

let _BCS (calculated_address, byte_at_address) = ()

let _BEQ (calculated_address, byte_at_address) = ()

let _BIT (calculated_address, byte_at_address) = ()

let _BMI (calculated_address, byte_at_address) = ()

let _BNE (calculated_address, byte_at_address) = ()

let _BPL (calculated_address, byte_at_address) = ()

let _BRK (calculated_address, byte_at_address) = ()

let _BVC (calculated_address, byte_at_address) = ()

let _BVS (calculated_address, byte_at_address) = ()

let _CLC (calculated_address, byte_at_address) = ()

let _CLD (calculated_address, byte_at_address) = ()

let _CLI (calculated_address, byte_at_address) = ()

let _CLV (calculated_address, byte_at_address) = ()

let _CMP (calculated_address, byte_at_address) = ()

let _CPX (calculated_address, byte_at_address) = ()

let _CPY (calculated_address, byte_at_address) = ()

let _DEC (calculated_address, byte_at_address) = ()

let _DEX (calculated_address, byte_at_address) = ()

let _DEY (calculated_address, byte_at_address) = ()

let _EOR (calculated_address, byte_at_address) = ()

let _INC (calculated_address, byte_at_address) = ()

let _INX (calculated_address, byte_at_address) = ()

let _INY (calculated_address, byte_at_address) = ()

let _JMP (calculated_address, byte_at_address) = ()

let _JSR (calculated_address, byte_at_address) = ()

let _LDA (calculated_address, byte_at_address) = ()

let _LDX (calculated_address, byte_at_address) = ()

let _LDY (calculated_address, byte_at_address) = ()

let _LSR (calculated_address, byte_at_address) = ()

let _NOP (calculated_address, byte_at_address) = ()

let _ORA (calculated_address, byte_at_address) = ()

let _PHA (calculated_address, byte_at_address) = ()

let _PHP (calculated_address, byte_at_address) = ()

let _PLA (calculated_address, byte_at_address) = ()

let _PLP (calculated_address, byte_at_address) = ()

let _ROL (calculated_address, byte_at_address) = ()

let _ROR (calculated_address, byte_at_address) = ()

let _RTI (calculated_address, byte_at_address) = ()

let _RTS (calculated_address, byte_at_address) = ()

let _SBC (calculated_address, byte_at_address) = ()

let _SEC (calculated_address, byte_at_address) = ()

let _SED (calculated_address, byte_at_address) = ()

let _SEI (calculated_address, byte_at_address) = ()

let _STA (calculated_address, byte_at_address) = ()

let _STX (calculated_address, byte_at_address) = ()

let _STY (calculated_address, byte_at_address) = ()

let _TAX (calculated_address, byte_at_address) = ()

let _TAY (calculated_address, byte_at_address) = ()

let _TSX (calculated_address, byte_at_address) = ()

let _TXA (calculated_address, byte_at_address) = ()

let _TXS (calculated_address, byte_at_address) = ()

let _TYA (calculated_address, byte_at_address) = ()

let run_instruction instruction calculated_address_and_byte_at_address =
  match instruction with
  ADC -> _ADC calculated_address_and_byte_at_address
  | AND -> _AND calculated_address_and_byte_at_address
  | ASL -> _ASL calculated_address_and_byte_at_address
  | BCC -> _BCC calculated_address_and_byte_at_address
  | BCS -> _BCS calculated_address_and_byte_at_address
  | BEQ -> _BEQ calculated_address_and_byte_at_address
  | BIT -> _BIT calculated_address_and_byte_at_address
  | BMI -> _BMI calculated_address_and_byte_at_address
  | BNE -> _BNE calculated_address_and_byte_at_address
  | BPL -> _BPL calculated_address_and_byte_at_address
  | BRK -> _BRK calculated_address_and_byte_at_address
  | BVC -> _BVC calculated_address_and_byte_at_address
  | BVS -> _BVS calculated_address_and_byte_at_address
  | CLC -> _CLC calculated_address_and_byte_at_address
  | CLD -> _CLD calculated_address_and_byte_at_address
  | CLI -> _CLI calculated_address_and_byte_at_address
  | CLV -> _CLV calculated_address_and_byte_at_address
  | CMP -> _CMP calculated_address_and_byte_at_address
  | CPX -> _CPX calculated_address_and_byte_at_address
  | CPY -> _CPY calculated_address_and_byte_at_address
  | DEC -> _DEC calculated_address_and_byte_at_address
  | DEX -> _DEX calculated_address_and_byte_at_address
  | DEY -> _DEY calculated_address_and_byte_at_address
  | EOR -> _EOR calculated_address_and_byte_at_address
  | INC -> _INC calculated_address_and_byte_at_address
  | INX -> _INX calculated_address_and_byte_at_address
  | INY -> _INY calculated_address_and_byte_at_address
  | JMP -> _JMP calculated_address_and_byte_at_address
  | JSR -> _JSR calculated_address_and_byte_at_address
  | LDA -> _LDA calculated_address_and_byte_at_address
  | LDX -> _LDX calculated_address_and_byte_at_address
  | LDY -> _LDY calculated_address_and_byte_at_address
  | LSR -> _LSR calculated_address_and_byte_at_address
  | NOP -> _NOP calculated_address_and_byte_at_address
  | ORA -> _ORA calculated_address_and_byte_at_address
  | PHA -> _PHA calculated_address_and_byte_at_address
  | PHP -> _PHP calculated_address_and_byte_at_address
  | PLA -> _PLA calculated_address_and_byte_at_address
  | PLP -> _PLP calculated_address_and_byte_at_address
  | ROL -> _ROL calculated_address_and_byte_at_address
  | ROR -> _ROR calculated_address_and_byte_at_address
  | RTI -> _RTI calculated_address_and_byte_at_address
  | RTS -> _RTS calculated_address_and_byte_at_address
  | SBC -> _SBC calculated_address_and_byte_at_address
  | SEC -> _SEC calculated_address_and_byte_at_address
  | SED -> _SED calculated_address_and_byte_at_address
  | SEI -> _SEI calculated_address_and_byte_at_address
  | STA -> _STA calculated_address_and_byte_at_address
  | STX -> _STX calculated_address_and_byte_at_address
  | STY -> _STY calculated_address_and_byte_at_address
  | TAX -> _TAX calculated_address_and_byte_at_address
  | TAY -> _TAY calculated_address_and_byte_at_address
  | TSX -> _TSX calculated_address_and_byte_at_address
  | TXA -> _TXA calculated_address_and_byte_at_address
  | TXS -> _TXS calculated_address_and_byte_at_address
  | TYA -> _TYA calculated_address_and_byte_at_address;;

let determine_needed_value addressing_mode bytes_following =
  let (byte_following_1, byte_following_2) = bytes_following in

  match addressing_mode with
  Immediate -> (-1, byte_following_1)
  | Implicit -> (-1, -1)
  | Absolute -> (byte_following_1 * 256 + byte_following_2, bus_temp.(byte_following_1 * 256 + byte_following_2))
  | AbsoluteX -> (byte_following_1 * 256 + byte_following_2 + cpu_state.index_register_X, bus_temp.(byte_following_1 * 256 + byte_following_2 + cpu_state.index_register_X))
  | AbsoluteY -> (byte_following_1 * 256 + byte_following_2 + cpu_state.index_register_Y, bus_temp.(byte_following_1 * 256 + byte_following_2 + cpu_state.index_register_Y))
  | ZeroPage -> (byte_following_1, bus_temp.(byte_following_1))
  | ZeroPageX -> ((byte_following_1 + cpu_state.index_register_X) mod 256, bus_temp.((byte_following_1 + cpu_state.index_register_X) mod 256))
  | ZeroPageY -> ((byte_following_1 + cpu_state.index_register_Y) mod 256, bus_temp.((byte_following_1 + cpu_state.index_register_Y) mod 256))
  | Accumulator -> (-1, -1)
  | Relative -> (-1, byte_following_1)
  | IndirectX -> (-1, -1) (* TODO *)
  | IndirectY -> (bus_temp.(byte_following_1) + bus_temp.((byte_following_1 + 1) mod 256) * 256 + cpu_state.index_register_Y, bus_temp.(bus_temp.(byte_following_1) + bus_temp.((byte_following_1 + 1) mod 256) * 256 + cpu_state.index_register_Y))
    (* TODO: Implement the bug of the 6502 with JMP with Indirect *)
  | Indirect -> (bus_temp.(byte_following_1) + bus_temp.(byte_following_2) * 256, -1);;

let run_next_instruction () =
  let instruction_code = bus_temp.(cpu_state.program_counter) in
  let following_bytes = (bus_temp.(cpu_state.program_counter + 1), bus_temp.(cpu_state.program_counter + 2)) in
  
  let instruction_with_addressing_mode = Hashtbl.find instructions_table instruction_code in

  let calculated_address_and_byte_at_address = determine_needed_value instruction_with_addressing_mode.addressing_mode following_bytes in

  let () = run_instruction instruction_with_addressing_mode.instruction calculated_address_and_byte_at_address in

  let program_counter_increment = Hashtbl.find addressing_mode_needed_bytes instruction_with_addressing_mode.addressing_mode in
  cpu_state.program_counter <- cpu_state.program_counter + program_counter_increment;;
