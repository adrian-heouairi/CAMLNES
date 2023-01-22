type addr_mode = Immediate | Implicit | Absolute | AbsoluteX | AbsoluteY
  | ZeroPage | ZeroPageX | ZeroPageY | Accumulator
  | Relative | IndirectX | IndirectY | Indirect;;

let get_instruction_size addr_mode =
  match addr_mode with
  Immediate -> 2
  | Implicit -> 1
  | Absolute -> 3
  | AbsoluteX -> 3
  | AbsoluteY -> 3
  | ZeroPage -> 2
  | ZeroPageX -> 2
  | ZeroPageY -> 2
  | Accumulator -> 1
  | Relative -> 2
  | IndirectX -> 2
  | IndirectY -> 2
  | Indirect -> 3;;

type instruction = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK
| BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX
| DEY | EOR | INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR
| NOP | ORA | PHA | PHP | PLA | PLP | ROL | ROR | RTI | RTS | SBC
| SEC | SED | SEI | STA | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA;;

let instruction_to_string instruction =
  match instruction with
  ADC -> "ADC" | AND -> "AND" | ASL -> "ASL" | BCC -> "BCC" | BCS -> "BCS" | BEQ -> "BEQ" | BIT -> "BIT" | BMI -> "BMI" | BNE -> "BNE" | BPL -> "BPL" | BRK -> "BRK"
| BVC -> "BVC" | BVS -> "BVS" | CLC -> "CLC" | CLD -> "CLD" | CLI -> "CLI" | CLV -> "CLV" | CMP -> "CMP" | CPX -> "CPX" | CPY -> "CPY" | DEC -> "DEC" | DEX -> "DEX"
| DEY -> "DEY" | EOR -> "EOR" | INC -> "INC" | INX -> "INX" | INY -> "INY" | JMP -> "JMP" | JSR -> "JSR" | LDA -> "LDA" | LDX -> "LDX" | LDY -> "LDY" | LSR -> "LSR"
| NOP -> "NOP" | ORA -> "ORA" | PHA -> "PHA" | PHP -> "PHP" | PLA -> "PLA" | PLP -> "PLP" | ROL -> "ROL" | ROR -> "ROR" | RTI -> "RTI" | RTS -> "RTS" | SBC -> "SBC"
| SEC -> "SEC" | SED -> "SED" | SEI -> "SEI" | STA -> "STA" | STX -> "STX" | STY -> "STY" | TAX -> "TAX" | TAY -> "TAY" | TSX -> "TSX" | TXA -> "TXA" | TXS -> "TXS" | TYA -> "TYA";;

let decode_instruction instruction =
  match instruction with
  0x69 -> (ADC, Immediate)
  | 0x65 -> (ADC, ZeroPage)
  | 0x75 -> (ADC, ZeroPageX)
  | 0x6D -> (ADC, Absolute)
  | 0x7D -> (ADC, AbsoluteX)
  | 0x79 -> (ADC, AbsoluteY)
  | 0x61 -> (ADC, IndirectX)
  | 0x71 -> (ADC, IndirectY)
  | 0x29 -> (AND, Immediate)
  | 0x25 -> (AND, ZeroPage)
  | 0x35 -> (AND, ZeroPageX)
  | 0x2D -> (AND, Absolute)
  | 0x3D -> (AND, AbsoluteX)
  | 0x39 -> (AND, AbsoluteY)
  | 0x21 -> (AND, IndirectX)
  | 0x31 -> (AND, IndirectY)
  | 0x0A -> (ASL, Accumulator)
  | 0x06 -> (ASL, ZeroPage)
  | 0x16 -> (ASL, ZeroPageX)
  | 0x0E -> (ASL, Absolute)
  | 0x1E -> (ASL, AbsoluteX)
  | 0x90 -> (BCC, Relative)
  | 0xB0 -> (BCS, Relative)
  | 0xF0 -> (BEQ, Relative)
  | 0x24 -> (BIT, ZeroPage)
  | 0x2C -> (BIT, Absolute)
  | 0x30 -> (BMI, Relative)
  | 0xD0 -> (BNE, Relative)
  | 0x10 -> (BPL, Relative)
  | 0x00 -> (BRK, Implicit)
  | 0x50 -> (BVC, Relative)
  | 0x70 -> (BVS, Relative)
  | 0x18 -> (CLC, Implicit)
  | 0xD8 -> (CLD, Implicit)
  | 0x58 -> (CLI, Implicit)
  | 0xB8 -> (CLV, Implicit)
  | 0xC9 -> (CMP, Immediate)
  | 0xC5 -> (CMP, ZeroPage)
  | 0xD5 -> (CMP, ZeroPageX)
  | 0xCD -> (CMP, Absolute)
  | 0xDD -> (CMP, AbsoluteX)
  | 0xD9 -> (CMP, AbsoluteY)
  | 0xC1 -> (CMP, IndirectX)
  | 0xD1 -> (CMP, IndirectY)
  | 0xE0 -> (CPX, Immediate)
  | 0xE4 -> (CPX, ZeroPage)
  | 0xEC -> (CPX, Absolute)
  | 0xC0 -> (CPY, Immediate)
  | 0xC4 -> (CPY, ZeroPage)
  | 0xCC -> (CPY, Absolute)
  | 0xC6 -> (DEC, ZeroPage)
  | 0xD6 -> (DEC, ZeroPageX)
  | 0xCE -> (DEC, Absolute)
  | 0xDE -> (DEC, AbsoluteX)
  | 0xCA -> (DEX, Implicit)
  | 0x88 -> (DEY, Implicit)
  | 0x49 -> (EOR, Immediate)
  | 0x45 -> (EOR, ZeroPage)
  | 0x55 -> (EOR, ZeroPageX)
  | 0x4D -> (EOR, Absolute)
  | 0x5D -> (EOR, AbsoluteX)
  | 0x59 -> (EOR, AbsoluteY)
  | 0x41 -> (EOR, IndirectX)
  | 0x51 -> (EOR, IndirectY)
  | 0xE6 -> (INC, ZeroPage)
  | 0xF6 -> (INC, ZeroPageX)
  | 0xEE -> (INC, Absolute)
  | 0xFE -> (INC, AbsoluteX)
  | 0xE8 -> (INX, Implicit)
  | 0xC8 -> (INY, Implicit)
  | 0x4C -> (JMP, Absolute)
  | 0x6C -> (JMP, Indirect)
  | 0x20 -> (JSR, Absolute)
  | 0xA9 -> (LDA, Immediate)
  | 0xA5 -> (LDA, ZeroPage)
  | 0xB5 -> (LDA, ZeroPageX)
  | 0xAD -> (LDA, Absolute)
  | 0xBD -> (LDA, AbsoluteX)
  | 0xB9 -> (LDA, AbsoluteY)
  | 0xA1 -> (LDA, IndirectX)
  | 0xB1 -> (LDA, IndirectY)
  | 0xA2 -> (LDX, Immediate)
  | 0xA6 -> (LDX, ZeroPage)
  | 0xB6 -> (LDX, ZeroPageY)
  | 0xAE -> (LDX, Absolute)
  | 0xBE -> (LDX, AbsoluteY)
  | 0xA0 -> (LDY, Immediate)
  | 0xA4 -> (LDY, ZeroPage)
  | 0xB4 -> (LDY, ZeroPageX)
  | 0xAC -> (LDY, Absolute)
  | 0xBC -> (LDY, AbsoluteX)
  | 0x4A -> (LSR, Accumulator)
  | 0x46 -> (LSR, ZeroPage)
  | 0x56 -> (LSR, ZeroPageX)
  | 0x4E -> (LSR, Absolute)
  | 0x5E -> (LSR, AbsoluteX)
  | 0xEA -> (NOP, Implicit)
  | 0x09 -> (ORA, Immediate)
  | 0x05 -> (ORA, ZeroPage)
  | 0x15 -> (ORA, ZeroPageX)
  | 0x0D -> (ORA, Absolute)
  | 0x1D -> (ORA, AbsoluteX)
  | 0x19 -> (ORA, AbsoluteY)
  | 0x01 -> (ORA, IndirectX)
  | 0x11 -> (ORA, IndirectY)
  | 0x48 -> (PHA, Implicit)
  | 0x08 -> (PHP, Implicit)
  | 0x68 -> (PLA, Implicit)
  | 0x28 -> (PLP, Implicit)
  | 0x2A -> (ROL, Accumulator)
  | 0x26 -> (ROL, ZeroPage)
  | 0x36 -> (ROL, ZeroPageX)
  | 0x2E -> (ROL, Absolute)
  | 0x3E -> (ROL, AbsoluteX)
  | 0x6A -> (ROR, Accumulator)
  | 0x66 -> (ROR, ZeroPage)
  | 0x76 -> (ROR, ZeroPageX)
  | 0x6E -> (ROR, Absolute)
  | 0x7E -> (ROR, AbsoluteX)
  | 0x40 -> (RTI, Implicit)
  | 0x60 -> (RTS, Implicit)
  | 0xE9 -> (SBC, Immediate)
  | 0xE5 -> (SBC, ZeroPage)
  | 0xF5 -> (SBC, ZeroPageX)
  | 0xED -> (SBC, Absolute)
  | 0xFD -> (SBC, AbsoluteX)
  | 0xF9 -> (SBC, AbsoluteY)
  | 0xE1 -> (SBC, IndirectX)
  | 0xF1 -> (SBC, IndirectY)
  | 0x38 -> (SEC, Implicit)
  | 0xF8 -> (SED, Implicit)
  | 0x78 -> (SEI, Implicit)
  | 0x85 -> (STA, ZeroPage)
  | 0x95 -> (STA, ZeroPageX)
  | 0x8D -> (STA, Absolute)
  | 0x9D -> (STA, AbsoluteX)
  | 0x99 -> (STA, AbsoluteY)
  | 0x81 -> (STA, IndirectX)
  | 0x91 -> (STA, IndirectY)
  | 0x86 -> (STX, ZeroPage)
  | 0x96 -> (STX, ZeroPageY)
  | 0x8E -> (STX, Absolute)
  | 0x84 -> (STY, ZeroPage)
  | 0x94 -> (STY, ZeroPageX)
  | 0x8C -> (STY, Absolute)
  | 0xAA -> (TAX, Implicit)
  | 0xA8 -> (TAY, Implicit)
  | 0xBA -> (TSX, Implicit)
  | 0x8A -> (TXA, Implicit)
  | 0x9A -> (TXS, Implicit)
  | 0x98 -> (TYA, Implicit)
  | _ -> failwith "Bad instruction";;
