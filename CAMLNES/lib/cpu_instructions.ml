type addr_mode =
  | Immediate
  | Implicit
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Accumulator
  | Relative
  | IndirectX
  | IndirectY
  | Indirect

let get_instruction_size addr_mode =
  match addr_mode with
  | Immediate -> 2
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
  | Indirect -> 3

type instruction =
  | ADC
  | AND
  | ASL
  | BCC
  | BCS
  | BEQ
  | BIT
  | BMI
  | BNE
  | BPL
  | BRK
  | BVC
  | BVS
  | CLC
  | CLD
  | CLI
  | CLV
  | CMP
  | CPX
  | CPY
  | DEC
  | DEX
  | DEY
  | EOR
  | INC
  | INX
  | INY
  | JMP
  | JSR
  | LDA
  | LDX
  | LDY
  | LSR
  | NOP
  | ORA
  | PHA
  | PHP
  | PLA
  | PLP
  | ROL
  | ROR
  | RTI
  | RTS
  | SBC
  | SEC
  | SED
  | SEI
  | STA
  | STX
  | STY
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS
  | TYA
  (* Unofficial instructions *)
  | AHX
  | ALR
  | ANC
  | ARR
  | AXS
  | DCP
  | ISC
  | LAS
  | LAX
  (*| NOP*)
  | RLA
  | RRA
  | SAX
  (*| SBC*)
  | SHX
  | SHY
  | SLO
  | SRE
  | STP
  | TAS
  | XAA

let instruction_to_string instruction =
  match instruction with
  | ADC -> "ADC"
  | AND -> "AND"
  | ASL -> "ASL"
  | BCC -> "BCC"
  | BCS -> "BCS"
  | BEQ -> "BEQ"
  | BIT -> "BIT"
  | BMI -> "BMI"
  | BNE -> "BNE"
  | BPL -> "BPL"
  | BRK -> "BRK"
  | BVC -> "BVC"
  | BVS -> "BVS"
  | CLC -> "CLC"
  | CLD -> "CLD"
  | CLI -> "CLI"
  | CLV -> "CLV"
  | CMP -> "CMP"
  | CPX -> "CPX"
  | CPY -> "CPY"
  | DEC -> "DEC"
  | DEX -> "DEX"
  | DEY -> "DEY"
  | EOR -> "EOR"
  | INC -> "INC"
  | INX -> "INX"
  | INY -> "INY"
  | JMP -> "JMP"
  | JSR -> "JSR"
  | LDA -> "LDA"
  | LDX -> "LDX"
  | LDY -> "LDY"
  | LSR -> "LSR"
  | NOP -> "NOP"
  | ORA -> "ORA"
  | PHA -> "PHA"
  | PHP -> "PHP"
  | PLA -> "PLA"
  | PLP -> "PLP"
  | ROL -> "ROL"
  | ROR -> "ROR"
  | RTI -> "RTI"
  | RTS -> "RTS"
  | SBC -> "SBC"
  | SEC -> "SEC"
  | SED -> "SED"
  | SEI -> "SEI"
  | STA -> "STA"
  | STX -> "STX"
  | STY -> "STY"
  | TAX -> "TAX"
  | TAY -> "TAY"
  | TSX -> "TSX"
  | TXA -> "TXA"
  | TXS -> "TXS"
  | TYA -> "TYA"
  (* Unofficial instructions, names are from https://www.nesdev.org/wiki/CPU_unofficial_opcodes *)
  | AHX -> "AHX"
  | ALR -> "ALR"
  | ANC -> "ANC"
  | ARR -> "ARR"
  | AXS -> "AXS"
  | DCP -> "DCP"
  | ISC -> "ISC"
  | LAS -> "LAS"
  | LAX -> "LAX"
  (*| NOP -> "NOP"*)
  | RLA -> "RLA"
  | RRA -> "RRA"
  | SAX -> "SAX"
  (*| SBC -> "SBC"*)
  | SHX -> "SHX"
  | SHY -> "SHY"
  | SLO -> "SLO"
  | SRE -> "SRE"
  | STP -> "STP"
  | TAS -> "TAS"
  | XAA -> "XAA"

let decode_instruction instruction =
  match instruction with
  (* 151 official instructions *)
  | 0x69 -> (ADC, Immediate)
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
  (* 105 unofficial instructions *)
  (* TODO: Some instructions marked Implicit might actually be Accumulator? *)
  | 0x02 -> (STP, Implicit)
  | 0x03 -> (SLO, IndirectX)
  | 0x04 -> (NOP, ZeroPage)
  | 0x07 -> (SLO, ZeroPage)
  | 0x0B -> (ANC, Immediate)
  | 0x0C -> (NOP, Absolute)
  | 0x0F -> (SLO, Absolute)
  | 0x12 -> (STP, Implicit)
  | 0x13 -> (SLO, IndirectY)
  | 0x14 -> (NOP, ZeroPageX)
  | 0x17 -> (SLO, ZeroPageX)
  | 0x1A -> (NOP, Implicit)
  | 0x1B -> (SLO, AbsoluteY)
  | 0x1C -> (NOP, AbsoluteX)
  | 0x1F -> (SLO, AbsoluteX)
  | 0x22 -> (STP, Implicit)
  | 0x23 -> (RLA, IndirectX)
  | 0x27 -> (RLA, ZeroPage)
  | 0x2B -> (ANC, Immediate)
  | 0x2F -> (RLA, Absolute)
  | 0x32 -> (STP, Implicit)
  | 0x33 -> (RLA, IndirectY)
  | 0x34 -> (NOP, ZeroPageX)
  | 0x37 -> (RLA, ZeroPageX)
  | 0x3A -> (NOP, Implicit)
  | 0x3B -> (RLA, AbsoluteY)
  | 0x3C -> (NOP, AbsoluteX)
  | 0x3F -> (RLA, AbsoluteX)
  | 0x42 -> (STP, Implicit)
  | 0x43 -> (SRE, IndirectX)
  | 0x44 -> (NOP, ZeroPage)
  | 0x47 -> (SRE, ZeroPage)
  | 0x4B -> (ALR, Immediate)
  | 0x4F -> (SRE, Absolute)
  | 0x52 -> (STP, Implicit)
  | 0x53 -> (SRE, IndirectY)
  | 0x54 -> (NOP, ZeroPageX)
  | 0x57 -> (SRE, ZeroPageX)
  | 0x5A -> (NOP, Implicit)
  | 0x5B -> (SRE, AbsoluteY)
  | 0x5C -> (NOP, AbsoluteX)
  | 0x5F -> (SRE, AbsoluteX)
  | 0x62 -> (STP, Implicit)
  | 0x63 -> (RRA, IndirectX)
  | 0x64 -> (NOP, ZeroPage)
  | 0x67 -> (RRA, ZeroPage)
  | 0x6B -> (ARR, Immediate)
  | 0x6F -> (RRA, Absolute)
  | 0x72 -> (STP, Implicit)
  | 0x73 -> (RRA, IndirectY)
  | 0x74 -> (NOP, ZeroPageX)
  | 0x77 -> (RRA, ZeroPageX)
  | 0x7A -> (NOP, Implicit)
  | 0x7B -> (RRA, AbsoluteY)
  | 0x7C -> (NOP, AbsoluteX)
  | 0x7F -> (RRA, AbsoluteX)
  | 0x80 -> (NOP, Immediate)
  | 0x82 -> (NOP, Immediate)
  | 0x83 -> (SAX, IndirectX)
  | 0x87 -> (SAX, ZeroPage)
  | 0x89 -> (NOP, Immediate)
  | 0x8B -> (XAA, Immediate)
  | 0x8F -> (SAX, Absolute)
  | 0x92 -> (STP, Implicit)
  | 0x93 -> (AHX, IndirectY)
  | 0x97 -> (SAX, ZeroPageY)
  | 0x9B -> (TAS, AbsoluteY)
  | 0x9C -> (SHY, AbsoluteX)
  | 0x9E -> (SHX, AbsoluteY)
  | 0x9F -> (AHX, AbsoluteY)
  | 0xA3 -> (LAX, IndirectX)
  | 0xA7 -> (LAX, ZeroPage)
  | 0xAB -> (LAX, Immediate)
  | 0xAF -> (LAX, Absolute)
  | 0xB2 -> (STP, Implicit)
  | 0xB3 -> (LAX, IndirectY)
  | 0xB7 -> (LAX, ZeroPageY)
  | 0xBB -> (LAS, AbsoluteY)
  | 0xBF -> (LAX, AbsoluteY)
  | 0xC2 -> (NOP, Immediate)
  | 0xC3 -> (DCP, IndirectX)
  | 0xC7 -> (DCP, ZeroPage)
  | 0xCB -> (AXS, Immediate)
  | 0xCF -> (DCP, Absolute)
  | 0xD2 -> (STP, Implicit)
  | 0xD3 -> (DCP, IndirectY)
  | 0xD4 -> (NOP, ZeroPageX)
  | 0xD7 -> (DCP, ZeroPageX)
  | 0xDA -> (NOP, Implicit)
  | 0xDB -> (DCP, AbsoluteY)
  | 0xDC -> (NOP, AbsoluteX)
  | 0xDF -> (DCP, AbsoluteX)
  | 0xE2 -> (NOP, Immediate)
  | 0xE3 -> (ISC, IndirectX)
  | 0xE7 -> (ISC, ZeroPage)
  | 0xEB -> (SBC, Immediate)
  | 0xEF -> (ISC, Absolute)
  | 0xF2 -> (STP, Implicit)
  | 0xF3 -> (ISC, IndirectY)
  | 0xF4 -> (NOP, ZeroPageX)
  | 0xF7 -> (ISC, ZeroPageX)
  | 0xFA -> (NOP, Implicit)
  | 0xFB -> (ISC, AbsoluteY)
  | 0xFC -> (NOP, AbsoluteX)
  | 0xFF -> (ISC, AbsoluteX)
  | x -> failwith @@ Printf.sprintf "Bad instruction 0x%02X" x
