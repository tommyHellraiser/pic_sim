use std::fmt::{Debug, Display, Formatter};
use error_mapper::{create_new_error, TheError, TheResult};

type RegisterFileAddrType = u8;
type DestinationType = char;
type BitLocationType = u8;
type LiteralType = u16;
type OpcodeType = u16;

#[allow(clippy::upper_case_acronyms)]
#[derive(PartialOrd, PartialEq, Clone, Debug, Copy)]
pub enum Instructions {
    //  Byte oriented file register operands
    // -- Add w and f
    ADDWF(RegisterFileAddrType, DestinationType),
    // -- AND w and f
    ANDWF(RegisterFileAddrType, DestinationType),
    // -- Clear f
    CLRF(RegisterFileAddrType),
    // -- Clear w
    CLRW,
    // -- Compliment f
    COMF(RegisterFileAddrType, DestinationType),
    // -- Decrement f
    DECF(RegisterFileAddrType, DestinationType),
    // -- Decrement f, skip if zero
    DECFSZ(RegisterFileAddrType, DestinationType),
    // -- Increment f
    INCF(RegisterFileAddrType, DestinationType),
    // -- Increment f, skip if zero
    INCFSZ(RegisterFileAddrType, DestinationType),
    // -- Inclusive or between w and f
    IORWF(RegisterFileAddrType, DestinationType),
    // -- Move f
    MOVF(RegisterFileAddrType, DestinationType),
    // -- Move w to f
    MOVWF(RegisterFileAddrType),
    // -- No operation
    NOP,
    // -- Rotate left, through carry bit
    RLF(RegisterFileAddrType, DestinationType),
    // -- Rotate right, through carry bit
    RRF(RegisterFileAddrType, DestinationType),
    // -- Subtract w from f
    SUBWF(RegisterFileAddrType, DestinationType),
    // -- Swap nibbles in f
    SWAPF(RegisterFileAddrType, DestinationType),
    // -- Exclusive or between w and f
    XORWF(RegisterFileAddrType, DestinationType),

    //  Bit oriented file register operations
    // -- Bit clear in f
    BCF(RegisterFileAddrType, BitLocationType),
    // -- Bit set in f
    BSF(RegisterFileAddrType, BitLocationType),
    // -- Bit test, skip if clear
    BTFSC(RegisterFileAddrType, BitLocationType),
    // -- Bit test skip if set
    BTFSS(RegisterFileAddrType, BitLocationType),

    //  Literal and control operations
    // -- Add literal and w
    ADDLW(LiteralType),
    // -- AND literal and w
    ANDLW(LiteralType),
    // -- Call subroutine
    CALL(LiteralType),
    // -- Clear watchdog timer
    CLRWDT,
    // -- Go to address
    GOTO(LiteralType),
    // -- Inclusive or between literal and w
    IORLW(LiteralType),
    // -- Move literal to w
    MOVLW(LiteralType),
    // -- Return from interrupt
    RETFIE,
    // -- Return with literal in w
    RETLW(LiteralType),
    // -- Simply return
    RETURN,
    // -- Enter standby mode
    SLEEP,
    // -- Subtract w from literal (it's backwards, careful with that)
    SUBLW(LiteralType),
    // -- Exclusive or between literal and w
    XORLW(LiteralType),
}

impl Instructions {
    pub fn from_opcode(opcode: OpcodeType) -> TheResult<Self> {

        //  Check the first 4 bits to separate by categories
        match opcode >> 12 {
            0b0011 => { Self::parse_literal_ops(opcode) },
            0b0010 => { Self::parse_jump_ops(opcode) },
            0b0001 => { Self::parse_single_bit_ops(opcode) },
            0b0000 => {
                if (opcode >> 9) & 0b111 != 0b000 {
                    return Self::parse_logic_ops(opcode)
                }

                if (opcode >> 7) & 0b11 != 0b00 {
                    return Self::parse_simple_ops(opcode)
                }

                Self::parse_system_ops(opcode)
            },
            _ => { Err(Self::error_from_opcode(opcode)) }
        }
    }

    fn parse_literal_ops(opcode: OpcodeType) -> TheResult<Self> {
        
        let literal = opcode & 0b11111111;
        
        match (opcode >> 10) & 0b11 {
            0b11 => {
                match (opcode >> 9) & 0b11 {
                    0b10 => { Ok(Self::SUBLW(literal)) },
                    0b11 => { Ok(Self::ADDLW(literal)) },
                    _ => { Err(Self::error_from_opcode(opcode)) }
                }
            },
            0b10 => {
                match (opcode >> 8) & 0b11 {
                    0b00 => { Ok(Self::IORLW(literal)) },
                    0b01 => { Ok(Self::ANDLW(literal)) },
                    0b10 => { Ok(Self::XORLW(literal)) },
                    _ => { Err(Self::error_from_opcode(opcode)) }
                }
            },
            0b01 => { Ok(Self::RETLW(literal)) },
            0b00 => { Ok(Self::MOVLW(literal)) },
            _ => { Err(Self::error_from_opcode(opcode)) }
        }
    }
    fn parse_jump_ops(opcode: OpcodeType) -> TheResult<Self> {
        
        let address = opcode & 0b11111111111;
        
        match (opcode >> 11) & 0b11 {
            0b00 => { Ok(Self::CALL(address)) },
            0b01 => { Ok(Self::GOTO(address)) },
            _ => { Err(Self::error_from_opcode(opcode)) }
        }
    }
    fn parse_single_bit_ops(opcode: OpcodeType) -> TheResult<Self> {
        
        let (register_file, bit) = (opcode as u8 & 0b1111111, (opcode as u8 >> 7) & 0b111);

        match (opcode >> 10) & 0b11 {
            0b00 => { Ok(Self::BCF(register_file, bit)) },
            0b01 => { Ok(Self::BSF(register_file, bit)) },
            0b10 => { Ok(Self::BTFSC(register_file, bit)) },
            0b11 => { Ok(Self::BTFSS(register_file, bit)) },
            _ => Err(Self::error_from_opcode(opcode))
        }
    }
    fn parse_logic_ops(opcode: OpcodeType) -> TheResult<Self> {
        let (register_file, destination) = (opcode as u8 & 0b1111111, if (opcode as u8 >> 7) & 0b1 == 0 { 'w' } else { 'f' } );

        match (opcode >> 8) & 0b1111 {
            0b0010 => { Ok(Self::SUBWF(register_file, destination)) },
            0b0011 => { Ok(Self::DECF(register_file, destination)) },
            0b0100 => { Ok(Self::IORWF(register_file, destination)) },
            0b0101 => { Ok(Self::ANDWF(register_file, destination)) },
            0b0110 => { Ok(Self::XORWF(register_file, destination)) },
            0b0111 => { Ok(Self::ADDWF(register_file, destination)) },
            0b1000 => { Ok(Self::MOVF(register_file, destination)) },
            0b1001 => { Ok(Self::COMF(register_file, destination)) },
            0b1010 => { Ok(Self::INCF(register_file, destination)) },
            0b1011 => { Ok(Self::DECFSZ(register_file, destination)) },
            0b1100 => { Ok(Self::RRF(register_file, destination)) },
            0b1101 => { Ok(Self::RLF(register_file, destination)) },
            0b1110 => { Ok(Self::SWAPF(register_file, destination)) },
            0b1111 => { Ok(Self::INCFSZ(register_file, destination)) },
            _ => { Err(Self::error_from_opcode(opcode)) }
        }
    }
    fn parse_simple_ops(opcode: OpcodeType) -> TheResult<Self> {
        
        let register_file = opcode as u8 & 0b1111111;
        
        match (opcode >> 7) & 0b11 {
            0b01 => { Ok(Self::MOVWF(register_file)) }
            0b11 => { Ok(Self::CLRF(register_file)) }
            0b10 => { Ok(Self::CLRW) }
            _ => { Err(Self::error_from_opcode(opcode)) }
        }
    }
    fn parse_system_ops(opcode: OpcodeType) -> TheResult<Self> {
        match opcode & 0b1111111 {
            0b0001000 => { Ok(Self::RETURN) }
            0b0001001 => { Ok(Self::RETFIE) }
            0b1100011 => { Ok(Self::SLEEP) }
            0b1100100 => { Ok(Self::CLRWDT) }
            _ => {
                if opcode & 0b11111 == 0b00000 && (opcode >> 7) & 0b111111111 == 0b000000000 {
                    Ok(Self::NOP)
                } else {
                    Err(Self::error_from_opcode(opcode))
                }
            }
        }
    }
    fn error_from_opcode(opcode: OpcodeType) -> TheError {
        create_new_error!(format!("Invalid operation code: {}", opcode))
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instructions::ADDWF(file, destination) => { write!(f, "ADDWF 0x{:x} {}", file, destination) },
            Instructions::ANDWF(file, destination) => { write!(f, "ANDWF 0x{:x} {}", file, destination) },
            Instructions::CLRF(file) => { write!(f, "CLRF 0x{:x}", file) },
            Instructions::CLRW => { write!(f, "CLRW") },
            Instructions::COMF(file, destination) => { write!(f, "COMF 0x{:x} {}", file, destination) },
            Instructions::DECF(file, destination) => { write!(f, "DECF 0x{:x} {}", file, destination) },
            Instructions::DECFSZ(file, destination) => { write!(f, "DECFSZ 0x{:x} {}", file, destination) },
            Instructions::INCF(file, destination) => { write!(f, "INCF 0x{:x} {}", file, destination) },
            Instructions::INCFSZ(file, destination) => { write!(f, "INCFSZ 0x{:x} {}", file, destination) },
            Instructions::IORWF(file, destination) => { write!(f, "IORWF 0x{:x} {}", file, destination) },
            Instructions::MOVF(file, destination) => { write!(f, "MOVF 0x{:x} {}", file, destination) },
            Instructions::MOVWF(file) => { write!(f, "MOVWF 0x{:x}", file) },
            Instructions::NOP => { write!(f, "NOP") },
            Instructions::RLF(file, destination) => { write!(f, "RLF 0x{:x} {}", file, destination) },
            Instructions::RRF(file, destination) => { write!(f, "RRF 0x{:x} {}", file, destination) },
            Instructions::SUBWF(file, destination) => { write!(f, "SUBWF 0x{:x} {}", file, destination) },
            Instructions::SWAPF(file, destination) => { write!(f, "SWAPF 0x{:x} {}", file, destination) },
            Instructions::XORWF(file, destination) => { write!(f, "XORWF 0x{:x} {}", file, destination) },
            Instructions::BCF(file, bit) => { write!(f, "BCF 0x{:x} {}", file, bit) },
            Instructions::BSF(file, bit) => { write!(f, "BSF 0x{:x} {}", file, bit) },
            Instructions::BTFSC(file, bit) => { write!(f, "BTFSC 0x{:x} {}", file, bit) },
            Instructions::BTFSS(file, bit) => { write!(f, "BTFSS 0x{:x} {}", file, bit) },
            Instructions::ADDLW(literal) => { write!(f, "ADDLW 0x{:x}", literal) },
            Instructions::ANDLW(literal) => { write!(f, "ANDLW 0x{:x}", literal) },
            Instructions::CALL(literal) => { write!(f, "CALL 0x{:x}", literal) },
            Instructions::CLRWDT => { write!(f, "CLRWDT") },
            Instructions::GOTO(literal) => { write!(f, "GOTO 0x{:x}", literal) },
            Instructions::IORLW(literal) => { write!(f, "IORLW 0x{:x}", literal) },
            Instructions::MOVLW(file) => { write!(f, "MOVLW 0x{:x}", file) },
            Instructions::RETFIE => { write!(f, "RETFIE") },
            Instructions::RETLW(file) => { write!(f, "RETLW 0x{:x}", file) },
            Instructions::RETURN => { write!(f, "RETURN") },
            Instructions::SLEEP => { write!(f, "SLEEP") },
            Instructions::SUBLW(file) => { write!(f, "SUBLW 0x{:x}", file) },
            Instructions::XORLW(file) => { write!(f, "XORLW 0x{:x}", file) },
        }
    }
}




#[cfg(test)]
mod instructions_tests {
    use crate::modules::instructions::Instructions;

    #[test]
    fn inst_addwf() {
        let result = Instructions::from_opcode(0b0000011100000000).unwrap();
        
        assert_eq!(result, Instructions::ADDWF(0, 'w'))
    }

    #[test]
    fn inst_andwf() {
        let result = Instructions::from_opcode(0b0000010100000000).unwrap();

        assert_eq!(result, Instructions::ANDWF(0, 'w'))
    }

    #[test]
    fn inst_clrf() {
        let result = Instructions::from_opcode(0b0000000110000000).unwrap();

        assert_eq!(result, Instructions::CLRF(0))
    }

    #[test]
    fn inst_clrw() {
        let result = Instructions::from_opcode(0b0000000100000000).unwrap();

        assert_eq!(result, Instructions::CLRW)
    }

    #[test]
    fn inst_comf() {
        let result = Instructions::from_opcode(0b0000100100000000).unwrap();

        assert_eq!(result, Instructions::COMF(0, 'w'))
    }

    #[test]
    fn inst_decf() {
        let result = Instructions::from_opcode(0b0000001100000000).unwrap();

        assert_eq!(result, Instructions::DECF(0, 'w'))
    }

    #[test]
    fn inst_defsz() {
        let result = Instructions::from_opcode(0b0000101100000000).unwrap();

        assert_eq!(result, Instructions::DECFSZ(0, 'w'))
    }

    #[test]
    fn inst_incf() {
        let result = Instructions::from_opcode(0b0000101000000000).unwrap();

        assert_eq!(result, Instructions::INCF(0, 'w'))
    }

    #[test]
    fn inst_infsz() {
        let result = Instructions::from_opcode(0b0000111100000000).unwrap();

        assert_eq!(result, Instructions::INCFSZ(0, 'w'))
    }

    #[test]
    fn inst_iorwf() {
        let result = Instructions::from_opcode(0b0000010000000000).unwrap();

        assert_eq!(result, Instructions::IORWF(0, 'w'))
    }

    #[test]
    fn inst_movf() {
        let result = Instructions::from_opcode(0b0000100000000000).unwrap();

        assert_eq!(result, Instructions::MOVF(0, 'w'))
    }

    #[test]
    fn inst_movwf() {
        let result = Instructions::from_opcode(0b0000000010000000).unwrap();

        assert_eq!(result, Instructions::MOVWF(0))
    }

    #[test]
    fn inst_nop() {
        let result = Instructions::from_opcode(0b0000000000000000).unwrap();

        assert_eq!(result, Instructions::NOP)
    }

    #[test]
    fn inst_rlf() {
        let result = Instructions::from_opcode(0b0000110100000000).unwrap();

        assert_eq!(result, Instructions::RLF(0, 'w'))
    }

    #[test]
    fn inst_rrf() {
        let result = Instructions::from_opcode(0b0000110000000000).unwrap();

        assert_eq!(result, Instructions::RRF(0, 'w'))
    }

    #[test]
    fn inst_subwf() {
        let result = Instructions::from_opcode(0b0000001000000000).unwrap();

        assert_eq!(result, Instructions::SUBWF(0, 'w'))
    }

    #[test]
    fn inst_swapf() {
        let result = Instructions::from_opcode(0b0000111000000000).unwrap();

        assert_eq!(result, Instructions::SWAPF(0, 'w'))
    }

    #[test]
    fn inst_xorwf() {
        let result = Instructions::from_opcode(0b0000011000000000).unwrap();

        assert_eq!(result, Instructions::XORWF(0, 'w'))
    }

    #[test]
    fn inst_bcf() {
        let result = Instructions::from_opcode(0b0001000000000000).unwrap();

        assert_eq!(result, Instructions::BCF(0, 0))
    }

    #[test]
    fn inst_bsf() {
        let result = Instructions::from_opcode(0b0001010000000000).unwrap();

        assert_eq!(result, Instructions::BSF(0, 0))
    }

    #[test]
    fn inst_btfsc() {
        let result = Instructions::from_opcode(0b0001100000000000).unwrap();

        assert_eq!(result, Instructions::BTFSC(0, 0))
    }

    #[test]
    fn inst_btfss() {
        let result = Instructions::from_opcode(0b0001110000000000).unwrap();

        assert_eq!(result, Instructions::BTFSS(0, 0))
    }
    
    #[test]
    fn inst_addlw() {
        let result = Instructions::from_opcode(0b0011111000000000).unwrap();

        assert_eq!(result, Instructions::ADDLW(0))
    }

    #[test]
    fn inst_andlw() {
        let result = Instructions::from_opcode(0b0011100100000000).unwrap();

        assert_eq!(result, Instructions::ANDLW(0))
    }

    #[test]
    fn inst_call() {
        let result = Instructions::from_opcode(0b0010000000000000).unwrap();

        assert_eq!(result, Instructions::CALL(0))
    }

    #[test]
    fn inst_clrwdt() {
        let result = Instructions::from_opcode(0b0000000001100100).unwrap();

        assert_eq!(result, Instructions::CLRWDT)
    }

    #[test]
    fn inst_goto() {
        let result = Instructions::from_opcode(0b0010100000000000).unwrap();

        assert_eq!(result, Instructions::GOTO(0))
    }

    #[test]
    fn inst_iorlw() {
        let result = Instructions::from_opcode(0b0011100000000000).unwrap();

        assert_eq!(result, Instructions::IORLW(0))
    }

    #[test]
    fn inst_movlw() {
        let result = Instructions::from_opcode(0b0011000000000000).unwrap();

        assert_eq!(result, Instructions::MOVLW(0))
    }

    #[test]
    fn inst_retfie() {
        let result = Instructions::from_opcode(0b0000000000001001).unwrap();

        assert_eq!(result, Instructions::RETFIE)
    }

    #[test]
    fn inst_retlw() {
        let result = Instructions::from_opcode(0b0011010000000000).unwrap();

        assert_eq!(result, Instructions::RETLW(0))
    }

    #[test]
    fn inst_return() {
        let result = Instructions::from_opcode(0b0000000000001000).unwrap();

        assert_eq!(result, Instructions::RETURN)
    }

    #[test]
    fn inst_sleep() {
        let result = Instructions::from_opcode(0b0000000001100011).unwrap();

        assert_eq!(result, Instructions::SLEEP)
    }

    #[test]
    fn inst_sublw() {
        let result = Instructions::from_opcode(0b0011110000000000).unwrap();

        assert_eq!(result, Instructions::SUBLW(0))
    }

    #[test]
    fn inst_xorlw() {
        let result = Instructions::from_opcode(0b0011101000000000).unwrap();

        assert_eq!(result, Instructions::XORLW(0))
    }
}


/*

Literal ops
MOVLW   -> 0011 00xx xxxx xxxx
RETLW   -> 0011 01xx xxxx xxxx

IORLW   -> 0011 1000 xxxx xxxx
ANDLW   -> 0011 1001 xxxx xxxx
XORLW   -> 0011 1010 xxxx xxxx

SUBLW   -> 0011 110x xxxx xxxx
ADDLW   -> 0011 111x xxxx xxxx

Jump ops
CALL    -> 0010 0xxx xxxx xxxx
GOTO    -> 0010 1xxx xxxx xxxx

Single bit ops
BCF     -> 0001 00xx xxxx xxxx
BSF     -> 0001 01xx xxxx xxxx
BTFSC   -> 0001 10xx xxxx xxxx
BTFSS   -> 0001 11xx xxxx xxxx

Logic ops
SUBFW   -> 0000 0010 xxxx xxxx
DECF    -> 0000 0011 xxxx xxxx
IORWF   -> 0000 0100 xxxx xxxx
ANDWF   -> 0000 0101 xxxx xxxx
XORWF   -> 0000 0110 xxxx xxxx
ADDWF   -> 0000 0111 xxxx xxxx
MOVF    -> 0000 1000 xxxx xxxx
COMF    -> 0000 1001 xxxx xxxx
INCF    -> 0000 1010 xxxx xxxx
DECFSZ  -> 0000 1011 xxxx xxxx
RRF     -> 0000 1100 xxxx xxxx
RLF     -> 0000 1101 xxxx xxxx
SWAPF   -> 0000 1110 xxxx xxxx
INCFSZ  -> 0000 1111 xxxx xxxx

Simple ops
MOVWF   -> 0000 0000 1xxx xxxx
CLRF    -> 0000 0001 1xxx xxxx
CLRW    -> 0000 0001 0xxx xxxx

System ops
NOP     -> 0000 0000 0xx0 0000
RETURN  -> 0000 0000 0000 1000
RETFIE  -> 0000 0000 0000 1001
SLEEP   -> 0000 0000 0110 0011
CLRWDT  -> 0000 0000 0110 0100


----------------------------------------------------------------

SLEEP   -> 0000 0000 0110 0011
RETFIE  -> 0000 0000 0000 1001
CLRWDT  -> 0000 0000 0110 0100
NOP     -> 0000 0000 0xx0 0000
ADDWF   -> 0000 0111 xxxx xxxx
ANDWF   -> 0000 0101 xxxx xxxx
CLRF    -> 0000 0001 1xxx xxxx
CKRW    -> 0000 0001 0xxx xxxx
COMF    -> 0000 1001 xxxx xxxx
DECF    -> 0000 0011 xxxx xxxx
DECFSZ  -> 0000 1011 xxxx xxxx
INCF    -> 0000 1010 xxxx xxxx
INCFSZ  -> 0000 1111 xxxx xxxx
IORWF   -> 0000 0100 xxxx xxxx
MOVF    -> 0000 1000 xxxx xxxx
MOFW    -> 0000 0000 1xxx xxxx
RLF     -> 0000 1101 xxxx xxxx
RETURN  -> 0000 0000 0000 1000
RRF     -> 0000 1100 xxxx xxxx
SUBFW   -> 0000 0010 xxxx xxxx
SWAPF   -> 0000 1110 xxxx xxxx
XORWF   -> 0000 0110 xxxx xxxx
BCF     -> 0001 00xx xxxx xxxx
BSF     -> 0001 01xx xxxx xxxx
BTFSC   -> 0001 10xx xxxx xxxx
BTFSS   -> 0001 11xx xxxx xxxx
ADDLW   -> 0011 111x xxxx xxxx
ANDLW   -> 0011 1001 xxxx xxxx
CALL    -> 0010 0xxx xxxx xxxx
GOTO    -> 0010 1xxx xxxx xxxx
IORLW   -> 0011 1000 xxxx xxxx
MOVLW   -> 0011 00xx xxxx xxxx
RETLW   -> 0011 01xx xxxx xxxx
SUBLW   -> 0011 110x xxxx xxxx
XORLW   -> 0011 1010 xxxx xxxx


-------------------------------------------------------------------


 */