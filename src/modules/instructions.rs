
type RegisterFileAddrType = u8;
type DestinationType = u8;
type BitLocationType = u8;
type LiteralType = u16;

#[allow(clippy::upper_case_acronyms)]
enum Instructions {
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
    CLRWDT(LiteralType),
    // -- Go to address
    GOTO,
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
    // fn from_opcode(opcode: &str) -> TheResult<Self> {
    //
    // }
}
