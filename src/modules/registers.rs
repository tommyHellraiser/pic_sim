
type AddressType = u8;
type BitsContentType = u8;

struct PicRegister {
    address: AddressType,
    bits_content: BitsContentType,
    available_bits: BitsContentType
}

impl PicRegister {
    fn get_bit_status(&self, bit: BitsContentType) -> bool {
        (self.bits_content >> (bit)) & 1 != 0
    }
    fn set_bit(&mut self, bit: BitsContentType) {
        self.bits_content ^= 1 << bit
    }
    fn reset_bit(&mut self, bit: BitsContentType) {
        self.bits_content &= !(1 << bit)
    }
    fn get_value(&self) -> BitsContentType {
        self.bits_content
    }
    fn set_value(&mut self, value: BitsContentType) {
        self.bits_content = value
    }
}

#[cfg(test)]
mod pic_register_tests {
    use crate::modules::registers::PicRegister;

    #[test]
    fn get_bit_status() {
        let mut register = get_defaulted_active_register();
        register.bits_content = 0b10101010;

        assert!(!register.get_bit_status(0));
        assert!(register.get_bit_status(1));
        assert!(!register.get_bit_status(2));
        assert!(register.get_bit_status(3));
        assert!(!register.get_bit_status(4));
        assert!(register.get_bit_status(5));
        assert!(!register.get_bit_status(6));
        assert!(register.get_bit_status(7));
    }
    
    #[test]
    fn set_bit() {
        let mut register = get_defaulted_active_register();
        register.set_bit(1);
        register.set_bit(4);
        register.set_bit(5);
        
        assert!(!register.get_bit_status(0));
        assert!(register.get_bit_status(1));
        assert!(!register.get_bit_status(2));
        assert!(!register.get_bit_status(3));
        assert!(register.get_bit_status(4));
        assert!(register.get_bit_status(5));
        assert!(!register.get_bit_status(6));
        assert!(!register.get_bit_status(7));
    }
    
    #[test]
    fn reset_bit() {
        let mut register = get_defaulted_active_register();
        register.bits_content = 0xFF;
        register.reset_bit(2);
        register.reset_bit(3);
        register.reset_bit(6);
        register.reset_bit(7);
        
        dbg!(&register.bits_content);

        assert!(register.get_bit_status(0));
        assert!(register.get_bit_status(1));
        assert!(!register.get_bit_status(2));
        assert!(!register.get_bit_status(3));
        assert!(register.get_bit_status(4));
        assert!(register.get_bit_status(5));
        assert!(!register.get_bit_status(6));
        assert!(!register.get_bit_status(7));
    }
    
    #[test]
    fn set_value() {
        let mut register = get_defaulted_active_register();
        register.set_value(0x69);

        assert_eq!(register.get_value(), 0x69);
    }
    
    fn get_defaulted_active_register() -> PicRegister {
        PicRegister {
            address: 0x00,
            bits_content: 0x00,
            available_bits: 0xFF
        }
    }
}
