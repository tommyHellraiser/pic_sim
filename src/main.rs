use crate::modules::instructions::Instructions;

mod modules;

fn main() {
    let instruction = Instructions::from_opcode(0x1C03).unwrap();
    
    println!("{}", instruction);
}
