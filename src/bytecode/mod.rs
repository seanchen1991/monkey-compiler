use byteorder::ByteOrder;
use byteorder::{BigEndian};
use std::collections::HashMap;

pub type Opcode = u16;

iota! {
    // Use iota here because we want opcodes values
    // to increment, but we don't care what values they are
    const OpConstant: u16 = 1 << iota;
}

pub struct Instruction(Vec<u16>);

#[derive(Debug, Hash)]
pub struct Definition {
    name: String,
    operand_widths: Vec<usize>,
}

// hashmap to hold Opcode keys to Definition values
pub struct Definitions(HashMap<Opcode, Definition>);

impl Definitions {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(OpConstant, Definition { name: "OpConstant".to_string(), operand_widths: vec![2 as usize] });
    
        Definitions(map)
    }
    
    pub fn lookup(&self, op: u16) -> Option<&Definition> {
        let Definitions(map) = self;

        match map.get(&op) {
            Some(ref def) => Some(def),
            None => {
                println!("Opcode {} is undefined", op);
                None
            }
        }
    }

    pub fn make_instruction(&self, op: Opcode, operands: &[i64]) -> Option<Instruction> {
        match self.lookup(op) {
            Some(def) => {
                let instruction_len: usize = def.operand_widths.iter().sum();
                let mut instruction = vec![0 as u16; instruction_len + 1];
                instruction[0] = op;

                let mut offset = 1;

                for (index, operand) in operands.iter().enumerate() {
                    let width = def.operand_widths[index];

                    match width {
                        2 => {
                            let o = *operand as u16;
                            instruction[offset] = o.to_be();
                        },
                        _ => println!("Width: {}", width),
                    }

                    offset += width;
                }
                
                Some(Instruction(instruction))
            },
            None => {
                println!("Couldn't find instruction for opcode {}", op);
                None
            }
        }
    }
}

#[cfg(test)]
mod tests;