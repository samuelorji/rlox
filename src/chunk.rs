
use crate::value;
use value::*;
// each operation has a one-byte operation code e.g add, subtract e.t.c
#[derive(Debug,Clone,Copy)]
#[repr(u8)]
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_DIVIDE,
    OP_MULTIPLY

}

// NB, this  must match the order above

impl From<u8> for OpCode{
    fn from(x: u8) -> Self {
        match x {
            0 => OpCode::OP_RETURN,
            1 => OpCode::OP_CONSTANT,
            2 => OpCode::OP_NEGATE,
            3 => OpCode::OP_ADD,
            4 => OpCode::OP_SUBTRACT,
            5 => OpCode::OP_DIVIDE,
            6 => OpCode::OP_MULTIPLY,
            _ => panic!( "unknown opcode {}", x)
        }

    }
}
impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self {
       opcode as u8
    }
}

impl OpCode {
    pub fn to_u8(&self) -> u8 {
        *self as u8
    }
}

pub struct Chunk {
     pub code:  Vec<u8>,
     pub constants: ValueArray,
     pub lines : Vec<usize>
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code : vec![],
            constants: ValueArray::new(),
            lines : vec![]
        }
    }
    pub fn write(&mut self, byte : u8, line : usize) -> () {
        self.code.push(byte);
        self.lines.push(line)
    }
    pub fn free(&mut self) -> () {
        self.code = Vec::new();
        self.constants.free();
        self.lines = Vec::new();
    }
    // return the index where the constant was appended to
    pub fn addConstant(&mut self, constant : Value) -> u32 {
        self.constants.write(constant);
        self.constants.count() - 1
    }

    pub fn readConstant(&self, index : usize) -> Value {
        self.constants.read_value(index)
    }

    pub fn read(&self, index : usize) -> u8 {
        self.code[index]
    }

    pub fn disassemble(&self, chunkName : &str)  {
        print!("== {} ==\n", chunkName);

        let mut offset  = 0;
        // offset here must end up being a valid opcode else it will panic
        // we want this, cos disassemble instruction must start at an opcode
        while(offset < self.code.len()) {
            offset = self.disassembleInstruction(offset);
        }
    }

    // returns next instruction index
    pub fn disassembleInstruction(&self, offset : usize) -> usize {
        print!("{:04} ",offset);

        if(offset >0 && self.lines[offset] == self.lines[offset -1]){
            // we're on the same line in the source code
            print!("   | ")
        } else {
            print!("{:4} ",self.lines[offset])
        }

        let instruction: OpCode = self.code[offset].into();
        // all things being equal, instruction should be an opcode
        match instruction {
            OpCode::OP_CONSTANT => {
                self.constantInstruction("OP_CONSTANT",offset)
            }
            OpCode::OP_RETURN => {
                self.simpleInstruction("OP_RETURN",offset)
            }
            OpCode::OP_NEGATE => {
                self.simpleInstruction("OP_NEGATE", offset)
            }

            OpCode::OP_ADD => self.simpleInstruction("OP_ADD", offset),
            OpCode::OP_SUBTRACT => self.simpleInstruction("OP_SUBTRACT", offset),
            OpCode::OP_DIVIDE => self.simpleInstruction("OP_DIVIDE", offset),
            OpCode::OP_MULTIPLY => self.simpleInstruction("OP_MULTIPLY", offset),
        }

    }

    fn simpleInstruction(&self,instructionName : &str, offset : usize) -> usize {
        print!("{}\n", instructionName);
        offset + 1
    }

    fn constantInstruction(&self,name : &str, offset : usize) -> usize {

        // constant index is the value which represents the index in the values array where the
        // constant for this opcode lives
        let constantIndex = self.code[offset + 1];
        print!("{name:-16} {constantIndex:4} '");
        // get the value in the value array at constant index
        let value = self.constants.values[constantIndex as usize];
        printValue(value);
        print!("'\n");
        offset + 2 // this should return the location of the next opcode
    }

    fn printValue(value : Value) {
        print!("{:e}",value)
    }
}