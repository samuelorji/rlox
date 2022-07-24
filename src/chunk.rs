
use crate::value;
use value::*;
// each operation has a one-byte operation code e.g add, subtract e.t.c
#[derive(Debug,Clone,Copy)]
#[repr(u8)]
pub enum OpCode {
    OP_CONSTANT,
    OP_NEGATE,
    OP_RETURN,
}

// NB, this  must match the order above

impl From<u8> for OpCode{
    fn from(x: u8) -> Self {


        match x {
            0 => OpCode::OP_CONSTANT,
            1 => OpCode::OP_NEGATE,
            2 => OpCode::OP_RETURN,
            _ => panic!( "unknown opcode {}", x)
        }

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
    lines : Vec<u32>
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code : vec![],
            constants: ValueArray::new(),
            lines : vec![]
        }
    }
    pub fn write(&mut self, byte : u8, line : u32) -> () {
        self.code.push(byte);
        self.lines.push(line)
    }
    pub fn free(&mut self) -> () {
        self.code.clear();
        self.code.resize(0,0);
        self.constants.free();
        self.lines.clear();
        self.lines.resize(0,0);
    }
    // return the index where the constant was appended to
    pub fn addConstant(&mut self, constant : Value) -> u32 {
        self.constants.write(constant);
        self.constants.count() - 1
    }

    pub fn disassemble(&self, chunkName : &str)  {
        print!("== {} ==\n", chunkName);


        let mut offset  = 0;
        // offset here must end up being a valid opcode else it will panic
        // we want this, cos disassemble instruction must start at an opcode
        while(offset < self.code.len()) {
            offset = Self::disassembleInstruction(&self, offset);
        }
    }

    // returns next instruction index
    pub fn disassembleInstruction(chunk : &Chunk, offset : usize) -> usize {
        print!("{:04} ",offset);

        if(offset >0 && chunk.lines[offset] == chunk.lines[offset -1]){
            // we're on the same line in the source code
            print!("   | ")
        } else {
            print!("{:4} ",chunk.lines[offset])
        }

        let instruction = chunk.code[offset];
        // all things being equal, instruction should be an opcode
        match OpCode::from(instruction) {
            OpCode::OP_CONSTANT => {
                Self::constantInstruction("OP_CONSTANT",chunk,offset)

            }
            OpCode::OP_RETURN => {
                Self::simpleInstruction("OP_RETURN",offset)
            }
            OpCode::OP_NEGATE => {
             Self::simpleInstruction("OP_NEGATE", offset)
            }
            _ => {
                println!("Unknown opcode {:?}",instruction);
                 offset + 1
            }
        }

    }

    fn simpleInstruction(instructionName : &str, offset : usize) -> usize {
        print!("{}\n", instructionName);
        offset + 1
    }

    fn constantInstruction(name : &str, chunk: &Chunk,offset : usize) -> usize {

        // constant index is the value which represents the index in the values array where the
        // constant for this opcode lives
        let constantIndex = chunk.code[offset + 1];
        print!("{:-16} {:4} '", name, constantIndex);
        // get the value in the value array at constant index
        let value = chunk.constants.values[constantIndex as usize];
        printValue(value);
        print!("'\n");
        offset + 2 // this should return the location of the next opcode
    }

    fn printValue(value : Value) {
        print!("{:e}",value)
    }
}