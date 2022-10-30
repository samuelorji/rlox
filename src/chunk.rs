
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
    OP_MULTIPLY,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_PRINT,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_JUMP_IF_FALSE,
    OP_JUMP,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE

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
            7 => OpCode::OP_NIL,
            8 => OpCode::OP_TRUE,
            9 => OpCode::OP_FALSE,
            10 => OpCode::OP_NOT,
            11 => OpCode::OP_EQUAL,
            12 => OpCode::OP_GREATER,
            13 => OpCode::OP_LESS,
            14 => OpCode::OP_PRINT,
            15 => OpCode::OP_POP,
            16 => OpCode::OP_DEFINE_GLOBAL,
            17 => OpCode::OP_GET_GLOBAL,
            18 => OpCode::OP_SET_GLOBAL,
            19 => OpCode::OP_GET_LOCAL,
            20 => OpCode::OP_SET_LOCAL,
            21 => OpCode::OP_JUMP_IF_FALSE,
            22 => OpCode::OP_JUMP,
            23 => OpCode::OP_LOOP,
            24 => OpCode::OP_CALL,
            25 => OpCode::OP_CLOSURE,
            _ => panic!( "unknown opcode {}", x)
        }

    }
}

// impl TryFrom<&u8> for OpCode {
//     type Error = u8;
//
//     fn try_from(value: &u8) -> Result<Self, Self::Error> {
//         match value {
//             0 => Ok(OpCode::OP_RETURN),
//             1 => Ok(OpCode::OP_CONSTANT),
//             2 => Ok(OpCode::OP_NEGATE),
//             3 => Ok(OpCode::OP_ADD),
//             4 => Ok(OpCode::OP_SUBTRACT),
//             5 => Ok(OpCode::OP_DIVIDE),
//             6 => Ok(OpCode::OP_MULTIPLY),
//             7 => Ok(OpCode::OP_NIL),
//             8 => Ok(OpCode::OP_TRUE),
//             9 => Ok(OpCode::OP_FALSE),
//             10 => Ok(OpCode::OP_NOT),
//             11 => Ok(OpCode::OP_EQUAL),
//             12 => Ok(OpCode::OP_GREATER),
//             13 => Ok(OpCode::OP_LESS),
//             14 => Ok(OpCode::OP_PRINT),
//             15 => Ok(OpCode::OP_POP),
//             16 => Ok(OpCode::OP_DEFINE_GLOBAL),
//             17 => Ok(OpCode::OP_GET_GLOBAL),
//             18 => Ok(OpCode::OP_SET_GLOBAL),
//             19 => Ok(OpCode::OP_GET_LOCAL),
//             20 => Ok(OpCode::OP_SET_LOCAL),
//             21 => Ok(OpCode::OP_JUMP_IF_FALSE),
//             22 => Ok(OpCode::OP_JUMP),
//             x => Err(*x)
//         }
//     }
// }
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

impl Clone for Chunk {
    fn clone(&self) -> Self {
        Chunk::new()
    }
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
        println!("=====    =====");
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
            OpCode::OP_NIL => self.simpleInstruction("OP_NIL", offset),
            OpCode::OP_TRUE => self.simpleInstruction("OP_TRUE", offset),
            OpCode::OP_FALSE => self.simpleInstruction("OP_FALSE", offset),

            OpCode::OP_NOT => self.simpleInstruction("OP_NOT", offset),

            OpCode::OP_EQUAL => self.simpleInstruction("OP_EQUAL", offset),
            OpCode::OP_GREATER => self.simpleInstruction("OP_GREATER", offset),
            OpCode::OP_LESS => self.simpleInstruction("OP_LESS", offset),

            OpCode::OP_PRINT => self.simpleInstruction("OP_PRINT", offset),

            OpCode::OP_POP => self.simpleInstruction("OP_POP", offset),
            OpCode::OP_DEFINE_GLOBAL => self.constantInstruction("OP_DEFINE_GLOBAL", offset),
            OpCode::OP_GET_GLOBAL => self.constantInstruction("OP_GET_GLOBAL", offset),
            OpCode::OP_SET_GLOBAL => self.constantInstruction("OP_SET_GLOBAL", offset),

            OpCode::OP_GET_LOCAL => self.byteInstruction("OP_GET_LOCAL", offset),
            OpCode::OP_SET_LOCAL => self.byteInstruction("OP_SET_LOCAL", offset),
            OpCode::OP_JUMP_IF_FALSE =>  self.jumpInstruction("OP_JUMP_IF_FALSE", 1,offset),
            OpCode::OP_JUMP => self.jumpInstruction("OP_JUMP", 1,offset),

            OpCode::OP_LOOP => self.jumpInstruction("OP_LOOP", -1, offset),

            OpCode::OP_CALL => self.byteInstruction("OP_CALL", offset),

            OpCode::OP_CLOSURE => {
                let mut offset = offset+1;
                let constant = self.code[offset];
                offset+=1;
                print!("{:>16} {:>4} ", "OP_CLOSURE", constant);
                printValue(&self.constants.values[constant as usize]);
                print!("\n");
                return offset;
            }
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
        printValue(&value);
        print!("'\n");
        offset + 2 // this should return the location of the next opcode
    }


    // tatic int jumpInstruction(const char* name, int sign,
    //                            Chunk* chunk, int offset) {
    //   uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    //   jump |= chunk->code[offset + 2];
    //   printf("%-16s %4d -> %d\n", name, offset,
    //          offset + 3 + sign * jump);
    //   return offset + 3;
    // }

    fn jumpInstruction(&self,name : &str, sign: isize,  offset : usize) -> usize {
        let mut jump : u16 = ((self.code[offset + 1]) as u16 ) << 8 ;
         jump |= (self.code[offset + 2]) as u16;

        println!("{name:-16} {offset:-4} -> {}", ((offset + 3) as isize + (sign * jump as isize)) as usize);

        offset + 3
    }
    fn byteInstruction(&self,name : &str, offset : usize) -> usize {

        let slot = self.code[offset + 1];
        println!("{:-16} {:-4}", name, slot);
        return offset + 2;
    }

    fn printValue(value : Value) {
        print!("{:?}",value)
    }
}