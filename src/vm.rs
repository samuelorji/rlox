use crate::{Chunk, OpCode, Value, printValue, ValueArray, StackArray};
use crate::vm::InterpretResult::INTERPRET_COMPILE_ERROR;

// ip is instruction pointer,
///We use an actual real C pointer pointing right into the middle of the bytecode array instead of something
/// like an integer index because it’s faster to dereference a pointer than look up an element in an array by index

const STACk_SIZE:u32 = 256;
pub struct VM {
    pub chunk : Chunk,
    ip: usize, // store as index into array
    stack: StackArray
}

impl VM {

    pub fn debugTraceExecution(&mut self) {

        /**
         printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
          printf("[ ");
          printValue(*slot);
          printf(" ]");
        }
        printf("\n");
         */

        for slot in &self.stack.values {
            print!("          ");
            print!("[ ");
            printValue(*slot);
            print!(" ]");
        }
        print!("\n");

        Chunk::disassembleInstruction(&self.chunk, &self.chunk.code.len() -1);
    }
    // pub fn init() -> Self {
    //     Self{
    //         chunk : Chunk::new(),
    //         ip: 0,
    //         stack : StackArray::new()
    //     }
    // }

    fn resetStack(&mut self){
        self.stack.free()
    }

    pub fn free(){

    }

    pub fn new () -> Self {
       Self {
           chunk : Chunk::new(),
           ip : 0,
           stack: StackArray::new()
       }
    }

    pub fn interpret(&mut self) -> InterpretResult {

        let stuff = &self.chunk.code[0] ;
        self.ip = stuff.clone() as usize;

        self.run()

     //   todo!()


    }

    fn run(&mut self) -> InterpretResult{
        loop {
            let instruction = self.readByte();
            match OpCode::from(*instruction) {
                OpCode::OP_CONSTANT => {
                    let constant = self.readConstant();
                    self.stack.push(constant);

                }
                OpCode::OP_RETURN => {
                    printValue(self.stack.pop());
                    print!("\n");
                    return InterpretResult::INTERPRET_OK
                }
                OpCode::OP_NEGATE => {
                    let constant = self.stack.pop();
                    self.stack.push(-constant);
                }
                x => panic!("not known opcode {:?}",x)
            }
        }

        InterpretResult::INTERPRET_OK

    }

    fn readByte<'a>(&'a mut self) -> &'a u8 {
        let index = self.readAndIncrementIp();
        &self.chunk.code[index]

    }

    fn readAndIncrementIp(&mut self) -> usize {
        let index = self.ip;
        self.ip+=1;
        index
    }

    fn readConstant(&mut self) -> Value {
        let indexOfConstant = *(self.readByte()) as usize;
        self.chunk.constants.values[indexOfConstant]

    }

}

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}


// impl VM {
//
//
//
//     // pub fn readByte(&mut self) -> &u8 {
//     //     let instruction = self.ip;
//     //     self.ip = &(self.ip + 1);
//     //     instruction
//     // }
// }
