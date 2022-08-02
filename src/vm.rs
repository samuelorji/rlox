use crate::{Chunk, OpCode, Value, printValue, ValueArray, compile, Compiler, compiled};
use crate::vm::InterpretResult::INTERPRET_COMPILE_ERROR;

// ip is instruction pointer,
///We use an actual real C pointer pointing right into the middle of the bytecode array instead of something
/// like an integer index because it’s faster to dereference a pointer than look up an element in an array by index

const STACk_SIZE:u32 = 256;
pub struct VM {
    ip: usize, // store as index into array
    stack: Vec<Value>
}

impl VM {
    fn resetStack(&mut self){
        self.stack = Vec::new()
    }

    pub fn free(&mut self){
        self.resetStack()

    }

    pub fn new () -> Self {
       Self {
           ip : 0,
           stack: Vec::new()
       }
    }

    // pub fn interpret(&mut self, chunk: &Chunk) -> InterpretResult {
    //     self.run(chunk)
    // }

    pub fn interpret(&mut self, source : Vec<u8>) -> InterpretResult {

        /**
         Chunk chunk;
        initChunk(&chunk);

        if (!compile(source, &chunk)) {
          freeChunk(&chunk);
          return INTERPRET_COMPILE_ERROR;
        }

        vm.chunk = &chunk;
        vm.ip = vm.chunk->code;

        InterpretResult result = run();

        freeChunk(&chunk);
        return result;

         */

        // compiled(source);
        // InterpretResult::INTERPRET_OK



        let mut chunk  = Chunk::new();
        let mut compiler = Compiler::new(&source,&mut chunk);



        if(!compiler.compile()){
            chunk.free();
            InterpretResult::INTERPRET_COMPILE_ERROR
        } else {
            let result = self.run(&chunk);
            chunk.free();
            result
        }
    }
    fn run(&mut self, chunk : &Chunk) -> InterpretResult{
        loop {
            #[cfg(feature = "debug_trace_execution")]
                {
                    for slot in &self.stack {
                                print!("          ");
                                print!("[ ");
                                printValue(*slot);
                                print!(" ]");
                            }
                            print!("\n");
                    chunk.disassembleInstruction(self.ip);
                }
            match self.readByte(chunk) {
                OpCode::OP_CONSTANT => {
                    let constant = self.readConstant(chunk);
                    self.stack.push(constant);
                }
                OpCode::OP_RETURN => {
                    printValue(self.stack.pop().unwrap());
                    print!("\n");
                    return InterpretResult::INTERPRET_OK
                }
                OpCode::OP_NEGATE => {
                    let constant = self.stack.pop().unwrap();
                    self.stack.push(-constant);
                }
                OpCode::OP_ADD => self.binaryOp(BinaryOp::ADD),
                OpCode::OP_SUBTRACT => self.binaryOp(BinaryOp::SUBTRACT),
                OpCode::OP_DIVIDE => self.binaryOp(BinaryOp::DIVIDE),
                OpCode::OP_MULTIPLY => self.binaryOp(BinaryOp::MULTIPLY),
            }
        }
    }

    fn readByte(&mut self, chunk : &Chunk) -> OpCode {
        let val : OpCode = chunk.read(self.ip).into();
        self.ip +=1;
        val

    }

    fn readConstant(&mut self, chunk : &Chunk) -> Value {
        let indexOfConstant = chunk.read(self.ip);
        self.ip += 1;
        chunk.readConstant(indexOfConstant as usize)
    }

    fn binaryOp(&mut self, binaryOp : BinaryOp) {
        // we put the first pop to b and second to a
        // because the left value is put into the stack first before the right,
        // thus, the right is popped first
        let b = self.stack.pop().unwrap();
        let a  = self.stack.pop().unwrap();
        let result = match binaryOp {
            BinaryOp::ADD => a + b,
            BinaryOp::SUBTRACT => a - b,
            BinaryOp::DIVIDE => a / b,
            BinaryOp::MULTIPLY => a * b,
        };

        self.stack.push(result);
    }


}

enum BinaryOp {
    ADD,
    SUBTRACT,
    DIVIDE,
    MULTIPLY
}
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}