use std::alloc::{alloc, Layout};
use crate::{Chunk, OpCode, Value, printValue, ValueArray, compile, Compiler, compiled, As, ValueType};
use crate::object::*;
use std::ptr;
use crate::vm::InterpretResult::INTERPRET_COMPILE_ERROR;

// ip is instruction pointer,
///We use an actual real C pointer pointing right into the middle of the bytecode array instead of something
/// like an integer index because it’s faster to dereference a pointer than look up an element in an array by index

const STACk_SIZE:u32 = 256;
pub struct VM {
    ip: usize, // store as index into array
    stack: Vec<Value>,
    objects : Vec<*mut Obj>
}

impl VM {
    fn resetStack(&mut self){
        self.stack = Vec::new()
    }

    pub fn free(&mut self){
        self.ip = 0;
        for object in &self.objects {
            println!("object is {:?}", unsafe {
                let object = **object;
                object;
            })
        }

    }

    pub fn new () -> Self {
       Self {
           ip : 0,
           stack: Vec::new(),
           objects: Vec::new()
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
                // prints code offset, line number instruction name, constant offset and constant value
                {
                    for slot in &self.stack {
                                print!("          ");
                                print!("[ ");
                                printValue(slot);
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
                    if(self.stack.is_empty()){
                        return InterpretResult::INTERPRET_OK
                    }
                    let value = self.pop_stack();
                    printValue(&value);
                    value.free();
                    print!("\n");
                    return InterpretResult::INTERPRET_OK
                }
                OpCode::OP_NEGATE => {

                    /**
                     if (!IS_NUMBER(peek(0))) {
                      runtimeError("Operand must be a number.");
                      return INTERPRET_RUNTIME_ERROR;
                    }
                    push(NUMBER_VAL(-AS_NUMBER(pop())));
                     */
                    let peeked = self.peek(0);
                    match &peeked.rep {
                        As::Number(r) => {
                            let number_on_stack  = self.pop_stack().as_number();
                            self.stack.push(Value::number_value(-number_on_stack));
                        },
                        _ => {
                            self.runtime_error("Operand must be a number.",self.get_line_number(&chunk));
                            return InterpretResult::INTERPRET_RUNTIME_ERROR;
                        }
                    }

                }
                OpCode::OP_ADD => self.binaryOp(BinaryOp::ADD,chunk),
                OpCode::OP_SUBTRACT => self.binaryOp(BinaryOp::SUBTRACT,chunk),
                OpCode::OP_DIVIDE => self.binaryOp(BinaryOp::DIVIDE,chunk),
                OpCode::OP_MULTIPLY => self.binaryOp(BinaryOp::MULTIPLY,chunk),
                OpCode::OP_NIL => self.stack.push(Value::nil_value()),
                OpCode::OP_TRUE => self.stack.push(Value::bool_value(true)),
                OpCode::OP_FALSE => self.stack.push(Value::bool_value(false)),
                OpCode::OP_NOT => {
                    let popped = self.pop_stack();
                    self.stack.push(Value::bool_value(self.isFalsey(&popped)));
                }

                OpCode::OP_EQUAL => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let result = self.valuesEqual(&a,&b);
                    self.stack.push(Value::bool_value(result))
                }
                OpCode::OP_GREATER => self.binaryOp(BinaryOp::GREATER,chunk),
                OpCode::OP_LESS => self.binaryOp(BinaryOp::LESS,chunk),
            }
        }
    }

    fn valuesEqual(&self, a : &Value, b : &Value) -> bool {
        if(a.valueType != b.valueType) {
            return false
        } else {
            match a.valueType {
                ValueType::BOOL => a.as_bool() == b.as_bool(),
                ValueType::NIL => true, // a== b , nil == nil
                ValueType::NUMBER => a.as_number() == b.as_number(),
                ValueType::OBJ => {
                    match (a.rep, b.rep) {
                        (As::OBJ(objA), (As::OBJ(objB))) => {
                            match (objA,objB) {
                                (Obj::STRING(ObjString { length, ptr }),  Obj::STRING(ObjString {length : lengthB,ptr : ptrB})) => {
                                    unsafe {
                                        std::slice::from_raw_parts(ptr, length) == std::slice::from_raw_parts(ptrB, lengthB)
                                    }
                                },
                                _ => todo!()
                            }
                        },
                        _ => panic!("other representations should have been handled")
                    }
                }
            }
        }
    }
    fn pop_stack(&mut self) -> Value {
        let value = self.stack.pop().unwrap();
        // match value {
        //     Value {valueType, rep} => {
        //         match rep {
        //             As::OBJ(mut p@Obj::ObjString {..}) => {
        //                 let object_ptr: *mut Obj = &mut p;
        //                 println!("writing string: {:?}, {:?}",p,object_ptr);
        //                 self.objects.push(object_ptr);
        //
        //             }
        //             _ => ()
        //         }
        //     }
        //     _ => ()
        // }

        value
    }
    fn isFalsey(&self, value: &Value) -> bool {
        match value.valueType {
            ValueType::NIL => true,
            ValueType::BOOL =>  !value.as_bool(),
            _ => true

        }
      //return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
    }
    fn get_line_number(&self, chunk : &Chunk) -> usize {
        let instruction = chunk.code[self.ip];
         chunk.lines[instruction as usize]
    }
    fn peek(&self,index : usize) -> &Value {
        &self.stack[self.stack.len() - index -1]
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

    fn binaryOp(&mut self, binaryOp : BinaryOp,chunk : &Chunk) {
        // we put the first pop to b and second to a
        // because the left value is put into the stack first before the right,
        // thus, the right is popped first
        let _b = self.stack.pop().unwrap().rep;
        let _a  = self.stack.pop().unwrap().rep;

        match (_a,_b) {
            (As::Number(a),As::Number(b)) =>  {
                let result : Result<f64,bool> = match binaryOp { // result not the best thing, but helps keep code DRY
                    BinaryOp::ADD => Ok(a + b),
                    BinaryOp::SUBTRACT => Ok(a - b),
                    BinaryOp::DIVIDE => Ok(a / b),
                    BinaryOp::MULTIPLY => Ok(a * b),
                    BinaryOp::GREATER => Err(a > b),
                    BinaryOp::LESS => Err(a < b),

                };
                match result {
                    Ok(r) =>  self.stack.push(Value::number_value(r)),
                    Err(b) =>  self.stack.push(Value::bool_value(b)),
                }

            },
            (As::OBJ(first @Obj::STRING(ObjString {length, ptr})), As::OBJ(second @Obj::STRING(ObjString {length: la, ptr : ptrB}))) => {
                unsafe  {
                    let str1 = std::slice::from_raw_parts(ptr,length);
                    let str2 = std::slice::from_raw_parts(ptrB,la);
                    let result = ObjString::concat_buffers(str1,str2);
                    self.stack.push(Value::obj_value(Obj::STRING(result)));
                    first.free();
                    second.free();
                }
            },
            (As::OBJ(first @Obj::STRING(ObjString {length, ptr})), As::Number(a)) => {
                unsafe  {
                    let str1 = std::slice::from_raw_parts(ptr,length);
                    let b =  format!("{}",a);
                    let result = ObjString::concat_buffers(str1,b.as_bytes());
                    self.stack.push(Value::obj_value(Obj::STRING(result)));
                    first.free();
                }
            },

            ( As::Number(a),As::OBJ(first @Obj::STRING(ObjString {length, ptr}))) => {
                self.runtime_error("Cannot concatenate a number and string",self.get_line_number(chunk));
                first.free()
            },
            _ => {
                self.runtime_error("Operands must be two numbers or two strings",self.get_line_number(chunk));
                _a.free();
                _b.free();
            }
        }
    }

    fn runtime_error(&mut self, msg : &str, line : usize) {
        /**
         static void runtimeError(const char* format, ...) {
          va_list args;
          va_start(args, format);
          vfprintf(stderr, format, args);
          va_end(args);
          fputs("\n", stderr);

          size_t instruction = vm.ip - vm.chunk->code - 1;
          int line = vm.chunk->lines[instruction];
          fprintf(stderr, "[line %d] in script\n", line);
          resetStack();
        }
         */

        eprintln!("{}",msg);
        eprint!("[line {}] in script", line);
        self.resetStack()
    }

}

#[derive(Debug)]
enum BinaryOp {
    ADD,
    SUBTRACT,
    DIVIDE,
    MULTIPLY,
    GREATER,
    LESS
}
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}