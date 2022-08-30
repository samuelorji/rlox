use std::alloc::{alloc, Layout};
use crate::{Chunk, OpCode, Value, printValue, ValueArray, compile, Compiler, compiled, As, ValueType, Table};
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
    objects : Vec<Value>,
    table : Table,
    pub strings : Table
}

impl VM {
    fn resetStack(&mut self){
        self.stack = Vec::new()
    }

    pub fn free(&mut self){

        println!("table is \n{:?}",&self.table);
        println!("strings are \n{:?}",&self.strings);
        for object in self.objects.iter_mut() {
            println!("object is {:?}",&object);
            object.free()
        }

         self.table.free();
         self.strings.free();


    }

    pub fn new () -> Self {
       Self {
           ip : 0,
           stack: Vec::new(),
           objects: Vec::new(),
           table: Table::new(),
           strings: Table::new()
       }
    }


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
        let mut compiler = Compiler::new(&source,&mut chunk,self);



        if(!compiler.compile()){
            chunk.free();
            InterpretResult::INTERPRET_COMPILE_ERROR
        } else {
            let result = self.run(&chunk);
            chunk.free();
            result
        }
    }

    fn add_value(&mut self, value : Value) {
        self.objects.push(value);
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
                    self.objects.push(value);
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
                    let result = &a == &b;
                    self.stack.push(Value::bool_value(result));
                    // self.objects.push(a);
                    // self.objects.push(b);
                }
                OpCode::OP_GREATER => self.binaryOp(BinaryOp::GREATER,chunk),
                OpCode::OP_LESS => self.binaryOp(BinaryOp::LESS,chunk),

                OpCode::OP_PRINT => {
                    let to_be_printed = self.pop_stack();
                    printValue(&to_be_printed);
                    println!("");
                    //self.objects.push(to_be_printed)
                },

                OpCode::OP_POP =>  {
                    if(!self.stack.is_empty()) {
                        self.pop_stack();
                    }
                },

                OpCode::OP_DEFINE_GLOBAL => {
                    // stack will
                    let constant = self.readConstant(chunk);
                    let variable_name = constant.as_obj_string();
                    println!("variable name is {:?}",variable_name.as_str_debug());
                    let variable_value = self.pop_stack();
                    self.table.set(variable_name, variable_value);

                },

                OpCode::OP_GET_GLOBAL => {
                    let constant = self.readConstant(chunk);
                    let variable_name = constant.as_obj_string();
                    match self.table.get(&variable_name) {
                       None => {
                           self.runtime_error(&format!("Undefined variable '{:?}'.", &variable_name), self.get_line_number(chunk));
                           return InterpretResult::INTERPRET_RUNTIME_ERROR;
                       }
                       Some(value) => {

                           let cloned_val = value.soft_clone();
                          // let cloned_val = value.clone();
                           //println!("cloned value is {}",&cloned_val.as_obj_string().as_str_debug());
                           self.stack.push(cloned_val);
                       }
                   }
                   // self.objects.push(constant);
                }
            }
        }
    }

    fn pop_stack(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
    fn isFalsey(&self, value: &Value) -> bool {
        match value.valueType {
            ValueType::NIL => true,
            ValueType::BOOL =>  !value.as_bool(),
            _ => true

        }
    }
    fn get_line_number(&self, chunk : &Chunk) -> usize {
        let instruction = chunk.code[self.ip];
         //chunk.lines[instruction as usize - 1]
         chunk.lines[0]
    }
    fn peek(&self,index : usize) -> &Value {
        &self.stack[self.stack.len() - index -1]
    }
    fn readByte(&mut self, chunk : &Chunk) -> OpCode {
        let val : OpCode = chunk.read(self.ip).into();
        self.ip +=1;
        val

    }

    // firstly read the constant index from the chunk
    // using the instruction pointer, then read the constant
    // at the constant index
    fn readConstant(&mut self, chunk : &Chunk) -> Value {
        let indexOfConstant = chunk.read(self.ip);
        self.ip += 1;
        chunk.readConstant(indexOfConstant as usize)
    }

    fn binaryOp(&mut self, binaryOp : BinaryOp,chunk : &Chunk) {
        // we put the first pop to b and second to a
        // because the left value is put into the stack first before the right,
        // thus, the right is popped first
        let _b = self.stack.pop().unwrap();
        let _a  = self.stack.pop().unwrap();

        match (_a.rep,_b.rep) {
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
            (As::OBJ( Obj::STRING(first @ ObjString {length, ptr, ..})), As::OBJ( Obj::STRING(second @ ObjString {length: la, ptr : ptrB, ..}))) => {
                unsafe  {
                    let str1 = std::slice::from_raw_parts(ptr,length);
                    let str2 = std::slice::from_raw_parts(ptrB,la);
                    //println!("str1 is {:?}\n and str2 is {:?}",std::str::from_utf8(str1),std::str::from_utf8(str2) );
                    let result = ObjString::concat_buffers(str1,str2);
                    let cloned_result = result.clone();
                    self.strings.set(result, Value::nil_value());
                    self.stack.push(Value::obj_value(Obj::STRING(cloned_result)));

                    // self.objects.push(_a);
                    // self.objects.push(_b);
                    // first.free();
                    // second.free();
                }
            },
            (As::OBJ(Obj::STRING(first @ObjString {length, ptr, ..})), As::Number(a)) => {
                unsafe  {
                    let str1 = std::slice::from_raw_parts(ptr,length);
                    let b =  format!("{}",a);
                    let result = ObjString::concat_buffers(str1,b.as_bytes());
                    let cloned_result = result.clone();
                    self.strings.set(result, Value::nil_value());
                    self.stack.push(Value::obj_value(Obj::STRING(cloned_result)));
                   // self.objects.push(_a);
                }
            },

            ( As::Number(a),As::OBJ(Obj::STRING(ObjString {..}))) => {
                self.runtime_error("Cannot concatenate a number and string",self.get_line_number(chunk));
                //self.objects.push(_a);
            },
            _ => {
                self.runtime_error("Operands must be two numbers or two strings",self.get_line_number(chunk));
                //self.objects.push(_a);
                //self.objects.push(_b);
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