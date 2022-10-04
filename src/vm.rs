use std::alloc::{alloc, Layout};
use crate::{Chunk, OpCode, printValue, ValueArray, compile, Compiler, compiled, Value, Table};
use crate::object::*;
use std::ptr;
use std::thread::sleep;
use crate::vm::InterpretResult::INTERPRET_COMPILE_ERROR;

// ip is instruction pointer,
///We use an actual real C pointer pointing right into the middle of the bytecode array instead of something
/// like an integer index because it’s faster to dereference a pointer than look up an element in an array by index


const FRAME_MAX: u8 = 64;
const STACK_MAX: usize = (FRAME_MAX as usize * u8::MAX as usize);

#[derive(Copy, Clone)]
struct CallFrame {
    function: ObjFunction,
    ip: usize,
    slot: usize, // index into what slot is used
}

impl CallFrame {
    fn new() -> Self {
        CallFrame {
            function: ObjFunction::new(),
            ip: 0,
            slot: 0,
        }
    }
}

pub struct VM {
    frames: [CallFrame; FRAME_MAX as usize],
    frameCount: usize,
    // ip: usize, // store as index into array
    stack: Vec<Value>,
    currentChunk: *mut Chunk,
    currentFrame: *mut CallFrame,
    objects: Vec<Value>,
    globals: Table,
    pub strings: Table,
    pub functionChunks: Vec<Chunk>,
}


impl VM {
    fn resetStack(&mut self) {
        self.stack.iter_mut().for_each(|value| value.free());
        self.stack = vec![];
        self.frameCount = 0;
    }

    pub fn free(&mut self) {

        //println!("table is \n{:?}",&self.globals);
        //println!("strings are \n{:?}",&self.strings);
        for object in self.objects.iter_mut() {
            // println!("object is {:?}",&object);
            object.free()
        }

        self.globals.free();
        self.strings.free();
        self.functionChunks.iter_mut()
            .for_each(|chunk| chunk.free());
    }

    pub fn new() -> Self {
        Self {
            frames: [CallFrame::new(); FRAME_MAX as usize],
            frameCount: 0,
            stack: Vec::new(),
            currentChunk: ptr::null_mut(),
            currentFrame: ptr::null_mut(),
            objects: Vec::new(),
            globals: Table::new(),
            strings: Table::new(),
            functionChunks: vec![],
        }
    }


    pub fn interpret(&mut self, source: Vec<u8>) -> InterpretResult {
        // ObjFunction* function = compile(source);
        //   if (function == NULL) return INTERPRET_COMPILE_ERROR;
        //
        //   push(OBJ_VAL(function));
        //   CallFrame* frame = &vm.frames[vm.frameCount++];
        //   frame->function = function;
        //   frame->ip = function->chunk.code;
        //   frame->slots = vm.stack;


        let mut chunk = Chunk::new();
        self.functionChunks.push(chunk);
        let funcChunk = &mut self.functionChunks[0];
        let funcChunkPointer = funcChunk as *mut Chunk;
        let mut compiler = Compiler::new(&source, self);
        let compile_result = compiler.compile();

        match compile_result {
            None => return INTERPRET_COMPILE_ERROR,
            Some(function) => {
                self.stack.push(Value::obj_value(Obj::FUNCTION(function)));
                let callframe = &mut self.frames[self.frameCount];

                self.currentChunk = funcChunkPointer;
                callframe.ip = 0;
                callframe.slot = 0;

                self.currentFrame = callframe as *mut CallFrame;

                self.run()
            }
        }
       // InterpretResult::INTERPRET_OK
        //
        // let result =  if(!compiler.compile()){
        //
        //
        //      InterpretResult::INTERPRET_COMPILE_ERROR
        //  } else {
        //      let result = self.run(&chunk);
        //      chunk.free();
        //      result
        //  };
        //
        //  self.ip = 0;
        //  result
    }

    fn add_value(&mut self, value: Value) {
        self.objects.push(value);
    }
    fn run(&mut self) -> InterpretResult {
        // self.currentFrame = &mut self.frames[(self.frameCount - 1) as usize];
        loop {
            #[cfg(feature = "debug_trace_execution")]
                // prints code offset, line number instruction name, constant offset and constant value
                {
                    let chunk = self.currentChunk;
                    for slot in &self.stack {
                        print!("          ");
                        print!("[ ");
                        printValue(slot);
                        print!(" ]");
                    }
                    print!("\n");

                    unsafe { (*chunk).disassembleInstruction(self.frames[self.frameCount].ip) };
                }
            match self.readOpCode() {
                OpCode::OP_CONSTANT => {
                    let constant = self.readConstant();
                    self.stack.push(constant);
                }
                OpCode::OP_RETURN => {
                    if (self.stack.is_empty()) {
                        return InterpretResult::INTERPRET_OK;
                    }
                    let value = self.pop_stack();
                    self.objects.push(value);
                    return InterpretResult::INTERPRET_OK;
                }
                OpCode::OP_NEGATE => {
                    let peeked = self.peek(0);
                    println!("peeked is {:?}", &peeked);
                    match &peeked {
                        Value::Number(r) => {
                            let number_on_stack = self.pop_stack().as_number();
                            self.stack.push(Value::number_value(-number_on_stack));
                        }
                        _ => {
                            self.runtime_error("Operand must be a number.", self.get_line_number());
                            return InterpretResult::INTERPRET_RUNTIME_ERROR;
                        }
                    }
                }
                OpCode::OP_ADD => self.binaryOp(BinaryOp::ADD),
                OpCode::OP_SUBTRACT => self.binaryOp(BinaryOp::SUBTRACT),
                OpCode::OP_DIVIDE => self.binaryOp(BinaryOp::DIVIDE),
                OpCode::OP_MULTIPLY => self.binaryOp(BinaryOp::MULTIPLY),
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
                }
                OpCode::OP_GREATER => self.binaryOp(BinaryOp::GREATER),
                OpCode::OP_LESS => self.binaryOp(BinaryOp::LESS),

                OpCode::OP_PRINT => {
                    let to_be_printed = self.pop_stack();
                    printValue(&to_be_printed);
                    println!("");
                }

                OpCode::OP_POP => {
                    if (!self.stack.is_empty()) {
                        self.pop_stack();
                    }
                }

                OpCode::OP_DEFINE_GLOBAL => {
                    // stack will
                    let constant = self.readConstant();
                    let variable_name = constant.as_obj_string();
                    let variable_value = self.pop_stack();
                    self.globals.set(variable_name, variable_value);
                }

                OpCode::OP_GET_GLOBAL => {
                    let constant = self.readConstant();
                    let variable_name = constant.as_obj_string();
                    match self.globals.get(&variable_name) {
                        None => {
                            self.runtime_error(&format!("Undefined variable '{:?}'.", &variable_name), self.get_line_number());
                            return InterpretResult::INTERPRET_RUNTIME_ERROR;
                        }
                        Some(value) => {
                            let cloned_val = value.soft_clone();
                            // let cloned_val = value.clone();
                            //println!("cloned value is {}",&cloned_val.as_obj_string().as_str_debug());
                            self.stack.push(cloned_val);
                        }
                    }
                }

                OpCode::OP_SET_GLOBAL => {
                    let name = self.readConstant().as_obj_string();
                    let cloned_name = name.clone();
                    if (self.globals.set(name, self.peek(0).clone())) {
                        self.globals.remove(&cloned_name);
                        self.runtime_error(&format!("Undefined variable '{:?}'.", &cloned_name), self.get_line_number());
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    }
                }

                OpCode::OP_GET_LOCAL => {
                    let localvariableIndex = self.readByte();

                    // slot for this local variable, is slot of the frame (where the frame started)
                    // plus the offset from that slot

                    let slot = unsafe { (*self.currentFrame).slot + localvariableIndex as usize };

                    let localvariable = self.stack[slot];
                    self.stack.push(localvariable)
                }
                OpCode::OP_SET_LOCAL => {
                    let localvariableIndex = self.readByte();
                    let slot = unsafe { (*self.currentFrame).slot + localvariableIndex as usize };
                    self.stack[slot] = self.peek(0).clone();
                }

                OpCode::OP_JUMP_IF_FALSE => {
                    let jump = self.read_16_bit_short();
                    // println!("jump is {}", &jump);
                    unsafe {
                        //println!("current frame ip is {}, jump :{}",  (*self.currentFrame).ip, jump as usize );
                        // check if what's on stack is true or not
                        if self.isFalsey(self.peek(0)) {
                            // if false ... add jump to ip
                            (*self.currentFrame).ip += jump as usize
                        }
                    }
                }

                OpCode::OP_JUMP => {
                    let jump = self.read_16_bit_short();
                    //println!("jump is {}",&jump);
                    unsafe {
                        (*self.currentFrame).ip += jump as usize
                    }
                }
                OpCode::OP_LOOP => {
                    let jump = self.read_16_bit_short();
                    unsafe { (*self.currentFrame).ip -= jump as usize }
                }
            }
        }
    }

    fn pop_stack(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
    fn isFalsey(&self, value: &Value) -> bool {
        match value {
            Value::Number(0f64) => true,
            Value::Bool(boolean) => !boolean,
            _ => true
        }
    }
    fn get_line_number(&self) -> usize {
        let chunk = self.currentChunk;
        // let instruction = chunk.code[self.ip];
        //  //chunk.lines[instruction as usize - 1]
        //  chunk.lines[0]
        0
    }

    fn read_16_bit_short(&mut self) -> u16 {
        let chunk = self.currentChunk;
        unsafe {
            let first_half_of_16_bit_jump = (*chunk).code[(*self.currentFrame).ip];
            let second_half_of_16_bit_jump = (*chunk).code[(*self.currentFrame).ip + 1];
            (*self.currentFrame).ip += 2;
            ((first_half_of_16_bit_jump as u16) << 8) | (second_half_of_16_bit_jump as u16)
        }
    }
    fn peek(&self, index: usize) -> &Value {
        &self.stack[self.stack.len() - index - 1]
    }
    fn readOpCode(&mut self) -> OpCode {
        self.readByte().into()
    }

    fn readByte(&mut self) -> u8 {
        unsafe {
            let chunk = self.currentChunk;
            let val: u8 = (*chunk).read((*self.currentFrame).ip);

            (*self.currentFrame).ip += 1;
            val
        }
    }

    // firstly read the constant index from the chunk
    // using the instruction pointer, then read the constant
    // at the constant index
    fn readConstant(&mut self) -> Value {
        let chunk = self.currentChunk;
        unsafe {
            let indexOfConstant = (*chunk).read((*self.currentFrame).ip);
            (*self.currentFrame).ip += 1;
            (*chunk).readConstant(indexOfConstant as usize)
        }
    }

    fn binaryOp(&mut self, binaryOp: BinaryOp) {
        let chunk = self.currentChunk;
        // we put the first pop to b and second to a
        // because the left value is put into the stack first before the right,
        // thus, the right is popped first
        let _b = self.stack.pop().unwrap();
        let _a = self.stack.pop().unwrap();

        match (_a, _b) {
            (Value::Number(a), Value::Number(b)) => {
                let result: Result<f64, bool> = match binaryOp { // result not the best thing, but helps keep code DRY
                    BinaryOp::ADD => Ok(a + b),
                    BinaryOp::SUBTRACT => Ok(a - b),
                    BinaryOp::DIVIDE => Ok(a / b),
                    BinaryOp::MULTIPLY => Ok(a * b),
                    BinaryOp::GREATER => Err(a > b),
                    BinaryOp::LESS => Err(a < b),
                };
                match result {
                    Ok(r) => self.stack.push(Value::number_value(r)),
                    Err(b) => self.stack.push(Value::bool_value(b)),
                }
            }
            (Value::OBJ(Obj::STRING(first @ ObjString { length, ptr, .. })), Value::OBJ(Obj::STRING(second @ ObjString { length: la, ptr: ptrB, .. }))) => {
                unsafe {
                    let str1 = std::slice::from_raw_parts(ptr, length);
                    let str2 = std::slice::from_raw_parts(ptrB, la);
                    //println!("str1 is {:?}\n and str2 is {:?}",std::str::from_utf8(str1),std::str::from_utf8(str2) );
                    let result = ObjString::concat_buffers(str1, str2);
                    let cloned_result = result.clone();
                    self.strings.set(result, Value::nil_value());
                    self.stack.push(Value::obj_value(Obj::STRING(cloned_result)));

                    // self.objects.push(_a);
                    // self.objects.push(_b);
                    // first.free();
                    // second.free();
                }
            }
            (Value::OBJ(Obj::STRING(first @ ObjString { length, ptr, .. })), Value::Number(a)) => {
                unsafe {
                    let str1 = std::slice::from_raw_parts(ptr, length);
                    let b = format!("{}", a);
                    let result = ObjString::concat_buffers(str1, b.as_bytes());
                    let cloned_result = result.clone();
                    self.strings.set(result, Value::nil_value());
                    self.stack.push(Value::obj_value(Obj::STRING(cloned_result)));
                    // self.objects.push(_a);
                }
            }

            (Value::Number(a), Value::OBJ(Obj::STRING(ObjString { .. }))) => {
                self.runtime_error("Cannot concatenate a number and string", self.get_line_number());
                //self.objects.push(_a);
            }
            _ => {
                self.runtime_error("Operands must be two numbers or two strings", self.get_line_number());
                //self.objects.push(_a);
                //self.objects.push(_b);
            }
        }
    }

    fn runtime_error(&mut self, msg: &str, line: usize) {
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

        eprintln!("{}", msg);
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
    LESS,
}

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}