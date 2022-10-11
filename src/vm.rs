use std::alloc::{alloc, Layout};
use crate::{Chunk, OpCode, printValue, ValueArray, compile, Compiler, compiled, Value, Table};
use crate::object::*;
use std::ptr;
use std::thread::sleep;
use crate::vm::InterpretResult::INTERPRET_COMPILE_ERROR;

// ip is instruction pointer,
///We use an actual real C pointer pointing right into the middle of the bytecode array instead of something
/// like an integer index because it’s faster to dereference a pointer than look up an element in an array by index


const FRAMES_MAX: u8 = 64;
const STACK_MAX: usize = (FRAMES_MAX as usize * u8::MAX as usize);

#[derive(Copy, Clone, Debug)]
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
    frames: [CallFrame; FRAMES_MAX as usize],
    frameCount: usize,
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

        // println!("table is \n{:?}",&self.globals);
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
       let mut vm=  Self {
            frames: [CallFrame::new(); FRAMES_MAX as usize],
            frameCount: 0,
            stack: Vec::new(),
            currentChunk: ptr::null_mut(),
            currentFrame: ptr::null_mut(),
            objects: Vec::new(),
            globals: Table::new(),
            strings: Table::new(),
            functionChunks: vec![],
        };

        let clock = ObjString::from_str("clock");
        let clockClone = clock.clone();
        vm.strings.set(clock, Value::empty());
        vm.define_native(clockClone);
        vm
    }

    fn define_native(&mut self, name : ObjString){
        let native_fn = Value::native_fn(name);
        self.globals.set(name, native_fn);

    }


    pub fn interpret(&mut self, source: Vec<u8>) -> InterpretResult {
        let mut chunk = Chunk::new();
        self.functionChunks.push(chunk);
        let mut compiler = Compiler::new(&source, self);
        let compile_result = compiler.compile();

        match compile_result {
            None => return INTERPRET_COMPILE_ERROR,
            Some(function) => {
                self.stack.push(Value::obj_value(Obj::FUNCTION(function)));
                self.call(function, 0);
                self.run()
                //InterpretResult::INTERPRET_OK
            }
        }

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
                    unsafe { (*chunk).disassembleInstruction((*self.currentFrame).ip) };
                }
            match self.readOpCode() {
                OpCode::OP_CONSTANT => {
                    let constant = self.readConstant();
                    self.stack.push(constant);
                }
                OpCode::OP_RETURN => {
                    // Value result = pop();
                    //         vm.frameCount--;
                    //         if (vm.frameCount == 0) {
                    //           pop();
                    //           return INTERPRET_OK;
                    //         }
                    //
                    //         vm.stackTop = frame->slots;
                    //         push(result);
                    //         frame = &vm.frames[vm.frameCount - 1];
                    //         break;
                    unsafe {
                        //println!("return called");

                        let functionReturnResult = self.pop_stack();
                        self.frameCount -= 1;
                        if (self.frameCount == 0) {
                            // at this point we have the <script> on the stack, so
                            // pop that and end
                            self.pop_stack();
                            return InterpretResult::INTERPRET_OK;
                        }

                        self.stack.resize((*self.currentFrame).slot, Value::empty());
                        self.stack.push(functionReturnResult);
                        self.currentFrame = &mut self.frames[self.frameCount - 1];
                        let currentFunctionIndex = (*self.currentFrame).function.chunkIndex as usize;
                        self.currentChunk = &mut self.functionChunks[currentFunctionIndex];
                        //println!("current frame: {:?}",(*self.currentFrame));
                        //return InterpretResult::INTERPRET_OK;
                    }
                }
                OpCode::OP_NEGATE => {
                    let peeked = self.peek(0);
                    match &peeked {
                        Value::Number(r) => {
                            let number_on_stack = self.pop_stack().as_number();
                            self.stack.push(Value::number_value(-number_on_stack));
                        }
                        _ => {
                            self.runtime_error("Operand must be a number.");
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
                            self.runtime_error(&format!("Undefined variable '{:?}'.", &variable_name));
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
                        self.runtime_error(&format!("Undefined variable '{:?}'.", &cloned_name));
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

                OpCode::OP_CALL => {
                    let argCount = self.readByte();
                    // from the stacks perspective, the callee is arg count down from the top of the stack
                    // as the callee piles the arguments on top
                    let callee = self.peek(argCount as usize).clone();
                    if (!self.callValue(callee, argCount)) {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    }
                    self.currentFrame = &mut self.frames[self.frameCount - 1];
                }
            }
        }
    }

    fn callValue(&mut self, callee: Value, argCount: u8) -> bool {
        // case OBJ_NATIVE: {
        //         NativeFn native = AS_NATIVE(callee);
        //         Value result = native(argCount, vm.stackTop - argCount);
        //         vm.stackTop -= argCount + 1;
        //         push(result);
        //         return true;
        //       }
        match callee {
            Value::OBJ(Obj::FUNCTION(function @ ObjFunction { .. })) => {
                self.call(function, argCount)
            }

            Value::OBJ(Obj::NATIVE_FUNCTION(native @ NativeFunction { .. })) => {
                if(argCount !=0){
                    self.runtime_error("native function takes only 0 arguments")
                }

               match native.name.as_str(){
                   "clock" =>  {
                       let result = native.clock();
                       self.stack.push(result);
                       true
                   },
                   n => {
                       self.runtime_error(&format!("Unknown native function {}", n));
                       false
                   }
               }
            }


            _ => {
                self.runtime_error("Can only call functions and classes ");
                false
            }
        }
    }

    fn call(&mut self, function: ObjFunction, argCount: u8) -> bool {
        if (argCount != function.arity) {
            self.runtime_error(&format!("Expected {} arguments, but got {}", function.arity, argCount));
            return false;
        }

        if (self.frameCount == FRAMES_MAX as usize) {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let mut frame = &mut self.frames[self.frameCount];
        self.currentChunk = &mut self.functionChunks[function.chunkIndex as usize];
        frame.function = function;
        frame.ip = 0;
        frame.slot = self.stack.len() - 1 - (argCount as usize); // the slot for this frame is stack top - arg count - 1 (saved slot for function)
        self.currentFrame = frame as *mut CallFrame;
        self.frameCount += 1;
        true
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
            (Value::BigNumber(a), Value::BigNumber(b)) => {
                let result: Result<usize, bool> = match binaryOp { // result not the best thing, but helps keep code DRY
                    BinaryOp::ADD => Ok(a + b),
                    BinaryOp::SUBTRACT => Ok(a - b),
                    BinaryOp::DIVIDE => Ok(a / b),
                    BinaryOp::MULTIPLY => Ok(a * b),
                    BinaryOp::GREATER => Err(a > b),
                    BinaryOp::LESS => Err(a < b),
                };
                match result {
                    Ok(r) => self.stack.push(Value::big_number(r)),
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
                }
            }
            (Value::OBJ(Obj::STRING(first @ ObjString { length, ptr, .. })), Value::BigNumber(a)) => {
                unsafe {
                    let str1 = std::slice::from_raw_parts(ptr, length);
                    let b = format!("{}", a);
                    let result = ObjString::concat_buffers(str1, b.as_bytes());
                    let cloned_result = result.clone();
                    self.strings.set(result, Value::nil_value());
                    self.stack.push(Value::obj_value(Obj::STRING(cloned_result)));
                }
            }

            (Value::Number(a), Value::OBJ(Obj::STRING(ObjString { .. }))) => {
                self.runtime_error("Cannot concatenate a number and string");

            }
            _ => {
                self.runtime_error("Operands must be two numbers or two strings");
            }
        }
    }

    fn runtime_error(&mut self, msg: &str) {
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
        let mut i = self.frameCount - 1;

        while (i > 0) {
            let currentFrame = &self.frames[i];
            let currentfunctionChunk = &self.functionChunks[currentFrame.function.chunkIndex as usize];

            // println!("{:?}",currentfunction);
            let line_number = (*currentfunctionChunk).lines[currentFrame.ip];

            let function_name = if (currentFrame.function.name.is_empty()) {
                "<script>"
            } else {
                currentFrame.function.name.as_str()
            };

            eprintln!("[line {}] in {}", line_number, function_name);
            i -= 1;
        }

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