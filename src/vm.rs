use crate::object::*;
use crate::vm::InterpretResult::INTERPRET_COMPILE_ERROR;
use crate::Value::OBJ;
use crate::{printValue, Chunk, Compiler, OpCode, Table, Value, ValueArray};
use std::alloc::{alloc, dealloc, Layout};
use std::hash::Hasher;
use std::ptr;
use std::thread::sleep;
use std::time::Instant;

// ip is instruction pointer,
///We use an actual real C pointer pointing right into the middle of the bytecode array instead of something
/// like an integer index because it’s faster to dereference a pointer than look up an element in an array by index

const FRAMES_MAX: u8 = 64;
const STACK_MAX: usize = (FRAMES_MAX as usize * u8::MAX as usize);

#[derive(Copy, Clone, Debug)]
struct CallFrame {
    closure: ObjClosure,
    ip: usize,
    slot: usize, // index into what slot is used
}

impl CallFrame {
    fn new() -> Self {
        CallFrame {
            closure: ObjClosure::empty(),
            ip: 0,
            slot: 0,
        }
    }
}
struct StackPtr {
    ptr: *mut Value,
}

struct UpValue {
    ptr: *mut Value,
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
    upValues: Vec<Vec<*mut Value>>,
    openUpValues: Vec<(StackPtr, UpValue)>, // stack pointer to value (heap) pointer
    instanceTables: Vec<Table>,
    methodTables: Vec<Table>,
    initString: ObjString,
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
        self.functionChunks
            .iter_mut()
            .for_each(|chunk| chunk.free());
        self.openUpValues
            .iter_mut()
            .for_each(|(_, upValue)| unsafe {
                dealloc(upValue.ptr as *mut u8, Layout::for_value(&Value::empty()))
            });
        self.instanceTables
            .iter_mut()
            .for_each(|table| table.free());
        self.methodTables.iter_mut().for_each(|table| table.free());
    }

    pub fn new() -> Self {
        let mut vm = Self {
            frames: [CallFrame::new(); FRAMES_MAX as usize],
            frameCount: 0,
            stack: Vec::new(),
            currentChunk: ptr::null_mut(),
            currentFrame: ptr::null_mut(),
            objects: Vec::new(),
            globals: Table::new(),
            strings: Table::new(),
            functionChunks: vec![],
            upValues: vec![],
            openUpValues: vec![],
            instanceTables: vec![],
            methodTables: vec![],
            initString: ObjString::empty(),
        };

        let clock = ObjString::from_str("clock");
        let clockClone = clock.clone();
        vm.strings.set(clock, Value::empty());
        vm.define_native(clockClone);
        let initString = ObjString::from_str("init");
        vm.initString = vm.getInternedString(initString);
        vm
    }

    fn define_native(&mut self, name: ObjString) {
        let native_fn = Value::nativeFn(name);
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
                self.stack.push(Value::objValue(Obj::FUNCTION(function)));
                let closure = ObjClosure::new(function);
                self.call(closure, 0);
                self.upValues.push(vec![]);
                self.run()
            }
        }
    }

    fn add_value(&mut self, value: Value) {
        self.objects.push(value);
    }
    fn run(&mut self) -> InterpretResult {
        // self.currentFrame = &mut self.frames[(self.frameCount - 1) as usize];
        loop {
            #[cfg(feature = "debug")]
            // prints code offset, line number instruction name, constant offset and constant value
            {
                let chunk = self.currentChunk;
                print!("          ");
                for slot in &self.stack {
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
                    unsafe {
                        let functionReturnResult = self.popStack();
                        self.frameCount -= 1;
                        if (self.frameCount == 0) {
                            // at this point we have the <script> on the stack, so
                            // pop that and end
                            self.popStack();
                            return InterpretResult::INTERPRET_OK;
                        }

                        self.stack.resize((*self.currentFrame).slot, Value::empty());
                        self.stack.push(functionReturnResult);
                        self.currentFrame = &mut self.frames[self.frameCount - 1];
                        let currentFunctionIndex =
                            (*self.currentFrame).closure.function.chunkIndex as usize;
                        self.currentChunk = &mut self.functionChunks[currentFunctionIndex];
                    }
                }
                OpCode::OP_NEGATE => {
                    let peeked = self.peek(0);
                    match &peeked {
                        Value::Number(r) => {
                            let number_on_stack = self.popStack().asNumber();
                            self.stack.push(Value::numberValue(-number_on_stack));
                        }
                        _ => {
                            self.runtimeError("Operand must be a number.");
                            return InterpretResult::INTERPRET_RUNTIME_ERROR;
                        }
                    }
                }
                OpCode::OP_ADD => self.binaryOp(BinaryOp::ADD),
                OpCode::OP_SUBTRACT => self.binaryOp(BinaryOp::SUBTRACT),
                OpCode::OP_DIVIDE => self.binaryOp(BinaryOp::DIVIDE),
                OpCode::OP_MULTIPLY => self.binaryOp(BinaryOp::MULTIPLY),
                OpCode::OP_NIL => self.stack.push(Value::nilValue()),
                OpCode::OP_TRUE => self.stack.push(Value::boolValue(true)),
                OpCode::OP_FALSE => self.stack.push(Value::boolValue(false)),
                OpCode::OP_NOT => {
                    let popped = self.popStack();
                    self.stack.push(Value::boolValue(self.isFalsey(&popped)));
                }

                OpCode::OP_EQUAL => {
                    let b = self.popStack();
                    let a = self.popStack();
                    let result = &a == &b;
                    self.stack.push(Value::boolValue(result));
                }
                OpCode::OP_GREATER => self.binaryOp(BinaryOp::GREATER),
                OpCode::OP_LESS => self.binaryOp(BinaryOp::LESS),

                OpCode::OP_PRINT => {
                    let to_be_printed = self.popStack();
                    printValue(&to_be_printed);
                    println!("");
                }

                OpCode::OP_POP => {
                    if (!self.stack.is_empty()) {
                        self.popStack();
                    }
                }

                OpCode::OP_DEFINE_GLOBAL => {
                    // stack will
                    let constant = self.readConstant();
                    let variable_name = constant.asObjString();
                    let variable_value = self.popStack();
                    self.globals.set(variable_name, variable_value);
                }

                OpCode::OP_GET_GLOBAL => {
                    let constant = self.readConstant();
                    let variable_name = constant.asObjString();
                    match self.globals.get(&variable_name) {
                        None => {
                            self.runtimeError(&format!(
                                "Undefined variable '{:?}'.",
                                &variable_name
                            ));
                            return InterpretResult::INTERPRET_RUNTIME_ERROR;
                        }
                        Some(value) => {
                            let cloned_val = value.clone();
                            self.stack.push(cloned_val);
                        }
                    }
                }

                OpCode::OP_SET_GLOBAL => {
                    let name = self.readConstant().asObjString();
                    let cloned_name = name.clone();
                    if (self.globals.set(name, self.peek(0).clone())) {
                        self.globals.remove(&cloned_name);
                        self.runtimeError(&format!("Undefined variable '{:?}'.", &cloned_name));
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
                    let jump = self.read16BitShort();
                    // println!("jump is {}", &jump);
                    unsafe {
                        // check if what's on stack is true or not
                        if self.isFalsey(self.peek(0)) {
                            // if false ... add jump to ip
                            (*self.currentFrame).ip += jump as usize
                        }
                    }
                }

                OpCode::OP_JUMP => {
                    let jump = self.read16BitShort();
                    //println!("jump is {}",&jump);
                    unsafe { (*self.currentFrame).ip += jump as usize }
                }
                OpCode::OP_LOOP => {
                    let jump = self.read16BitShort();
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

                OpCode::OP_CLOSURE => {
                    unsafe {
                        let function = self.readConstant().asFunction();
                        let closure = ObjClosure::new(function);
                        self.stack.push(Value::makeClosure(closure));
                        if (closure.function.chunkIndex != 0) {
                            // push upvalues for closure if not exists
                            match self.upValues.get(closure.function.chunkIndex as usize) {
                                Some(_) => {}
                                None => self.upValues.push(vec![]),
                            }

                            for i in 0..closure.upValueCount {
                                // println!("closure up value count for function {} is {}, i is {i}",closure.function.name.as_str() ,closure.upValueCount);
                                let isLocal = self.readByte();
                                let index = self.readByte();

                                let mut valuePtr: *mut Value = ptr::null_mut();

                                let isLocal = isLocal == 1;
                                if (isLocal) {
                                    let localValueIndex =
                                        (*self.currentFrame).slot + index as usize;
                                    let mut value = {
                                        let value = self.getAtMut(localValueIndex);
                                        value as *mut Value
                                    };
                                    self.captureUpValue(
                                        closure.function.chunkIndex as usize,
                                        valuePtr,
                                        value,
                                        i,
                                    )
                                } else {
                                    let closureIndex =
                                        (*self.currentFrame).closure.function.chunkIndex as usize;
                                    let upValuesPtr: &mut Vec<*mut Value> =
                                        &mut self.upValues[closureIndex];
                                    let read = (*upValuesPtr)[index as usize].clone();
                                    valuePtr = read;
                                    &mut self.upValues[closure.function.chunkIndex as usize]
                                        .push(valuePtr);
                                }
                            }
                        }
                    }
                }

                OpCode::OP_GET_UPVALUE => {
                    let slot = self.readByte();
                    let value: Value = unsafe {
                        let index = (*self.currentFrame).closure.function.chunkIndex as usize;
                        let mut upValuesPtr: &mut Vec<*mut Value> = &mut self.upValues[index];
                        let value = (*upValuesPtr)[slot as usize].read();
                        value
                    };

                    self.stack.push(value)
                }
                OpCode::OP_SET_UPVALUE => {
                    let slot = self.readByte();
                    let peeked = self.peek(0).clone();
                    unsafe {
                        let closureIndex =
                            (*self.currentFrame).closure.function.chunkIndex as usize;
                        let mut upValuesPtr: &mut Vec<*mut Value> =
                            &mut self.upValues[closureIndex];
                        let ptr = (*upValuesPtr)[slot as usize];
                        let read = ptr.read();
                        //println!("Read is {:?} at {:p}, peeked is {:?}",&read,&read, &peeked);
                        ptr.write(peeked)
                    }
                }

                OpCode::OP_CLOSE_UPVALUE => {}
                OpCode::OP_CLASS => {
                    let className = self.readConstant().asObjString();
                    let methodTableIndex = self.methodTables.len();
                    self.methodTables.push(Table::new());
                    self.stack
                        .push(Value::makeClass(className, methodTableIndex))
                }

                OpCode::OP_SET_PROPERTY => {
                    // When this executes, the top of the stack has the instance
                    // whose field is being set and above that, the value to be stored.
                    let shouldBeInstance = self.peek(1);
                    if (!shouldBeInstance.isInstance()) {
                        self.runtimeError("Only instances have fields.");
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    } else {
                        let instance = shouldBeInstance.clone().asInstance();
                        let name = self.readConstant().asObjString();
                        let value = self.peek(0).clone();
                        let instanceFieldTable = &mut self.instanceTables[instance.getTableIndex()];
                        instanceFieldTable.set(name, value);
                        let value = self.popStack();
                        self.popStack();
                        self.stack.push(value)
                    }
                }
                OpCode::OP_GET_PROPERTY => {
                    let peekedValue = self.peek(0);
                    if (!peekedValue.isInstance()) {
                        self.runtimeError("Only instances have properties.");
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    } else {
                        let instance = peekedValue.clone().asInstance();

                        let name = self.readConstant().asObjString();
                        let instanceFieldTable = &mut self.instanceTables[instance.getTableIndex()];
                        match instanceFieldTable.get(&name) {
                            None => {
                                // assume its a method

                                if (!self.bindMethod(instance.getClass(), name)) {
                                    return InterpretResult::INTERPRET_RUNTIME_ERROR;
                                }
                            }
                            Some(value) => {
                                self.stack.pop(); // remove instance
                                self.stack.push(value.clone())
                            }
                        }
                    }
                }

                OpCode::OP_METHOD => {
                    let methodName = self.readConstant().asObjString();
                    self.defineMethod(methodName);
                }

                OpCode::OP_INVOKE => unsafe {
                    let method = self.readConstant().asObjString();
                    let argCount = self.readByte();

                    if (!self.invoke(method, argCount)) {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    }

                    let currentFrameIndex = self.frameCount - 1;
                    self.currentFrame = &mut self.frames[currentFrameIndex] as *mut CallFrame;
                },

                OpCode::OP_INHERIT => {
                    let superClass = self.peek(1).clone();
                    let subClass = self.peek(0).clone().asClass();

                    if (!superClass.isClass()) {
                        self.runtimeError("Superclass must be a class.");
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    }

                    let superClass = superClass.asClass();
                    // having to use a pointer cos you can't mutably borrow twice
                    let mut superClassMethods =
                        &mut self.methodTables[superClass.getMethodTableIndex()] as *mut Table;
                    let mut subClassMethods =
                        &mut self.methodTables[subClass.getMethodTableIndex()];

                    unsafe {
                        Table::add_all(&mut (*superClassMethods), subClassMethods);
                    }
                    self.popStack();
                }

                OpCode::OP_GET_SUPER => {
                    let name = self.readConstant().asObjString();
                    let superClass = self.popStack().asClass();

                    if (!self.bindMethod(superClass, name)) {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    }
                }

                OpCode::OP_SUPER_INVOKE => {
                    let method = self.readConstant().asObjString();
                    let argCount = self.readByte();

                    let superClass = self.popStack().asClass();
                    if (!self.invokeFromClass(superClass, method, argCount)) {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR;
                    }
                    unsafe {
                        let currentFrameIndex = self.frameCount - 1;
                        self.currentFrame = &mut self.frames[currentFrameIndex] as *mut CallFrame;
                    }
                }
            }
        }
    }

    fn invoke(&mut self, name: ObjString, argCount: u8) -> bool {
        let receiver = self.peek(argCount as usize);
        if (!receiver.isInstance()) {
            self.runtimeError("Only instances have methods.");
            return false;
        }

        let instance = receiver.asInstance();

        let result: Option<Value> = self.instanceTables[instance.getTableIndex()]
            .get(&name)
            .map(|valueRef| valueRef.clone());

        match result {
            None => {}
            Some(value) => {
                let index = self.stack.len() - 1 - (argCount as usize);
                self.stack[index] = value;
                return self.callValue(value, argCount);
            }
        }

        self.invokeFromClass(instance.getClass(), name, argCount)
    }

    fn invokeFromClass(&mut self, klass: ObjClass, name: ObjString, argCount: u8) -> bool {
        match self.methodTables[klass.getMethodTableIndex()].get(&name) {
            None => {
                // println!("invoke from class");
                self.runtimeError(&format!("Undefined property '{}'.", name.as_str()));
                return false;
            }
            Some(value) => {
                let method = value.clone().asClosure();
                self.call(method, argCount)
            }
        }
    }

    fn bindMethod(&mut self, klass: ObjClass, name: ObjString) -> bool {
        let peeked = self.peek(0).clone();
        match self.methodTables[klass.getMethodTableIndex()].get(&name) {
            None => {
                self.runtimeError(&format!("Undefined property {}.", name.as_str()));
                return false;
            }
            Some(value) => {
                let method = value.clone();
                let bound = ObjBoundMethod::new(
                    &mut (self.peek(0).clone()) as *mut Value,
                    method.asClosure(),
                );
                self.popStack();
                self.stack.push(Value::makeBoundMethod(bound))
            }
        }

        return true;
    }

    fn defineMethod(&mut self, methodName: ObjString) {
        let method = self.peek(0).clone();
        let klass = self.peek(1).clone().asClass();

        &mut self.methodTables[klass.getMethodTableIndex()].set(methodName, method);
        self.popStack();
    }

    fn captureUpValue(
        &mut self,
        closureIndex: usize,
        mut valuePtr: *mut Value,
        stackPtr: *mut Value,
        ind: u16,
    ) {
        unsafe {
            // check if we have the value already;

            //let mut i : usize = 0;
            let mut i = self.openUpValues.len();
            let mut maybeOpenUpValue: *mut Value = ptr::null_mut();

            if (i != 0) {
                while (i >= 1) {
                    let (savedStackPtr, upValue) = &mut self.openUpValues[i - 1];
                    if (ptr::eq(savedStackPtr.ptr, stackPtr)) {
                        maybeOpenUpValue = (*upValue).ptr.clone();
                        break;
                    }
                    i -= 1;
                }
            }

            // only create new upvalue it hasn't been captured by another closure
            if (maybeOpenUpValue.is_null()) {
                let layout = Layout::for_value(&Value::empty());
                valuePtr = alloc(layout) as *mut Value;
                self.openUpValues.push((
                    StackPtr {
                        ptr: stackPtr.clone(),
                    },
                    UpValue {
                        ptr: valuePtr.clone(),
                    },
                ));
                valuePtr.write(*stackPtr);
            } else {
                valuePtr = maybeOpenUpValue
            }

            let len = self.upValues.len();
            if (closureIndex >= len) {
                // println!("inside if ");
                let diff = closureIndex - len;
                for i in 0..(diff + 1) {
                    match self.upValues.get(len) {
                        Some(_) => {}
                        None => {
                            self.upValues.push(vec![]);
                        }
                    }
                }
            }
            // add ptr to closures value ptrs;
            //println!("pushing {:p} {}",&valuePtr, i);

            &mut self.upValues[closureIndex].push(valuePtr);
        };
    }

    fn callValue(&mut self, callee: Value, argCount: u8) -> bool {
        // println!("calling {:?}",&callee);
        match callee {
            Value::OBJ(Obj::BOUND_METHOD(mut method @ ObjBoundMethod { .. })) => {
                let receiverIndex = self.stack.len() - 1 - (argCount as usize);
                let mut receiver = self.getAt(receiverIndex).clone();

                method.setReceiver(&mut receiver as *mut Value);
                self.call(method.getClosure(), argCount)
            }
            Value::OBJ(Obj::CLOSURE(closure @ ObjClosure { .. })) => self.call(closure, argCount),
            Value::OBJ(Obj::CLASS(class @ ObjClass { .. })) => {
                let taleIndex = self.instanceTables.len();
                self.instanceTables.push(Table::new());
                let instance = Value::makeInstance(ObjInstance::new(class, taleIndex));
                let instanceIndex = self.stack.len() - 1 - argCount as usize;
                self.stack[instanceIndex] = instance;

                // After the runtime allocates the new instance,
                // we look for an init() method on the class. If we find one, we initiate a call to it.

                // call init if exists on instance

                let initString = &self.initString;
                match self.methodTables[class.getMethodTableIndex()].get(initString) {
                    None => {
                        // if there is no init() method,
                        // then it doesn’t make any sense to pass arguments to the class when creating the instance
                        if (argCount != 0) {
                            self.runtimeError(&format!(
                                "Expected 0 arguments but got {}.",
                                argCount
                            ));
                        }
                    }
                    Some(initializer) => {
                        let closure = initializer.clone().asClosure();
                        self.call(closure, argCount);
                    }
                }

                return true;
            }

            Value::OBJ(Obj::NATIVE_FUNCTION(native @ NativeFunction { .. })) => {
                if (argCount != 0) {
                    self.runtimeError("native function takes only 0 arguments")
                }

                match native.name.as_str() {
                    "clock" => {
                        let result = native.clock();
                        self.stack.push(result);
                        true
                    }
                    n => {
                        self.runtimeError(&format!("Unknown native function {}", n));
                        false
                    }
                }
            }

            _ => {
                self.runtimeError("Can only call functions and classes ");
                false
            }
        }
    }

    fn call(&mut self, closure: ObjClosure, argCount: u8) -> bool {
        if (argCount != closure.function.arity) {
            self.runtimeError(&format!(
                "Expected {} arguments, but got {}",
                closure.function.arity, argCount
            ));
            return false;
        }

        if (self.frameCount == FRAMES_MAX as usize) {
            self.runtimeError("Stack overflow.");
            return false;
        }

        let mut frame = &mut self.frames[self.frameCount];
        self.currentChunk = &mut self.functionChunks[closure.function.chunkIndex as usize];
        frame.closure = closure;
        frame.ip = 0;
        frame.slot = self.stack.len() - 1 - (argCount as usize); // the slot for this frame is stack top - arg count - 1 (saved slot for function)
        self.currentFrame = frame as *mut CallFrame;
        self.frameCount += 1;
        true
    }

    fn popStack(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
    fn isFalsey(&self, value: &Value) -> bool {
        match value {
            Value::Number(0f64) => true,
            Value::Bool(boolean) => !boolean,
            _ => true,
        }
    }

    fn read16BitShort(&mut self) -> u16 {
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

    fn getAt(&self, index: usize) -> &Value {
        &self.stack[index]
    }
    fn getAtMut(&mut self, index: usize) -> &mut Value {
        &mut self.stack[index]
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
                let result: Result<f64, bool> = match binaryOp {
                    // result not the best thing, but helps keep code DRY
                    BinaryOp::ADD => Ok(a + b),
                    BinaryOp::SUBTRACT => Ok(a - b),
                    BinaryOp::DIVIDE => Ok(a / b),
                    BinaryOp::MULTIPLY => Ok(a * b),
                    BinaryOp::GREATER => Err(a > b),
                    BinaryOp::LESS => Err(a < b),
                };
                match result {
                    Ok(r) => self.stack.push(Value::numberValue(r)),
                    Err(b) => self.stack.push(Value::boolValue(b)),
                }
            }
            (Value::BigNumber(a), Value::BigNumber(b)) => {
                let result: Result<usize, bool> = match binaryOp {
                    // result not the best thing, but helps keep code DRY
                    BinaryOp::ADD => Ok(a + b),
                    BinaryOp::SUBTRACT => Ok(a - b),
                    BinaryOp::DIVIDE => Ok(a / b),
                    BinaryOp::MULTIPLY => Ok(a * b),
                    BinaryOp::GREATER => Err(a > b),
                    BinaryOp::LESS => Err(a < b),
                };
                match result {
                    Ok(r) => self.stack.push(Value::bigNumber(r)),
                    Err(b) => self.stack.push(Value::boolValue(b)),
                }
            }
            (
                Value::OBJ(Obj::STRING(first @ ObjString { .. })),
                Value::OBJ(Obj::STRING(second @ ObjString { .. })),
            ) => unsafe {
                let result = ObjString::concat_buffers(first.as_bytes(), second.as_bytes());
                let cloned_result = result.clone();
                self.strings.set(result, Value::nilValue());
                self.stack.push(Value::objValue(Obj::STRING(cloned_result)));
            },
            (Value::OBJ(Obj::STRING(first @ ObjString { .. })), Value::Number(a)) => unsafe {
                let str1 = first.as_bytes();
                let b = format!("{}", a);
                let result = ObjString::concat_buffers(str1, b.as_bytes());
                let cloned_result = result.clone();
                self.strings.set(result, Value::nilValue());
                self.stack.push(Value::objValue(Obj::STRING(cloned_result)));
            },
            (Value::OBJ(Obj::STRING(first @ ObjString { .. })), Value::BigNumber(a)) => unsafe {
                let b = format!("{}", a);
                let result = ObjString::concat_buffers(first.as_bytes(), b.as_bytes());
                let cloned_result = result.clone();
                self.strings.set(result, Value::nilValue());
                self.stack.push(Value::objValue(Obj::STRING(cloned_result)));
            },

            (Value::Number(a), Value::OBJ(Obj::STRING(ObjString { .. }))) => {
                self.runtimeError("Cannot concatenate a number and string");
            }
            _ => {
                self.runtimeError("Operands must be two numbers or two strings");
            }
        }
    }
    pub fn getInternedString(&mut self, string: ObjString) -> ObjString {
        match self.strings.get_key(&string) {
            None => {
                let cloned_string = string.clone();
                self.strings.set(string, Value::nilValue());
                cloned_string
            }
            Some(internedString) => {
                // free allocated string, return clone instead
                string.free();
                internedString.clone()
            }
        }
    }

    fn runtimeError(&mut self, msg: &str) {
        eprintln!("{}", msg);
        let mut i = self.frameCount - 1;

        while (i >= 0) {
            let currentFrame = &self.frames[i];
            let currentfunctionChunk =
                &self.functionChunks[currentFrame.closure.function.chunkIndex as usize];

            // println!("{:?}",currentfunction);
            let line_number = (*currentfunctionChunk).lines[currentFrame.ip];

            let function_name = if (currentFrame.closure.function.name.is_empty()) {
                "script"
            } else {
                currentFrame.closure.function.name.as_str()
            };

            eprintln!("[line {}] in {}", line_number, function_name);
            if (i == 0) {
                break;
            } else {
                i -= 1;
            }
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
