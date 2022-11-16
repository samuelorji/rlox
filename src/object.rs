use std::alloc;
use std::ptr;
use std::alloc::Layout;
use std::any::Any;
use std::fmt::{Debug, Formatter};
use std::time::{SystemTime, UNIX_EPOCH};
use crate::{Chunk, FunctionType, Table, Value};

#[derive(Copy, Clone,PartialEq)]
pub enum Obj {
    STRING(ObjString),
    FUNCTION(ObjFunction),
    NATIVE_FUNCTION(NativeFunction),
    CLOSURE(ObjClosure),
    CLASS(ObjClass),
    INSTANCE(ObjInstance),
    BOUND_METHOD(ObjBoundMethod)

}

#[derive(Copy, Clone)]
pub struct ObjBoundMethod {
    receiver : *mut Value,
    method : ObjClosure
}
impl Debug for ObjBoundMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.method.function.name.as_str()))
    }
}
impl PartialEq for ObjBoundMethod {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self,other)
    }
}

impl ObjBoundMethod {
    pub fn getMethodName(&self) -> &str {
        self.method.function.name.as_str()
    }
    pub fn getClosure(&self) -> ObjClosure {
        self.method
    }
    pub fn setReceiver(&mut self, valuePtr : *mut Value) {
        self.receiver = valuePtr
    }

    pub fn new(receiver : *mut Value, method : ObjClosure) -> Self {
        Self {
            receiver,
            method
        }
    }
}

#[derive(Copy, Clone)]
pub struct ObjUpValue {
    location: *mut Value
}

#[derive(Copy, Clone,PartialEq)]
pub struct ObjClosure {
    pub function : ObjFunction,
    pub upValueCount : u16
}

impl ObjClosure {
    pub fn new(function : ObjFunction) -> Self {
        Self {
            function,
            upValueCount: function.upValueCount
        }
    }

    pub fn empty() -> Self {
        Self {
            function: ObjFunction::new(),
            upValueCount: 0
        }
    }
}

#[derive(Copy, Clone,PartialEq)]
pub struct ObjInstance {
    class : ObjClass,
    tableIndex : usize
}

impl ObjInstance {
    pub fn new(class: ObjClass, tableIndex : usize) -> ObjInstance {
        ObjInstance {
            class,
            tableIndex
        }
    }

    pub fn getTableIndex(&self) -> usize {
        self.tableIndex
    }
    pub fn getClass(&self) -> ObjClass {
        self.class
    }
}

impl Debug for ObjInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{} instance", self.class.name.as_str())
    }
}

#[derive(Copy, Clone,PartialEq)]
pub struct ObjClass {
    name : ObjString,
    methodTableIndex : usize
}

impl ObjClass {
    pub fn new(name : ObjString, methodTableIndex: usize) -> Self {
        ObjClass {
            name,
            methodTableIndex
        }
    }
    pub fn getMethodTableIndex(&self) -> usize {
        self.methodTableIndex
    }
    pub fn empty() -> Self {
        Self {
            name : ObjString::empty(),
            methodTableIndex: 0
        }
    }
}

impl Debug for ObjClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>",self.name.as_str())
    }
}

impl Debug for ObjClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name  = if(self.function.name.is_empty()){
            "<script>"
        } else {
            self.function.name.as_str()
        };
        write!(f, "{{ name : {} , arity : {} }}",name,self.function.arity)
    }
}
#[derive(Copy, Clone,PartialEq)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunkIndex : u16,
    pub name : ObjString,
    pub functionType: FunctionType,
    pub upValueCount: u16
}

#[derive(Copy,Clone,PartialEq)]
pub struct NativeFunction{
    pub name : ObjString
}

impl NativeFunction {
    pub fn new(name :ObjString) -> Self {
        Self {
            name
        }
    }
    pub fn clock(&self) -> Value {
        Value::big_number( SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_millis() as usize)
    }
}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name  = if(self.name.is_empty()){
            "<script>"
        } else {
            self.name.as_str()
        };
        write!(f, "{{ name : {} , arity : {} }}",name,self.arity)
    }
}


impl ObjFunction {
    pub fn new() -> Self {
        Self {
            arity:0,
            chunkIndex: 0,
            name: ObjString::empty(),
            functionType: FunctionType::SCRIPT,
            upValueCount: 0
        }
    }

    pub fn as_String(&self) -> String {
        format!("<fn {:?}>", &self.name)

    }


}

pub struct ObjString {
    length: usize,
    ptr: *mut u8,
    hash : u32,
    is_clone : bool
}

impl Copy for ObjString {

}
impl Clone for ObjString{
    fn clone(&self) -> Self {
        Self {
            length: self.length,
            hash: self.hash,
            ptr: self.ptr.clone(),
            is_clone: true
        }
    }

}

impl Debug for ObjString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
impl ObjString {
    pub fn empty() -> Self{
        ObjString {
            length: 0,
            hash:0,
            ptr: ptr::null_mut(),
            is_clone: false
        }
    }
    pub fn equalsStr(&self, other : &str) -> bool {
        if(self.length != other.len()){
            return  false
        } else {
            self.as_bytes() == other.as_bytes()
        }
    }
    pub fn is_clone(&self) -> bool {
        self.is_clone
    }
    pub fn is_empty(&self) -> bool {
        self.length == 0 && self.hash == 0 && self.ptr.is_null()
    }

    pub fn hash(&self) -> u32 {
        self.hash
    }
    pub fn length(&self) -> usize {
        self.length
    }
    pub fn as_str(&self) -> &str {
        unsafe{ std::str::from_utf8(std::slice::from_raw_parts(self.ptr, self.length)).expect("cannot parse string") }
    }

    pub fn as_str_debug(&self) -> String {
        format!("{},is clone : {}",self.as_str(),self.is_clone())
    }
    pub fn from_buffer(buffer : &[u8]) -> Self {
        let len_of_string = buffer.len();
        // allocate memory of that length
        unsafe  {
            let layout = Layout::array::<u8>(len_of_string).expect("cannot create layout for string");
            let ptr = alloc::alloc(layout);
            let mut ptr_offset : isize = 0;
            let mut hash: u32 = 2166136261;
            for byte in buffer.iter() {
                ObjString::hash_byte(&mut hash,*byte);
                ptr.offset(ptr_offset).write(*byte);
                ptr_offset+=1;
            };


           let stuff =  ObjString {
                length : len_of_string,
                ptr,
                hash,
               is_clone: false
            };
            //println!("hash is {}, str is {:?}",hash, &stuff.as_str());

            stuff
        }
    }

    pub fn from_str(strString : &str) -> Self {
        ObjString::from_buffer(strString.as_bytes())
    }

    fn hash_byte(hash : &mut u32, byte : u8) {
        *hash = *hash ^ (byte as u32);
        *hash = hash.wrapping_mul(16777619);
    }


    pub fn concat_buffers(str1 : &[u8], str2 : &[u8]) -> Self {
        unsafe {
            let length =  str1.len() + str2.len();
            let layout = Layout::array::<u8>(length).expect("cannot create layour for string");
            let ptr = alloc::alloc(layout);
            let mut ptr_offset = 0;
            let mut hash: u32 = 2166136261;


            for i in str1 {
                ObjString::hash_byte(&mut hash,*i);
                ptr.offset(ptr_offset).write(*i);
                ptr_offset+=1
            }
            for i in str2 {
                ObjString::hash_byte(&mut hash,*i);
                ptr.offset(ptr_offset).write(*i);
                ptr_offset+=1
            }

            ObjString {
                length,
                ptr,
                hash,
                is_clone: false
            }

        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr,self.length) }
    }

    pub fn free(self) {
        if(!self.is_empty() && !self.is_clone()) {
            unsafe {
                let layout = Layout::array::<u8>(self.length).expect("cannot get layout for obj");
                alloc::dealloc(self.ptr, layout)
            }
        }
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        // we can use hashes to compare strings,
        // we only do a memory compare if they're the same hash
        // to avoid representing strings that collide as the same
        if(self.hash != other.hash) {
            false
        }else {
            self.as_bytes() == other.as_bytes()
        }
    }

    fn ne(&self, other: &Self) -> bool {
        self.as_bytes() != other.as_bytes()
    }
}
impl Debug for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::STRING(string @ ObjString { .. }) => {
               // f.write_str(unsafe{ std::str::from_utf8(std::slice::from_raw_parts(*ptr, *length)).expect("cannot parse string") })
                f.write_str(string.as_str())
            }
            Obj::FUNCTION(function @ObjFunction { ..}) => {
                f.write_str(function.name.as_str())
            }
            Obj::NATIVE_FUNCTION(native @ NativeFunction{.. }) => {
                f.write_str("<native fn>")
            },
            Obj::CLOSURE(closure @ObjClosure{ .. }) => {
                f.write_str(closure.function.name.as_str())
            }
            Obj::CLASS(class @ ObjClass{ .. }) => {
                f.write_str(class.name.as_str())
            }
            Obj::INSTANCE(instance @ ObjInstance{ .. }) => {
                f.write_str(&format!("{:?}", instance))
            }

            Obj::BOUND_METHOD(method @ ObjBoundMethod{ ..}) => {
                f.write_str(&format!("{:?}", method))
            }
        }
    }
}

impl Obj {
    pub fn free(self) {
        match self {
            Obj::STRING(string @ ObjString { .. }) => {
                string.free()
            }
            Obj::FUNCTION(function @ObjFunction { ..}) => {
                function.name.free();
            }
            Obj::NATIVE_FUNCTION(native @ NativeFunction{ .. }) => {
                native.name.free()
            },
            Obj::CLOSURE(closure @ObjClosure{ .. }) => {

            }
            Obj::CLASS( class @ ObjClass{ ..}) => {
                class.name.free()
            }
            Obj::INSTANCE(instance @ ObjInstance{ .. }) => {
                instance.class.name.free()
            }

            Obj::BOUND_METHOD(method @ ObjBoundMethod{ ..}) => {
                method.method.function.name.free()

            }


        }
    }
}