use std::alloc;
use std::ptr;
use std::alloc::Layout;
use std::fmt::{Debug, Formatter};

#[derive(Copy,Clone)]
pub enum Obj {
    STRING(ObjString),
}

#[derive(Copy,Clone)]
pub struct ObjString {
    pub length: usize,
    pub ptr: *mut u8,
    pub hash : u32
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
        }
    }
    pub fn is_empty(&self) -> bool {
        self.length == 0 && self.hash == 0 && self.ptr.is_null()
    }

    pub fn as_str(&self) -> &str {
        unsafe{ std::str::from_utf8(std::slice::from_raw_parts(self.ptr, self.length)).expect("cannot parse string") }
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
                hash
            };
            //println!("hash is {}, str is {:?}",hash, &stuff.as_str());

            stuff
        }
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
                hash
            }

        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr,self.length) }
    }

    pub fn free(self) {
        if(!self.is_empty()) {
            unsafe {
                let layout = Layout::array::<u8>(self.length).expect("cannot get layout for obj");
                alloc::dealloc(self.ptr, layout)
            }
        }
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
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
        }
    }
}

impl Obj {
    pub fn free(self) {
        match self {
            Obj::STRING(string @ ObjString { .. }) => {
                string.free()
            }
        }
    }
}