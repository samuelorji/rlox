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
    pub ptr: *mut u8
}

impl ObjString {
    pub fn empty() -> Self{
        ObjString {
            length: 0,
            ptr: ptr::null_mut(),
        }
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
            for byte in buffer.iter() {
                ptr.offset(ptr_offset).write(*byte);
                ptr_offset+=1;
            };
            ObjString {
                length : len_of_string,
                ptr
            }
        }
    }

    pub fn concat_buffers(str1 : &[u8], str2 : &[u8]) -> Self {
        unsafe {
            let length =  str1.len() + str2.len();
            let layout = Layout::array::<u8>(length).expect("cannot create layour for string");
            let ptr = alloc::alloc(layout);
            let mut ptr_offset = 0;

            for i in str1 {
                ptr.offset(ptr_offset).write(*i);
                ptr_offset+=1
            }
            for i in str2 {
                ptr.offset(ptr_offset).write(*i);
                ptr_offset+=1
            }

            ObjString {
                length,
                ptr
            }

        }
    }

    pub fn free(self) {
        unsafe {
            let layout = Layout::array::<u8>(self.length).expect("cannot get layout for obj");
            alloc::dealloc(self.ptr, layout)
        }
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