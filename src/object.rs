use std::alloc;
use std::alloc::Layout;
use std::fmt::{Debug, Formatter};

#[derive(Copy,Clone)]
pub enum Obj {
    ObjString {
        length: usize,
        ptr: *mut u8
    }
}

impl Debug for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::ObjString { length, ptr } => {
                f.write_str(unsafe{ std::str::from_utf8(std::slice::from_raw_parts(*ptr, *length)).expect("cannot parse string") })
            }
        }
    }
}

impl Obj {
    pub fn free(self) {
        match self {
            Obj::ObjString { length, ptr } => {
                unsafe {
                    let layout = Layout::array::<u8>(length).expect("cannot get layout for obj");
                    alloc::dealloc(ptr, layout)
                }
            }
        }
    }
}

// impl Obj {
//
//     fn new(& self) -> Self {
//         match self {
//             Obj::ObjString { length,ptr } => {
//                 unsafe {
//                     let layout = Layout::array::<u8>(len).expect("cannot create layour for string");
//                     let ptr = alloc::alloc(layout);
//
//                     Obj::ObjString {
//                         length,
//                         ptr
//                     }
//             }
//         }
//     }
//
// }