#[derive(Copy,Clone)]
pub enum Obj {
    ObjString {
        length: usize,
        ptr: *mut u8
    }
}