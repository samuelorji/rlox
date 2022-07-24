pub type Value = f32;

pub struct ValueArray {
     pub values : Vec<Value>
}

pub fn printValue(value : Value) {
    print!("{}",value)
}

impl ValueArray {
    pub fn new() -> Self {
        Self{
            values : vec![]
        }
    }

    pub fn write(&mut self,value : Value) ->(){
        self.values.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.values.pop()
    }

    pub fn free(&mut self) {
        self.values.resize(0,0_f32)
    }
    pub fn count(&self) -> u32 {
        self.values.len() as u32
    }

}

pub struct StackArray{
    pub values : Vec<Value>
}

impl StackArray {
    pub fn new() -> Self {
        Self{
            values : vec![]
        }
    }

    pub fn push(&mut self, value : Value) {
        self.values.push(value);
    }

    pub fn pop(&mut self) -> Value {
        self.values.pop().unwrap_or_else(|| panic!("popping from an empty stack"))
    }
    pub fn free(&mut self) {
        self.values.resize(0,0_f32)
    }

}