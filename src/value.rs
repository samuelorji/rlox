//pub type Value = f32;
use std::any::Any;
use std::fmt::{Debug, Formatter};

pub struct ValueArray {
     pub values : Vec<Value>
}

pub fn printValue(value : &Value) {
    print!("{:?}",&value.rep)
}

impl ValueArray {
    pub fn new() -> Self {
        Self{
            values : vec![]
        }
    }

    pub fn write(&mut self,value : Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.values.pop()
    }

    pub fn free(&mut self) {
        self.values = Vec::new()
    }
    pub fn count(&self) -> u32 {
        self.values.len() as u32
    }
    pub fn read_value(&self, index : usize) -> Value {
     self.values[index]
    }

}
struct Bool {
    value : bool
}
struct Number {
    value : f64
}

#[derive(Debug,Copy, Clone)]
pub enum ValueType {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
}
#[derive(Copy, Clone)]
pub enum As {
    Bool(bool),
    Number(f64)
}

impl Debug for As {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            As::Bool(r) => {
                let rep = format!("{}",r);
                f.write_str(&rep)
            },
            As::Number(r) => {
                let rep = format!("{}",r);
                f.write_str(&rep)
            }
        }
    }
}

#[derive(Debug,Copy, Clone)]
pub struct Value {
    valueType : ValueType,
    pub rep : As
}

impl Value {

    pub fn bool_value(value : bool) -> Self {
        Self {
            valueType: ValueType::VAL_BOOL,
            rep : As::Bool(value)
        }
    }

    pub fn nil_value() -> Self {
        Self {
            valueType: ValueType::VAL_NIL,
            rep: As::Number(0_f64)
        }
    }
    pub fn number_value(number : f64) -> Self {
        Self {
            valueType: ValueType::VAL_NUMBER,
            rep: As::Number(number)
        }
    }

    pub fn as_bool(self) -> bool {
        match self.rep {
            p @ As::Bool(b) => b,
            x => panic!("{:?} is not a bool",x)
        }
    }

    pub fn as_number(self) -> f64 {
        match self.rep {
            p @ As::Number(r) => r,
            x => panic!("{:?} is not a number",x)
        }
    }

}