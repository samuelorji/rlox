//pub type Value = f32;
use std::any::Any;
use std::fmt::{Debug, Formatter};
use crate::object::*;

pub struct ValueArray {
     pub values : Vec<Value>
}

pub fn printValue(value : &Value) {
    match value.valueType {
        ValueType::NIL => print!("nil"),
        _ => print!("{:?}",&value.rep)

    }

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

#[derive(Debug,Copy, Clone,PartialEq)]
pub enum ValueType {
    BOOL,
    NIL,
    NUMBER,
    OBJ,
    Empty
}
#[derive(Copy, Clone)]
pub enum As {
    Bool(bool),
    Number(f64),
    OBJ(Obj),
    Empty
}

impl As {
    pub fn free(self) {
        match self  {
            As::OBJ(obj) => {
                obj.free()
            },
            _ => ()
        }
    }
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
            As::OBJ(obj) => {
                match obj {
                    Obj::STRING(string @ObjString {.. }) => {
                        f.write_str(string.as_str())
                    }
                }
            }

            As::Empty => f.write_str("null")
        }
    }
}

#[derive(Debug,Copy, Clone)]
pub struct Value {
    pub valueType : ValueType,
    pub rep : As
}

impl PartialEq for Value{
    fn eq(&self, other: &Self) -> bool {
        let a = self;
        let b = other;
        if(a.valueType != b.valueType) {
            return false
        } else {
            match a.valueType {
                ValueType::BOOL => a.as_bool() == b.as_bool(),
                ValueType::NIL => true, // a== b , nil == nil
                ValueType::NUMBER => a.as_number() == b.as_number(),
                ValueType::OBJ => {
                    match (a.rep, b.rep) {
                        (As::OBJ(objA), (As::OBJ(objB))) => {
                            match (objA,objB) {
                                (Obj::STRING(first @ObjString { .. }),  Obj::STRING(second @ObjString {..})) => {
                                    first == second
                                },
                                _ => todo!()
                            }
                        },
                        _ => panic!("other representations should have been handled")
                    }
                }

                ValueType::Empty =>  true
            }
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !(Value::eq(self, other))
    }


}
impl Value {

    pub fn bool_value(value : bool) -> Self {
        Self {
            valueType: ValueType::BOOL,
            rep : As::Bool(value)
        }
    }

    pub fn nil_value() -> Self {
        Self {
            valueType: ValueType::NIL,
            rep: As::Number(0_f64)
        }
    }
    pub fn number_value(number : f64) -> Self {
        Self {
            valueType: ValueType::NUMBER,
            rep: As::Number(number)
        }
    }

    pub fn obj_value(obj : Obj) -> Self {
        Self {
            valueType: ValueType::OBJ,
            rep: As::OBJ(obj)
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
    pub fn empty() -> Self {
        Value {
            valueType : ValueType::Empty,
            rep: As::Empty
        }
    }

    pub fn is_empty(&self) -> bool {
        match (*self).valueType {
            ValueType::Empty => true,
             _ => false
        }
    }


    pub fn is_nil(&self) -> bool {
        match self.valueType {
            ValueType::NIL =>  {
              match self.rep   {
                  As::Number(r) => r == 0f64,
                 _ => false
              }
            },
            _ => false
        }
    }


    pub fn free(self) {
        match  self.valueType {
            ValueType::OBJ =>  self.rep.free(),
            _ => ()
        }
    }
}