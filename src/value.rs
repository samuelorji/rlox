//pub type Value = f32;
use std::any::Any;
use std::fmt::{Debug, Formatter, Write};
use crate::object::*;

#[derive(Debug)]
pub struct ValueArray {
     pub values : Vec<Value>
}

pub fn printValue(value : &Value) {
    match value {
        Value::Number(0f64) => {
            print!("nil")
        },
        _ => print!("{:?}",&value)

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
#[derive(Copy, Clone,PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    BigNumber(usize),
    OBJ(Obj),
    Empty
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(r) => {
                let rep = format!("{}",r);
                f.write_str(&rep)
            },
            Value::Number(r) => {
                let rep = format!("{}",r);
                f.write_str(&rep)
            }

            Value::OBJ(obj) => {
                match obj {
                    Obj::STRING(string @ObjString {.. }) => {
                        f.write_str(&string.as_str())
                    }
                    Obj::FUNCTION(function @ ObjFunction {..}) => {
                        let funcName = if function.name.is_empty() {
                            "<script>".to_string()
                        } else {
                            function.as_String()
                        };
                        f.write_str(&funcName)
                    }
                    Obj::NATIVE_FUNCTION(native @NativeFunction{..}) => {
                        f.write_str("<native fn>")
                    }
                    Obj::CLOSURE(closure @ObjClosure{ .. }) => {
                        let funcName = if closure.function.name.is_empty() {
                            "<script>".to_string()
                        } else {
                            closure.function.as_String()
                        };
                        f.write_str(&funcName)
                    }
                    Obj::CLASS(class @ ObjClass{ .. }) => {
                        f.write_str(&format!("{:?}",&class))
                    }
                    Obj::INSTANCE(instance @ ObjInstance{ .. }) => {
                        f.write_str(&format!("{:?}", instance))
                    }
                }
            }

            Value::Empty => f.write_str("null"),
            Value::BigNumber(num) => {
                f.write_str(&num.to_string())
            }
        }
    }
}

impl Value {

    pub fn bool_value(value : bool) -> Self {
        Value::Bool(value)
    }
    pub fn native_fn(name :ObjString) -> Self {
        Value::OBJ(Obj::NATIVE_FUNCTION(NativeFunction::new(name)))
    }

    pub fn soft_clone(&self) -> Self {
        match self {
            Value::OBJ(Obj::STRING(p @ObjString{..})) => Value::OBJ(Obj::STRING(p.clone())),
           x => x.clone()
        }
    }

    pub fn nil_value() -> Self {
        Value::Number(0_f64)
    }
    pub fn number_value(number : f64) -> Self {
        Value::Number(number)
    }

    pub fn big_number(value : usize) -> Self {
        Value::BigNumber(value)
    }

    pub fn obj_value(obj : Obj) -> Self {
        Value::OBJ(obj)
    }

    pub fn as_bool(self) -> bool {
        match self {
            p @ Value::Bool(b) => b,
            x => panic!("{:?} is not a bool",x)
        }
    }

    pub fn as_obj_string(self) -> ObjString {
        match self {
            Value::OBJ(Obj::STRING(obj @ ObjString { .. })) => {
                obj
            },
            x => panic!("{:?} is not an obj string",x)
        }
    }

    pub fn as_function(self) -> ObjFunction {
        match self {
            Value::OBJ(Obj::FUNCTION(func @ ObjFunction { .. })) => {
                func
            },
            x => panic!("{:?} is not an function string",x)
        }
    }

    pub fn as_closure(self) -> ObjClosure {
        match self {
            Value::OBJ(Obj::CLOSURE(closure @ ObjClosure { .. })) => {
                closure
            },
            x => panic!("{:?} is not an function string",x)
        }
    }

    pub fn as_class(self) -> ObjClass {
        match self {
            Value::OBJ(Obj::CLASS(class @ ObjClass{ ..})) => {
                class
            }
            x => panic!("{:?} is not an class",x)
        }
    }

    pub fn as_instance(self) -> ObjInstance {
        match self {
            Value::OBJ(Obj::INSTANCE(instance @ ObjInstance{ ..})) => {
                instance
            }
            x => panic!("{:?} is not an instance",x)
        }
    }

    pub fn is_instance(&self) -> bool {
        match self {
            Value::OBJ(Obj::INSTANCE(ObjInstance{ ..})) => {
                true
            }
            x => false
        }

    }



    pub fn as_number(self) -> f64 {
        match self {
            Value::Number(r) => r,
            x => panic!("{:?} is not a number",x)
        }
    }
    pub fn empty() -> Self {
        Value::Empty
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Value::Empty => true,
             _ => false
        }
    }

    pub fn make_class(className : ObjString) -> Value {
        Value::OBJ(Obj::CLASS(ObjClass { name : className}))

    }

    pub fn make_closure(closure : ObjClosure) -> Value {
        Value::OBJ(Obj::CLOSURE(closure))
    }
    pub fn make_instance(instance : ObjInstance) -> Value {
        Value::OBJ(Obj::INSTANCE(instance))
    }


    pub fn is_nil(&self) -> bool {
        match self {
            Value::Number(num) =>  *num == 0_f64,
            _ => false
        }
    }


    pub fn free(self) {
        match  self {
            Value::OBJ(obj) =>  obj.free(),
            _ => ()
        }
    }
}