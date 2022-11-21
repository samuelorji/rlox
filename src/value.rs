//pub type Value = f32;
use crate::object::*;
use std::any::Any;
use std::fmt::{Debug, Formatter, Write};

#[derive(Debug)]
pub struct ValueArray {
    pub values: Vec<Value>,
}

pub fn printValue(value: &Value) {
    match value {
        Value::Number(0f64) => {
            print!("nil")
        }
        _ => print!("{:?}", &value),
    }
}

impl ValueArray {
    pub fn new() -> Self {
        Self { values: vec![] }
    }

    pub fn write(&mut self, value: Value) -> usize {
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
    pub fn readValue(&self, index: usize) -> Value {
        self.values[index]
    }
}
#[derive(Copy, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    BigNumber(usize),
    OBJ(Obj),
    Empty,
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(r) => {
                let rep = format!("{}", r);
                f.write_str(&rep)
            }
            Value::Number(r) => {
                let rep = format!("{}", r);
                f.write_str(&rep)
            }

            Value::OBJ(obj) => match obj {
                Obj::STRING(string @ ObjString { .. }) => f.write_str(&string.as_str()),
                Obj::FUNCTION(function @ ObjFunction { .. }) => {
                    let funcName = if function.name.is_empty() {
                        "<script>".to_string()
                    } else {
                        function.as_String()
                    };
                    f.write_str(&funcName)
                }
                Obj::NATIVE_FUNCTION(native @ NativeFunction { .. }) => f.write_str("<native fn>"),
                Obj::CLOSURE(closure @ ObjClosure { .. }) => {
                    let funcName = if closure.function.name.is_empty() {
                        "<script>".to_string()
                    } else {
                        closure.function.as_String()
                    };
                    f.write_str(&funcName)
                }
                Obj::CLASS(class @ ObjClass { .. }) => f.write_str(&format!("{:?}", &class)),
                Obj::INSTANCE(instance @ ObjInstance { .. }) => {
                    f.write_str(&format!("{:?}", instance))
                }
                Obj::BOUND_METHOD(boundMethod @ ObjBoundMethod { .. }) => {
                    f.write_str(boundMethod.getMethodName())
                }
            },

            Value::Empty => f.write_str("null"),
            Value::BigNumber(num) => f.write_str(&num.to_string()),
        }
    }
}

impl Value {
    pub fn boolValue(value: bool) -> Self {
        Value::Bool(value)
    }
    pub fn nativeFn(name: ObjString) -> Self {
        Value::OBJ(Obj::NATIVE_FUNCTION(NativeFunction::new(name)))
    }

    pub fn soft_clone(&self) -> Self {
        match self {
            Value::OBJ(Obj::STRING(p @ ObjString { .. })) => Value::OBJ(Obj::STRING(p.clone())),
            x => x.clone(),
        }
    }

    pub fn nilValue() -> Self {
        Value::Number(0_f64)
    }
    pub fn numberValue(number: f64) -> Self {
        Value::Number(number)
    }

    pub fn bigNumber(value: usize) -> Self {
        Value::BigNumber(value)
    }

    pub fn objValue(obj: Obj) -> Self {
        Value::OBJ(obj)
    }

    pub fn asBool(self) -> bool {
        match self {
            p @ Value::Bool(b) => b,
            x => panic!("{:?} is not a bool", x),
        }
    }

    pub fn asObjString(self) -> ObjString {
        match self {
            Value::OBJ(Obj::STRING(obj @ ObjString { .. })) => obj,
            x => panic!("{:?} is not an obj string", x),
        }
    }

    pub fn asFunction(self) -> ObjFunction {
        match self {
            Value::OBJ(Obj::FUNCTION(func @ ObjFunction { .. })) => func,
            x => panic!("{:?} is not an function string", x),
        }
    }

    pub fn asClosure(self) -> ObjClosure {
        match self {
            Value::OBJ(Obj::CLOSURE(closure @ ObjClosure { .. })) => closure,
            x => panic!("{:?} is not an function string", x),
        }
    }

    pub fn asClass(self) -> ObjClass {
        match self {
            Value::OBJ(Obj::CLASS(class @ ObjClass { .. })) => class,
            x => panic!("{:?} is not an class", x),
        }
    }

    pub fn asInstance(self) -> ObjInstance {
        match self {
            Value::OBJ(Obj::INSTANCE(instance @ ObjInstance { .. })) => instance,
            x => panic!("{:?} is not an instance", x),
        }
    }

    pub fn asBoundMethod(self) -> ObjBoundMethod {
        match self {
            Value::OBJ(Obj::BOUND_METHOD(method @ ObjBoundMethod { .. })) => method,
            x => panic!("{:?} is not a bound method", x),
        }
    }

    pub fn isInstance(&self) -> bool {
        match self {
            Value::OBJ(Obj::INSTANCE(ObjInstance { .. })) => true,
            x => false,
        }
    }

    pub fn isClass(&self) -> bool {
        match self {
            Value::OBJ(Obj::CLASS(ObjClass { .. })) => true,
            x => false,
        }
    }

    pub fn asNumber(self) -> f64 {
        match self {
            Value::Number(r) => r,
            x => panic!("{:?} is not a number", x),
        }
    }
    pub fn empty() -> Self {
        Value::Empty
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Value::Empty => true,
            _ => false,
        }
    }

    pub fn makeClass(className: ObjString, methodTableIndex: usize) -> Value {
        Value::OBJ(Obj::CLASS(ObjClass::new(className, methodTableIndex)))
    }
    pub fn makeBoundMethod(boundMethod: ObjBoundMethod) -> Value {
        Value::OBJ(Obj::BOUND_METHOD(boundMethod))
    }

    pub fn makeClosure(closure: ObjClosure) -> Value {
        Value::OBJ(Obj::CLOSURE(closure))
    }
    pub fn makeInstance(instance: ObjInstance) -> Value {
        Value::OBJ(Obj::INSTANCE(instance))
    }

    pub fn isNil(&self) -> bool {
        match self {
            Value::Number(num) => *num == 0_f64,
            _ => false,
        }
    }

    pub fn free(self) {
        match self {
            Value::OBJ(obj) => obj.free(),
            _ => (),
        }
    }
}
