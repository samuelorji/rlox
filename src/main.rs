extern crate core;

mod chunk;
mod value;
mod vm;
mod compiler;
mod scanner;
mod object;
mod table;

use object::*;
use table::*;


use std::borrow::Cow;
use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;
use std::str::Split;
use compiler::*;
use value::*;
use chunk::*;
use vm::*;


fn main() {

    /**
     var i = 2;

    {
    var a = 3;
    print a;
    {
    var b= 2;
    print b;
    }
    }

    fun sam(){
    0;
    }

    fun git(){
    var a = 3;
    }

     */

    // let mut map = Table::new();
    //
    // // these two collide
    // map.set(ObjString::from_buffer("costarring".as_bytes()),Value::bool_value(true));
    // map.set(ObjString::from_buffer("liquid".as_bytes()),Value::bool_value(true));
    //
    // // these collide
    // map.set(ObjString::from_buffer("declinate".as_bytes()),Value::bool_value(true));
    // map.set(ObjString::from_buffer("macallums".as_bytes()),Value::bool_value(true));
    //
    // println!("size of obj string {:?}",std::mem::size_of::<*mut u8>());
    // println!("size of obj string {:?}",std::mem::size_of::<u32>());
    // println!("size of obj string {:?}",std::mem::size_of::<usize>());
    // println!("size of obj string {:?}",std::mem::size_of::<bool>());

    let mut vm = VM::new();

    let args: Vec<String> = std::env::args().collect();

    let interpretResult = match args.len() {
        1 => repl(&mut vm),
        2 => runFile(&args[1], &mut vm),
        _ => {
            eprintln!("usage rlox [path]\n: {:?}",&args);
            (InterpretResult::INTERPRET_OK)
        }
    };

    vm.free();




    //
    // let mut chunk = Chunk::new();
    //
    // let constantIndex = chunk.addConstant(1.2);
    // chunk.write(OpCode::OP_CONSTANT.to_u8(),123);
    // chunk.write(constantIndex as u8,123);
    //
    //
    // let constantIndex = chunk.addConstant(2.5);
    // chunk.write(OpCode::OP_CONSTANT.to_u8(),123);
    // chunk.write(constantIndex as u8,123);
    //
    // chunk.write(OpCode::OP_MULTIPLY.to_u8(), 123);
    // chunk.write(OpCode::OP_NEGATE.to_u8(),123);
    // chunk.write(OpCode::OP_RETURN.to_u8(),123);
    //
    // //chunk.disassemble("test chunk");


}

fn repl(vm : &mut VM)  -> InterpretResult {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer);

        if (buffer.trim().eq("quit")) {
            println!("Thanks for using Lox");
            break;
        } else {
            buffer.push('\0' as char);
            match  vm.interpret(buffer.into_bytes()) {
                InterpretResult::INTERPRET_OK => {

                }
                InterpretResult::INTERPRET_COMPILE_ERROR => {}
                InterpretResult::INTERPRET_RUNTIME_ERROR => {}
            }
        }
    }

    InterpretResult::INTERPRET_OK
}

fn runFile(path : &str,vm : &mut VM) -> InterpretResult {
   match  read_file(path) {
       Ok(source) => {
           vm.interpret(source)
       }
       Err(e) =>
           {
               eprintln!("Could not read file : {:?}", e);
               std::process::exit(65)
           }
   }
}

fn read_file(path : &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut reader =  std::io::BufReader::new(file);
    let mut source : Vec<u8> = Vec::new();
    reader.read_to_end(&mut source)?;
    source.push('\0' as u8);
    Ok(source)
}
