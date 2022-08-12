extern crate core;

mod chunk;
mod value;
mod vm;
mod compiler;
mod scanner;


use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom, Write};
use compiler::*;
use value::*;
use chunk::*;
use vm::*;


#[warn(unused_imports)]
fn main() {

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
        if (buffer.eq("quit")) {
            break;
        } else {
            match  vm.interpret(buffer.into_bytes()) {
                InterpretResult::INTERPRET_OK => {}
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
