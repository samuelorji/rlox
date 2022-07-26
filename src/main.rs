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
use crate::InterpretResult::INTERPRET_OK;

#[warn(unused_imports)]
fn main() {

    let mut vm = VM::new();

    let args : Vec<String> = std::env::args().collect();


  let (interpretResult,mut vm) =   match args.len() {
        1 => repl(vm),
        2 => runFile(&args[1],vm),
        _ => {
            eprintln!("usage rlox [path]\n");
            (InterpretResult::INTERPRET_OK,vm)
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

fn repl(mut vm : VM)  -> (InterpretResult,VM) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer);
        //std::io::stdin().read(&mut buffer).unwrap();
        //std::io::stdin().readL(&mut buffer).unwrap();
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

    (InterpretResult::INTERPRET_OK,vm)
}

fn interpret(source : &[u8]) -> InterpretResult {
    INTERPRET_OK
}

fn runFile(path : &str,mut vm : VM) -> (InterpretResult,VM) {
   let res = match  read_file(path) {
       Ok(source) => {
           vm.interpret(source)
       }
       Err(e) =>
           {
               eprintln!("Could not read file : {:?}", e);
               std::process::exit(65)
           }
   };
    (res,vm)
}

fn read_file(path : &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut reader =  std::io::BufReader::new(file);
    let mut source : Vec<u8> = Vec::new();
    reader.read_to_end(&mut source)?;
    source.push('\0' as u8);

    Ok(source)
}
