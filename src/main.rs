mod chunk;
mod value;
mod vm;

use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom, Write};
use value::*;
use chunk::*;
use vm::*;
use crate::InterpretResult::INTERPRET_OK;

#[warn(unused_imports)]
fn main() {

    let mut vm = VM::new();

    let args : Vec<String> = std::env::args().collect();


  let interpretResult =   match args.len() {
        1 => repl(),
        2 => runFile(&args[1]),
        _ => eprintln!("usage rlox [path]\n")
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

fn repl()  -> InterpretResult {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut buffer: String = String::new();
        std::io::stdin().read_line(&mut buffer);
        interpret(buffer)
    }
}

fn interpret(line : String) -> InterpretResult {
    println!("received : {line}")
    INTERPRET_OK
}
fn runFile(path : &str) -> InterpretResult {
   match  read_file(path) {
       Ok(source) => {
           interpret(source)
       }
       Err(e) => eprintln!("Could not read file : {:?}",e)
   }
}

fn read_file(path : &str) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut reader =  std::io::BufReader::new(file);
    let mut source : String = String::new();
    reader.read_to_string(&mut source)?;
    Ok(source)
}
