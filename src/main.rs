extern crate core;

mod chunk;
mod compiler;
mod object;
mod scanner;
mod table;
mod value;
mod vm;

use object::*;
use table::*;

use chunk::*;
use compiler::*;
use std::borrow::Cow;
use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom, Write};
use std::mem::size_of;
use std::path::Path;
use std::str::Split;
use value::*;
use vm::*;

fn main() {
    let mut vm = VM::new();

    let args: Vec<String> = std::env::args().collect();

    let interpretResult = match args.len() {
        1 => repl(&mut vm),
        2 => runFile(&args[1], &mut vm),
        _ => {
            eprintln!("usage rlox [path]\n: {:?}", &args);
            (InterpretResult::INTERPRET_OK)
        }
    };

    vm.free();

    match interpretResult {
        InterpretResult::INTERPRET_OK => {}
        InterpretResult::INTERPRET_COMPILE_ERROR => std::process::exit(65),
        InterpretResult::INTERPRET_RUNTIME_ERROR => std::process::exit(70),
    }
}

fn repl(vm: &mut VM) -> InterpretResult {
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
            match vm.interpret(buffer.into_bytes()) {
                InterpretResult::INTERPRET_OK => {}
                InterpretResult::INTERPRET_COMPILE_ERROR => {}
                InterpretResult::INTERPRET_RUNTIME_ERROR => {}
            }
        }
    }

    InterpretResult::INTERPRET_OK
}

fn runFile(path: &str, vm: &mut VM) -> InterpretResult {
    match read_file(path) {
        Ok(source) => vm.interpret(source),
        Err(e) => {
            eprintln!("Could not read file : {:?}", e);
            std::process::exit(65)
        }
    }
}

fn read_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut reader = std::io::BufReader::new(file);
    let mut source: Vec<u8> = Vec::new();
    reader.read_to_end(&mut source)?;
    source.push('\0' as u8);
    Ok(source)
}
