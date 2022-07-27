use crate::scanner::*;
pub fn compile(source : Vec<u8>) {
    let mut scanner = Scanner::new(&source);

     let token =  scanner.scanTokens();
     print!("{:?}",&token);
}