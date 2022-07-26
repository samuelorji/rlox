use crate::scanner::*;
pub fn compile(source : Vec<u8>) {
    let mut scanner = Scanner::new();

    let token =  scanner.scanTokens(&source);
    print!("token called{:?}",&token);
}