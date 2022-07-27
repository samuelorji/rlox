use crate::scanner::*;
pub fn compile(source : Vec<u8>) {
    let mut scanner = Scanner::new(&source);


    loop {
        let token =  scanner.scanTokens();
        print!("{:?}\n",&token);
        match token.tokenType {
             TokenType::EOF => break,
            _ => ()

        }
    }

}