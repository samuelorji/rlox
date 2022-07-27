use std::fmt::{Debug, Display, Formatter};
use TokenType::*;
pub struct Scanner<'a> {
    source: &'a [u8],
    start :  usize,
    current: usize,
    line : usize,
}

impl<'a> Scanner<'a>{
   pub fn new(source: &'a [u8] ) -> Scanner {
        Scanner {
            source,
            start : 0,
            current: 0,
            line: 1
        }
    }

    pub fn scanTokens(&mut self) -> Token<'a> {
        self.start = self.current;

        self.skipWhiteSpace();
        if(self.isAtEnd()) {
            return self.makeToken(EOF)
        }

        let c = self.advance();

        match  (*c as char) {
             '('=> return self.makeToken(LEFT_PAREN),
             ')'=> return self.makeToken(RIGHT_PAREN),
             '{'=> return self.makeToken(LEFT_BRACE),
             '}'=> return self.makeToken(RIGHT_BRACE),
             ';'=> return self.makeToken(SEMICOLON),
             ','=> return self.makeToken(COMMA),
             '.'=> return self.makeToken(DOT),
             '-'=> return self.makeToken(MINUS),
             '+'=> return self.makeToken(PLUS),
             '/'=> return self.makeToken(SLASH),
             '*'=> return self.makeToken(STAR),
             '!'=>  {
                 let tokenType = self.tryToMatch('=',BANG_EQUAL,BANG);
                 return self.makeToken(tokenType)
             },
            '>'=>  {
                let tokenType = self.tryToMatch('=',GREATER_EQUAL,GREATER);
                return self.makeToken(tokenType)
            },
            '<'=>  {
                let tokenType = self.tryToMatch('=',LESS_EQUAL,LESS);
                return self.makeToken(tokenType)
            },
            '='=>  {
                let tokenType = self.tryToMatch('=',EQUAL_EQUAL,EQUAL);
                return self.makeToken(tokenType)
            },
            c => {
                println!("error token : {}",c as u8);
                self.errorToken("error token")
            }
        }
    }

    pub fn tryToMatch(&mut self, expected: char, onMatch : TokenType, onNoMatch : TokenType) -> TokenType {
       match self.matchChar(expected)  {
            true => onMatch,
            false => onNoMatch
        }
    }

    pub fn matchChar(&mut self, expected: char) -> bool {
        let current = self.current;
        if (self.isAtEnd()) {
            return false;
        }
        if (self.source[current] != (expected as u8)) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn advance(&mut self) -> &'a u8 {
        self.current+=1;
        &self.source[self.current -1]

    }

    fn makeToken(&self, tokenType: TokenType) -> Token<'a> {
        Token {
            tokenType,
            start: &self.source[self.start .. self.current],
            line : self.line
        }
    }

    pub fn isAtEnd(&self) -> bool {
        let current = self.current;
        self.source[current] == b'\0'
    }

    pub fn skipWhiteSpace(&mut self) {
        loop {
            let peeked = self.peek();
            match (*peeked as char){
                ' ' | '\r'  | '\t' =>  {
                    self.advance();
                    break
                },
                '\n' => {
                    self.advance();
                    // we do this not to capture the \n
                    self.start +=1;
                    self.line +=1;
                    break;
                }
                _ => {

                    break;
                }
            }

        }
    }

    pub fn skip(&mut self) {

    }

    pub fn peek(&self) -> &u8 {
        let current = self.current;
        self.source.get(current).unwrap()
    }

    pub fn errorToken(&self, message : &'a str ) -> Token<'a> {
        Token {
            tokenType : ERROR,
            start: message.as_bytes(),
            line : self.line
        }
    }


}

 pub struct Token<'a> {
     pub tokenType : TokenType,
     start: &'a [u8],
     line: usize
 }

// impl Display for Token {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         let tokenString = format!("Token {{ tokenType: {}, lexeme : {}, line : {}",self.tokenType,self.start,self.line)
//     }
// }

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let lexeme = std::str::from_utf8(self.start).unwrap();
        let tokenString = format!("Token {{ tokenType: {:?}, lexeme : \"{}\", line : {} }}",self.tokenType,lexeme,self.line);
        f.write_str(&tokenString)
    }
}

impl <'a> Token<'a> {
    pub fn length(&self) -> usize {
        self.start.len()
    }



}

#[derive(Debug)]
 pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF
}
pub fn initScanner(source : String) {

}