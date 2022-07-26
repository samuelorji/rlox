use std::fmt::{Debug, Display, Formatter};
use TokenType::*;
pub struct Scanner {
    start :  usize,
    current: usize,
    line : usize,
}

impl Scanner{
   pub fn new() -> Self {
        Scanner {
            start : 0,
            current: 0,
            line: 1
        }
    }

    pub fn scanTokens<'a>(&mut self, source : &'a [u8]) -> Token<'a> {
        self.start = self.current;

        let c = self.advance(source);

        match  (*c as char) {
             '('=> return self.makeToken(source,TOKEN_LEFT_PAREN),
             ')'=> return self.makeToken(source,TOKEN_RIGHT_PAREN),
             '{'=> return self.makeToken(source,TOKEN_LEFT_BRACE),
             '}'=> return self.makeToken(source,TOKEN_RIGHT_BRACE),
             ';'=> return self.makeToken(source,TOKEN_SEMICOLON),
             ','=> return self.makeToken(source,TOKEN_COMMA),
             '.'=> return self.makeToken(source,TOKEN_DOT),
             '-'=> return self.makeToken(source,TOKEN_MINUS),
             '+'=> return self.makeToken(source,TOKEN_PLUS),
             '/'=> return self.makeToken(source,TOKEN_SLASH),
             '*'=> return self.makeToken(source,TOKEN_STAR),
             '!'=>  {
                 let tokenType = self.tryToMatch(source,'=',TOKEN_BANG_EQUAL,TOKEN_BANG);
                 return self.makeToken(source,tokenType)
             },
            _ => self.errorToken("error token")
        }
        // if (self.isAtEnd(source)) {
        //    return self.makeToken(source,TOKEN_EOF)
        // }
        //
        // return self.errorToken("Unexpected character.");

    }

    pub fn tryToMatch(&mut self, source : &[u8] , expected: char, onMatch : TokenType, onNoMatch : TokenType) -> TokenType {
       match self.matchChar(source,expected)  {
            true => onMatch,
            false => onNoMatch
        }
    }

    pub fn matchChar(&mut self, source: &[u8], expected: char) -> bool {
        if (self.isAtEnd(source)) {
            return false;
        }
        if (source[self.current] != (expected as u8)) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn advance<'a>(&mut self, source : &'a [u8]) -> &'a u8 {
        self.current+=1;
        &source[self.current -1]

    }

    fn makeToken<'a>(&self, source : &'a [u8], tokenType: TokenType) -> Token<'a> {
        Token {
            tokenType,
            start: &source[self.start .. self.current],
            line : self.line
        }
    }

    pub fn isAtEnd(&self, source : &[u8]) -> bool {
        source[self.current] == b'0'
    }

    pub fn errorToken<'a>(&self, message : &'a str ) -> Token<'a> {
        Token {
            tokenType : TOKEN_ERROR,
            start: message.as_bytes(),
            line : self.line
        }
    }


}

 pub struct Token<'a> {
     tokenType : TokenType,
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
 enum TokenType {
    // Single-character tokens.
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG, TOKEN_BANG_EQUAL,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_LESS, TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
    TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
    TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
    TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

    TOKEN_ERROR, TOKEN_EOF
}
pub fn initScanner(source : String) {

}