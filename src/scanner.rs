use std::fmt::{Debug, Display, Formatter};
use std::ptr::eq;
use std::thread::current;
use TokenType::*;

pub struct Scanner<'a> {
    source: &'a [u8],
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a [u8]) -> Scanner {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn empty() -> Scanner<'a> {
        Scanner {
            source : &[],
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn scanTokens(&mut self) -> Token<'a> {
        self.start = self.current;

        self.skipWhiteSpace();
        if (self.isAtEnd()) {
            return self.makeToken(EOF);
        }

        let c = self.advance();

        if (self.isAlpha(*c as char)) {
            return self.identifier();
        }
        if (self.isDigit(*c)) {
            return self.number();
        }

        match (*c as char) {
            '(' => self.makeToken(LEFT_PAREN),
            ')' => self.makeToken(RIGHT_PAREN),
            '{' => self.makeToken(LEFT_BRACE),
            '}' => self.makeToken(RIGHT_BRACE),
            ';' => self.makeToken(SEMICOLON),
            ',' => self.makeToken(COMMA),
            '.' => self.makeToken(DOT),
            '-' => self.makeToken(MINUS),
            '+' => self.makeToken(PLUS),
            '/' => self.makeToken(SLASH),
            '*' => self.makeToken(STAR),
            '"' => self.makeString(),
            '!' => {
                let tokenType = self.tryToMatch('=', BANG_EQUAL, BANG);
                self.makeToken(tokenType)
            }
            '>' => {
                let tokenType = self.tryToMatch('=', GREATER_EQUAL, GREATER);
                self.makeToken(tokenType)
            }
            '<' => {
                let tokenType = self.tryToMatch('=', LESS_EQUAL, LESS);
                self.makeToken(tokenType)
            }
            '=' => {
                let tokenType = self.tryToMatch('=', EQUAL_EQUAL, EQUAL);
                self.makeToken(tokenType)
            }
            c => {
                println!("error token : {}", c as u8);
                self.errorToken("error token")
            }
        }
    }

    fn makeString(&mut self) -> Token<'a> {
        let mut peeked = self.peek();
        while (*peeked != b'"') {
            if (*peeked == b'\0') {
                return self.errorToken("Unterminated sting");
            } else {
                if (*peeked == b'\n') {
                    self.line += 1
                }
                self.advance();
            }
            peeked = self.peek()
        }

        // when we reach here, we've encountered the closing quote (terminating part of the string)
        // and we can move ahead to cover the closing quote
        self.advance();
        self.makeToken(TokenType::STRING)
    }
    fn tryToMatch(&mut self, expected: char, onMatch: TokenType, onNoMatch: TokenType) -> TokenType {
        match self.matchChar(expected) {
            true => onMatch,
            false => onNoMatch
        }
    }

    fn isDigit(&mut self, byteValue: u8) -> bool {
        byteValue >= b'0' && byteValue <= b'9'
    }

    fn matchChar(&mut self, expected: char) -> bool {
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
       // self.start+=1;
        self.current += 1;
        &self.source[self.current - 1]
    }

    fn makeToken(&self, tokenType: TokenType) -> Token<'a> {
        Token {
            tokenType,
            start: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn isAtEnd(&self) -> bool {
        let current = self.current;
        self.source[current] == b'\0'
    }

    fn skipWhiteSpace(&mut self) {
        // we keep loooping to skip white spaces or comments or new lines
        loop {
            let peeked = self.peek();
            match (*peeked as char) {
                ' ' | '\r' | '\t' => {
                    self.advance();
                    self.start += 1;
                }
                '\n' => {
                    self.advance();
                    // we do this not to capture the \n
                    self.start += 1;
                    self.line += 1;
                }
                '/' => {
                    // if next is / then that's a comment
                    if (*(self.peekNext()) == b'/') {
                        // this is a comment
                        // now scan until you encounter a new line and we're not at the end
                        // once we encounter a new line, we come out of the while loop
                        // into the outer loop which then runs again and matches against '\n'
                        while (!self.isAtEnd() && *(self.peek()) != b'\n') {
                            self.advance();
                            self.start += 1;
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn isAlpha(&self, character: char) -> bool {
        (character >= 'a' && character <= 'z') ||
            (character >= 'A' && character <= 'Z') || character == '_'
    }

    fn identifier(&mut self) -> Token<'a> {
        while (self.isAlpha(*self.peek() as char) || self.isDigit(*self.peek())) {
            self.advance();
        };
        self.makeToken(self.identifierType())
    }

    fn identifierType(&self) -> TokenType {
        match self.source[self.start] as char {
            'a' => return self.checkKeyword(1, 2, b"nd", AND),
            'e' => return self.checkKeyword(1, 3, b"lse", ELSE),
            'c' => return self.checkKeyword(1, 4, b"lass", CLASS),
            'i' => return self.checkKeyword(1, 1, b"f", IF),
            'n' => return self.checkKeyword(1, 2, b"il", NIL),
            'o' => return self.checkKeyword(1, 1, b"r", OR),
            'p' => return self.checkKeyword(1, 4, b"rint", PRINT),
            'r' => return self.checkKeyword(1, 5, b"eturn", RETURN),
            's' => return self.checkKeyword(1, 4, b"uper", SUPER),
            'v' => return self.checkKeyword(1, 2, b"ar", VAR),
            'w' => return self.checkKeyword(1, 4, b"hile", WHILE),
            'f' => {
                if (self.current - self.start > 1) {
                    // we've captured more than just f, so f and at least another character
                    match self.source[self.start + 1] as char {
                        'o' => return self.checkKeyword(2, 1, b"r", FOR),
                        'a' => return self.checkKeyword(2, 3, b"lse", FALSE),
                        'u' => return self.checkKeyword(2, 1, b"n", FUN),
                        _ => ()
                    };
                };
            }
            't' => {
                if (self.current - self.start > 1) {
                    // we've captured more than just t, so t and at least another character
                    match self.source[self.start + 1] as char {
                        'h' => return self.checkKeyword(2, 2, b"is", THIS),
                        'r' => return self.checkKeyword(2, 2, b"ue", TRUE),
                        _ => ()
                    }
                }
            }
            _ => ()
        };
        IDENTIFIER
    }

    fn checkKeyword(&self, start: usize, length: usize, rest: &[u8], tokenType: TokenType) -> TokenType {
        // check if self.source[self.start + start - start] == rest ,
        // e.g for else case, check if lse == lse
        if (self.current - self.start == start + length && &self.source[self.start + start..self.current] == rest) {
            return tokenType;
        }
        IDENTIFIER
    }

    fn memcmp(&self, a: &[u8], b: &[u8], length: usize) -> bool {
        if (a.len() != length || b.len() != length) {
            return false;
        } else {
            a == b
        }
    }


    fn skip(&mut self) {}

    fn peek(&self) -> &u8 {
        let current = self.current;
        self.source.get(current).unwrap()
    }

    fn peekNext(&self) -> &u8 {
        if (self.isAtEnd()) {
            &b'\0'
        } else {
            let current = self.current;
            self.source.get(current + 1).unwrap()
        }
    }

    fn errorToken(&self, message: &'a str) -> Token<'a> {
        Token {
            tokenType: ERROR,
            start: message.as_bytes(),
            line: self.line,
        }
    }
    fn number(&mut self) -> Token<'a> {
        // let mut peeked = self.peek();
        while (self.isDigit(*self.peek())) {
            self.advance();
        }

        // we've captured all whole numbers above,
        // now for fractional parts

        if (*self.peek() == b'.' && self.isDigit(*self.peekNext())) {
            self.advance();
            while (self.isDigit(*self.peek())) {
                self.advance();
            }
        }
        self.makeToken(TokenType::NUMBER)
    }
}

#[derive(Copy,Clone)]
pub struct Token<'a> {
    pub tokenType: TokenType,
    pub start: &'a [u8],
    pub line: usize,
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        if(self.tokenType != other.tokenType) {
            return false
        } else {
            self.start == other.start
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}


// impl Display for Token {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         let tokenString = format!("Token {{ tokenType: {}, lexeme : {}, line : {}",self.tokenType,self.start,self.line)
//     }
// }

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let lexeme = std::str::from_utf8(self.start).unwrap();
        let tokenString = format!("Token {{ tokenType: {:?}, lexeme : \"{}\", line : {} : raw-bytes : {:?} }}", self.tokenType, lexeme, self.line,self.start);
        f.write_str(&tokenString)
    }
}

impl<'a> Token<'a> {
    pub fn length(&self) -> usize {
        self.start.len()
    }

    pub fn fromStr(tokenStr : &'a str) -> Self {
        Self {
            tokenType: TokenType::IDENTIFIER,
            start: tokenStr.as_bytes(),
            line: 0
        }
    }

    pub fn this() -> Self {
        Self {
            tokenType: TokenType::THIS,
            start: "this".as_bytes(),
            line: 0
        }
    }
    
    pub fn empty() -> Self {
        Self {
            tokenType: TokenType::EMPTY,
            start: &[],
            line: 0
        }
    }

    pub fn lexeme(&self) -> &str {
        std::str::from_utf8(self.start).unwrap()
    }
}

//
// impl PartialEq for TokenType {
//     fn eq(&self, other: &Self) -> bool {
//         self as &u8 == other as &u8
//     }
//
//     fn ne(&self, other: &Self) -> bool {
//         self as u8 != other as u8
//     }
// }

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN = 0,
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
    EOF,
    EMPTY, // must be last
}

impl TokenType {
    pub fn as_u8(&self) -> u8 {
        *self as u8
    }
    pub fn as_usize(&self) -> usize {
        *self as usize
    }
    pub fn elements_len() -> u8 {
        TokenType::EMPTY.as_u8() + 1
    }
}

pub fn initScanner(source: String) {}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn testParseSourceWithComment() {

        // Explicit tokens used
        let mut source = String::from(r#"
!
//hello
="#);
        source.push('\0'); // add null terminating byte

        let mut sourceCode = source.trim().as_bytes();

        let mut scanner = Scanner::new(sourceCode);
        let mut scannedToken = scanner.scanTokens();

        let bang = Token {
            tokenType: TokenType::BANG,
            start: &sourceCode[0..1],
            line: 1,
        };

        assert_eq!(bang, scannedToken);

        scannedToken = scanner.scanTokens();

        let equalsSign =
            Token {
                tokenType: TokenType::EQUAL,
                start: &sourceCode[10..11],
                line: 3,
            };
        assert_eq!(equalsSign, scannedToken);

        // we use start as an empty slice because EOF doesn't capture the null byte
        // but returns an empty slice instead
        let eofToken =
            Token {
                tokenType: TokenType::EOF,
                start: &sourceCode[11..11], // could be anything to denote an empty slice 1..1, 2..2
                line: 3,
            };
        scannedToken = scanner.scanTokens();
        assert_eq!(eofToken, scannedToken);
    }
}