use std::str::FromStr;
use crate::scanner::*;
use crate::{Chunk, OpCode, Value};
use crate::OpCode::{OP_CONSTANT, OP_RETURN};
use crate::scanner::TokenType::RIGHT_PAREN;

pub fn compiled(source : Vec<u8>) {
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


pub struct Parser<'a> {
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    hadError: bool,
    panicMode: bool,
}

impl<'a> Parser<'a> {
    fn new() -> Self {
        Self {
            current: None,
            previous: None,
            hadError: false,
            panicMode: false,
        }
    }
}

pub struct Compiler<'a> {
    source: &'a [u8],
    parser: Parser<'a>,
    scanner: Scanner<'a>,
    chunk : &'a mut Chunk
}

impl<'a> Compiler<'a> {
    pub fn compile(&mut self) -> bool {
        self.setScanner();
        self.advance();
        self.expression();
        self.consume(TokenType::EOF, "Expect end of expression.");

        self.end();
        !self.parser.hadError
    }

    fn emitByte(&mut self,byte: u8) {
        self.chunk.write(byte,self.parser.previous.unwrap().line)
    }

    fn parsePrecedence(&mut self, precedence : Precedence) {

    }
    fn unary(&mut self) {
        let operatorType = self.parser.previous.unwrap().tokenType;

        self.expression();

        self.parsePrecedence(Precedence::UNARY);

        match operatorType {
            TokenType::MINUS => {
                self.emitByte(OpCode::OP_NEGATE.to_u8())
            },
            _ => ()
        }
    }

    pub fn end(&mut self) {
        self.emitReturn()

    }
    fn grouping(&mut self, chunk : &mut Chunk) {
        self.expression();
        self.consume(RIGHT_PAREN, "Expect a ')' after a grouping")
    }

    pub fn number(&mut self) {

        //f32::from
        let value : Value = f32::from_str(std::str::from_utf8(self.parser.previous.unwrap().start).unwrap()).unwrap();
        let constantIndex = self.makeConstant(value);
        // write constant and constant index
        self.emitBytes(OP_CONSTANT.to_u8(),constantIndex);

    }

    pub fn makeConstant(&mut self, value : Value) -> u8 {
        let constantIndex =  self.chunk.addConstant(value);
        if(constantIndex > u8::MAX as u32) {
            self.error("Too many constants in one chunk.");
             0
        } else {
            constantIndex as u8
        }
    }

    pub fn emitBytes(&mut self, byte1 : u8, byte2 : u8) {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }


    pub fn emitReturn(&mut self) {
        self.chunk.write(OP_RETURN.to_u8(),self.parser.previous.unwrap().line)
    }

    pub fn expression(&mut self) {
        self.parsePrecedence(Precedence::ASSIGNMENT)
    }

    pub fn advance(&mut self) {

        // self.parser.current.mut
        let current = self.parser.current.take();
        self.parser.previous = current;
        loop {
            let scannedToken = self.scanner.scanTokens();
            if (scannedToken.tokenType != TokenType::ERROR) {
                self.parser.current = Some(scannedToken);
                break;
            }
        }
    }
    fn errorAtCurrent(&mut self, message: &str) {
        let currentToken = self.parser.current.take().unwrap();
        self.errorAt(currentToken, message)
    }

    pub fn error(&mut self, message : &str) {
        self.errorAt(self.parser.previous.unwrap(),message);
    }
    fn errorAt(&mut self, token: Token<'a>, message: &str) {
        ///static void errorAt(Token* token, const char* message) {
        //   fprintf(stderr, "[line %d] Error", token->line);
        //
        //   if (token->type == TOKEN_EOF) {
        //     fprintf(stderr, " at end");
        //   } else if (token->type == TOKEN_ERROR) {
        //     // Nothing.
        //   } else {
        //     fprintf(stderr, " at '%.*s'", token->length, token->start);
        //   }
        //
        //   fprintf(stderr, ": %s\n", message);
        //   parser.hadError = true;
        // }

        if (self.parser.panicMode) {
            return;
        }
        self.parser.panicMode = true;
        eprintln!("[line {}] Error", token.line);

        match token.tokenType {
            TokenType::EOF => eprintln!(" at end"),
            TokenType::ERROR => (),
            _ => eprintln!(" at {}", std::str::from_utf8(token.start).unwrap())
        }
        self.parser.hadError = true;
    }

    fn consume(&mut self, tokenType: TokenType, message: &str) {
        match &self.parser.current {
            Some(token) => {
                if (token.tokenType == tokenType) {
                    self.advance();

                } else {
                    self.errorAtCurrent(message)
                }
            }

            _ => ()
        }

        // let token = self.parser.current.take().unwrap();
        // if(token.tokenType == tokenType) {
        //     self.advance();
        //     return;
        // }
        // self.errorAtCurrent(message)
    }

    fn setScanner(&mut self) {
        self.scanner = Scanner::new(&self.source)
    }
    pub fn new(sourcer: &'a [u8], chunk : &'a mut Chunk) -> Self {
        Self {
            source: sourcer,
            parser: Parser::new(),
            scanner: Scanner::empty(),
            chunk

        }
    }
}

enum Precedence {
    NONE,
    ASSIGNMENT,  // =
    OR,          // or
    AND,         // and
    EQUALITY,    // == !=
    COMPARISON,  // < > <= >=
    TERM,        // + -
    FACTOR,      // * /
    UNARY,       // ! -
    CALL,        // . ()
    PRIMARY,
}


pub fn compile(source: Vec<u8>, chunk: &mut Chunk) -> bool {
    ///advance();
//   expression();
//   consume(TOKEN_EOF, "Expect end of expression.");

    let mut scanner = Scanner::new(&source);

    true
}

