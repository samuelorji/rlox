use crate::scanner::*;
use crate::Chunk;
// pub fn compile(source : Vec<u8>) {
//     let mut scanner = Scanner::new(&source);
//
//
//     loop {
//         let token =  scanner.scanTokens();
//         print!("{:?}\n",&token);
//         match token.tokenType {
//              TokenType::EOF => break,
//             _ => ()
//
//         }
//     }
//
// }


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
}

impl<'a> Compiler<'a> {
    pub fn compile(&mut self, chunk: &mut Chunk) -> bool {
        self.setScanner();
        self.advance();
        self.expression();
        self.consume(TokenType::EOF, "Expect end of expression.");

        !self.parser.hadError
    }

    pub fn expression(&mut self) {}

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
                    return;
                }
                self.errorAtCurrent(message);
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
    pub fn new(sourcer: &'a [u8]) -> Self {
        Self {
            source: sourcer,
            parser: Parser::new(),
            scanner: Scanner::empty(),

        }
    }
}

pub fn compile(source: Vec<u8>, chunk: &mut Chunk) -> bool {
    ///advance();
//   expression();
//   consume(TOKEN_EOF, "Expect end of expression.");

    let mut scanner = Scanner::new(&source);

    true
}

