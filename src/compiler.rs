use std::str::FromStr;
use crate::scanner::*;
use crate::{Chunk, OpCode, Value};
use crate::OpCode::{OP_CONSTANT, OP_RETURN};
use crate::scanner::TokenType::RIGHT_PAREN;



#[derive(Copy,Clone,Debug)]
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

impl Precedence {
    pub fn to_u8(&self) -> u8 {
        *self as u8
    }
    pub fn next(&self) -> Self {
        match self {
            Precedence::NONE => Precedence::ASSIGNMENT,
            Precedence::ASSIGNMENT => Precedence::OR,
            Precedence::OR => Precedence::AND,
            Precedence::AND => Precedence::EQUALITY,
            Precedence::EQUALITY => Precedence::COMPARISON,
            Precedence::COMPARISON => Precedence::TERM,
            Precedence::TERM => Precedence::FACTOR,
            Precedence::FACTOR => Precedence::UNARY,
            Precedence::UNARY => Precedence::CALL,
            Precedence::CALL => Precedence::PRIMARY,
            Precedence::PRIMARY => panic!("no precedence after primary"),
        }
    }
}

impl Precedence {
    fn list() -> Vec<u8> {
        vec![
            Precedence::NONE.to_u8(),
            Precedence::ASSIGNMENT.to_u8(),  // =
            Precedence::OR.to_u8(),          // or
            Precedence::AND.to_u8(),         // and
            Precedence::EQUALITY.to_u8(),    // == !=
            Precedence::COMPARISON.to_u8(),  // < > <= >=
            Precedence::TERM.to_u8(),        // + -
            Precedence::FACTOR.to_u8(),      // * /
            Precedence::UNARY.to_u8(),       // ! -
            Precedence::CALL.to_u8(),        // . ()
            Precedence::PRIMARY.to_u8(),
        ]
    }

}
#[derive(Copy,Clone)]
struct ParseRule {
    prefix: Option<fn(&mut Compiler) -> ()>,
    infix: Option<fn(&mut Compiler) -> ()>,
    precedence: Precedence,
}

impl ParseRule {
    fn new() -> Self  {
        Self {
            prefix: None ,
            infix: None ,
            precedence : Precedence::NONE
        }
    }

}

impl ParseRule {

    /**
     [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_GREATER]       = {NULL,     NULL,   PREC_NONE},
    [TOKEN_GREATER_EQUAL] = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LESS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LESS_EQUAL]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_STRING]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
     */
    fn rules() -> Vec<ParseRule> {
       let mut rules =  vec![ParseRule::new(); TokenType::elements_len() as usize]; // create rules with default values
        rules[TokenType::LEFT_PAREN.as_usize()] =  createParseRule( Some(grouping),None,Precedence::NONE);
        rules[TokenType::MINUS.as_usize()] =  createParseRule( Some(unary),Some(binary),Precedence::TERM);
        rules[TokenType::PLUS.as_usize()] =  createParseRule( None,Some(binary),Precedence::TERM);
        rules[TokenType::SLASH.as_usize()] =  createParseRule( None,Some(binary),Precedence::FACTOR);
        rules[TokenType::STAR.as_usize()] =  createParseRule( None,Some(binary),Precedence::FACTOR);
        rules[TokenType::NUMBER.as_usize()] =  createParseRule( Some(number),None,Precedence::NONE);


        rules
    }


}

fn createParseRule(prefix : Option<fn(&mut Compiler) -> ()>, infix :Option<fn(&mut Compiler) -> ()>, precedence : Precedence ) -> ParseRule {
    ParseRule{
        prefix,
        infix,
        precedence
    }
}

pub struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    hadError: bool,
    panicMode: bool,
}

impl<'a> Parser<'a> {
    fn new() -> Self {
        Self {
            current: Token::empty(),
            previous: Token::empty(),
            hadError: false,
            panicMode: false,
        }
    }
}

pub struct Compiler<'a> {
    source: &'a [u8],
    parser: Parser<'a>,
    scanner: Scanner<'a>,
    chunk : &'a mut Chunk,
    parseRules : Vec<ParseRule>
}



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
        self.chunk.write(byte,self.parser.previous.line)
    }

    fn parsePrecedence(&mut self, precedence : Precedence) {
        self.advance();
        let token = self.parser.previous;
        let tokenType = token.tokenType;
        //println!("token  is {:?}, token type is {:?}, and precedence is {:?}", &token.lexeme(),&tokenType ,&precedence);
        let prefixRule  = self.getRule(tokenType).prefix;

        match prefixRule {
            None => self.error("Expect expression."),
            Some(rule) => rule(self)
        }

       // println!("previous is {:?} and current is {:?}", &self.parser.previous.lexeme(), &self.parser.current);
        // at this point the token has been consumed, previous is like literal, current is operator (like +)

        while (precedence.to_u8() <= self.getRule(self.parser.current.tokenType).precedence.to_u8()) {
            // move token to operator
            self.advance();
            let infixRule = self.getRule(self.parser.previous.tokenType).infix;
            match infixRule {
                None => (),
                Some(rule) => rule(self)
            }

        }
    }
    fn unary(&mut self) {
        let operatorType = self.parser.previous.tokenType;
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
        let value : Value = Value::number_value(f64::from_str(std::str::from_utf8(self.parser.previous.start).unwrap()).unwrap());
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
        self.chunk.write(OP_RETURN.to_u8(),self.parser.previous.line)
    }

    pub fn expression(&mut self) {
        self.parsePrecedence(Precedence::ASSIGNMENT)
    }

    pub fn advance(&mut self) {

        // self.parser.current.mut
        let current = self.parser.current;
        self.parser.previous = current;
        loop {
            let scannedToken = self.scanner.scanTokens();
            if (scannedToken.tokenType != TokenType::ERROR) {
                self.parser.current = scannedToken;
                break;
            }
        }
    }
    fn errorAtCurrent(&mut self, message: &str) {
        let currentToken = self.parser.current;
        self.errorAt(currentToken, message)
    }

    pub fn getRule(&self, tokenType : TokenType) -> &ParseRule {
        &self.parseRules[tokenType.as_u8() as usize]
    }

    pub fn error(&mut self, message : &str) {
        self.errorAt(self.parser.previous,message);
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
        eprint!("[line {}] Error", token.line);

        match token.tokenType {
            TokenType::EOF => eprint!(" at end"),
            TokenType::ERROR => (),
            _ => eprint!(" at {}", std::str::from_utf8(token.start).unwrap())
        }
        eprintln!(": {message}");
        self.parser.hadError = true;
    }

    fn consume(&mut self, tokenType: TokenType, message: &str) {

        if(self.parser.current.tokenType == tokenType){
            self.advance();
        } else {
            self.errorAtCurrent(message)
        }
        // match &self.parser.current {
        //     Some(token) => {
        //         if (token.tokenType == tokenType) {
        //             self.advance();
        //
        //         } else {
        //             self.errorAtCurrent(message)
        //         }
        //     }
        //
        //     _ => ()
        // }
    }

    fn setScanner(&mut self) {
        self.scanner = Scanner::new(&self.source)
    }
    pub fn new(sourcer: &'a [u8], chunk : &'a mut Chunk) -> Self {
        Self {
            source: sourcer,
            parser: Parser::new(),
            scanner: Scanner::empty(),
            chunk,
            parseRules : ParseRule::rules() // store default on the compiler

        }
    }
}



fn grouping<'a>(compiler: &mut Compiler<'a>) {
    compiler.expression();
    compiler.consume(RIGHT_PAREN, "Expect a ')' after a grouping")
}

fn unary<'a>(compiler: &mut Compiler<'a>){
    let operatorType = compiler.parser.previous.tokenType;
    compiler.expression();

    compiler.parsePrecedence(Precedence::UNARY);

    match operatorType {
        TokenType::MINUS => {
            compiler.emitByte(OpCode::OP_NEGATE.to_u8())
        },
        _ => ()
    }
}

fn binary<'a>(compiler: &mut Compiler<'a>){
    let operatorType = compiler.parser.previous.tokenType;
    let parseRule = compiler.getRule(operatorType);

    // parse rule here is binary


   compiler.parsePrecedence(parseRule.precedence.next());

    match operatorType {
        TokenType::PLUS => compiler.emitByte(OpCode::OP_ADD.to_u8()),
        TokenType::MINUS => compiler.emitByte(OpCode::OP_SUBTRACT.to_u8()),
        TokenType::STAR => compiler.emitByte(OpCode::OP_MULTIPLY.to_u8()),
        TokenType::SLASH => compiler.emitByte(OpCode::OP_DIVIDE.to_u8()),
        _ => ()
    };




}

fn number<'a>(compiler: &mut Compiler<'a>){
    //f32::from
    let value : Value = Value::number_value(f64::from_str(std::str::from_utf8(compiler.parser.previous.start).unwrap()).unwrap());
    let constantIndex = compiler.makeConstant(value);
    // write constant and constant index
    compiler.emitBytes(OP_CONSTANT.to_u8(),constantIndex);
}
pub fn compile(source: Vec<u8>, chunk: &mut Chunk) -> bool {
    ///advance();
//   expression();
//   consume(TOKEN_EOF, "Expect end of expression.");

    let mut scanner = Scanner::new(&source);

    true
}

