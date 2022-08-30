use std::str::FromStr;
use std::alloc;
use std::alloc::Layout;
use std::fmt::format;
use crate::scanner::*;
use crate::{Chunk, OpCode, Value, VM};
use crate::object::{Obj, ObjString};
use crate::OpCode::{OP_CONSTANT, OP_FALSE, OP_NIL, OP_POP, OP_PRINT, OP_RETURN, OP_TRUE};
use crate::scanner::TokenType::*;



#[derive(Copy,Clone,Debug,PartialOrd,PartialEq)]
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
    prefix: Option<fn(&mut Compiler,bool) -> ()>,
    infix: Option<fn(&mut Compiler,bool) -> ()>,
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

    fn rules() -> Vec<ParseRule> {
        // this just gives rules as to where a token type can appear, either as a prefix or as an infix,
        // example, this is invalid "/4" , but this is valid "-4", which explains why
        // slash '/' has no prefix rule because it cannot be used as a prefix, but minus '-' has because it can
       let mut rules =  vec![ParseRule::new(); TokenType::elements_len() as usize]; // create rules with default values
        rules[TokenType::LEFT_PAREN.as_usize()] =  createParseRule( Some(grouping),None,Precedence::NONE);
        rules[TokenType::MINUS.as_usize()] =  createParseRule( Some(unary),Some(binary),Precedence::TERM);
        rules[TokenType::PLUS.as_usize()] =  createParseRule( None,Some(binary),Precedence::TERM);
        rules[TokenType::SLASH.as_usize()] =  createParseRule( None,Some(binary),Precedence::FACTOR);
        rules[TokenType::STAR.as_usize()] =  createParseRule( None,Some(binary),Precedence::FACTOR);
        rules[TokenType::NUMBER.as_usize()] =  createParseRule( Some(number),None,Precedence::NONE);
        rules[TokenType::FALSE.as_usize()] =  createParseRule( Some(literal),None,Precedence::NONE);
        rules[TokenType::TRUE.as_usize()] =  createParseRule( Some(literal),None,Precedence::NONE);
        rules[TokenType::NIL.as_usize()] =  createParseRule( Some(literal),None,Precedence::NONE);
        rules[TokenType::BANG.as_usize()] =  createParseRule( Some(unary),None,Precedence::NONE);
        rules[TokenType::BANG_EQUAL.as_usize()] =  createParseRule( None,Some(binary),Precedence::EQUALITY);
        rules[TokenType::EQUAL_EQUAL.as_usize()] =  createParseRule( None,Some(binary),Precedence::EQUALITY);
        rules[TokenType::GREATER.as_usize()] =  createParseRule( None,Some(binary),Precedence::COMPARISON);
        rules[TokenType::GREATER_EQUAL.as_usize()] =  createParseRule( None,Some(binary),Precedence::COMPARISON);
        rules[TokenType::LESS.as_usize()] =  createParseRule( None,Some(binary),Precedence::COMPARISON);
        rules[TokenType::LESS_EQUAL.as_usize()] =  createParseRule( None,Some(binary),Precedence::COMPARISON);
        rules[TokenType::STRING.as_usize()] =  createParseRule( Some(string),None,Precedence::NONE);
        rules[TokenType::IDENTIFIER.as_usize()] =  createParseRule( Some(variable),None,Precedence::NONE);



        rules
    }


}

fn createParseRule(prefix : Option<fn(&mut Compiler,bool) -> ()>, infix :Option<fn(&mut Compiler,bool) -> ()>, precedence : Precedence ) -> ParseRule {
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
    parseRules : Vec<ParseRule>,
    vm: &'a mut VM
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
        while(!self.match_type(EOF)){
            self.declaration()
        }

        self.end();
        !self.parser.hadError
    }

    fn declaration(&mut self) {
        if(self.match_type(VAR)){
            self.varDeclaration()
        } else {
            self.statement()
        }
        if(self.parser.panicMode) {
            self.synchronize()
        }

    }

    fn varDeclaration(&mut self) {
        /**
         uint8_t global = parseVariable("Expect variable name.");

        if (match(TOKEN_EQUAL)) {
          expression();
        } else {
          emitByte(OP_NIL);
        }
        consume(TOKEN_SEMICOLON,
                "Expect ';' after variable declaration.");

        defineVariable(global);
         */

        // parses the variable name, stores in constant pool and retrieves the index
        let global_variable_index = self.parse_variable("Expect variable name.");
        if(self.match_type(EQUAL)){
            // var a = <expression>
            self.expression()
        } else {
            // var a
            self.emitOpcode(OP_NIL)
        }

        self.consume(SEMICOLON, "Expect ';' after variable declaration.");

        self.define_variable(global_variable_index);


    }

    fn parse_variable(&mut self,errorMsg : &str) -> u8{
        self.consume(IDENTIFIER, errorMsg);
        self.identifierConstant()

    }

    fn define_variable(&mut self, index: u8) {
        self.emitBytes(OpCode::OP_DEFINE_GLOBAL.to_u8(), index)

    }

    // makes an indentifier constant, using parser.previous.lexeme as the string
    fn identifierConstant(&mut self) -> u8 {
        let identifier = ObjString::from_str(self.parser.previous.lexeme());
        let interned_string = self.get_interned_string(identifier);
        self.makeConstant(Value::obj_value( Obj::STRING(interned_string)))
    }

    fn get_interned_string(&mut self, string : ObjString) -> ObjString {
        match self.vm.strings.get_key(&string) {
            None => {
                let cloned_string = string.clone();
                self.vm.strings.set(string, Value::nil_value());
                cloned_string
            }
            Some(internedString) => {
                // free allocated string, return clone instead
                string.free();
                internedString.clone()
            }
        }
    }

    fn synchronize(&mut self) {
        self.parser.panicMode = false;
        while(self.parser.current.tokenType != EOF) {
            if(self.parser.previous.tokenType == SEMICOLON) {
                return;
            }

            match self.parser.current.tokenType {
                CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return,
                _ => ()
            }

            self.advance();
        }

    }
    fn statement(&mut self) {
        if(self.match_type(PRINT)){
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn expression_statement(&mut self){
        self.expression();
        self.consume(SEMICOLON, "Expect ';' after expression.");
        self.emitOpcode(OP_POP);

    }
    fn match_type(&mut self, tokenType : TokenType) -> bool {
        if(!self.check_current_type(tokenType)) {
            false
        } else {
            self.advance();
            true
        }

    }

    pub fn named_variable(&mut self,canAssign :bool) {
        // uint8_t arg = identifierConstant(&name);
        //   emitBytes(OP_GET_GLOBAL, arg);
        let index = self.identifierConstant();

        if(canAssign && self.match_type(EQUAL)){
            // assignment of a variable
            //name = "samuel"
            self.expression(); // parse expression on the right hand
            self.emitBytes(OpCode::OP_SET_GLOBAL.to_u8(),index);


        } else {
            self.emitBytes(OpCode::OP_GET_GLOBAL.to_u8(), index);
        }



    }
    fn check_current_type(&mut self, tokenType : TokenType) -> bool {
        self.parser.current.tokenType == tokenType
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(SEMICOLON, "Expect ';' after value.");
        self.emitOpcode(OP_PRINT);
    }

    fn emitByte(&mut self,byte: u8) {
        self.chunk.write(byte,self.parser.previous.line)
    }

    fn emitOpcode(&mut self, opCode : OpCode) {
        self.chunk.write(opCode.to_u8(),self.parser.previous.line)
    }

    fn parsePrecedence(&mut self, precedence : Precedence) {
        self.advance();
        let token = self.parser.previous;
        let tokenType = token.tokenType;
        //println!("token  is {:?}, token type is {:?}, and precedence is {:?}", &token.lexeme(),&tokenType ,&precedence);
        let prefixRule  = self.getRule(tokenType).prefix;

        let canAssign = precedence <= Precedence::ASSIGNMENT;

        match prefixRule {

            None => {
                // we don't have a rule for the token type, its unexpected
                self.error("Expect expression.")
            },
            Some(rule) => rule(self,canAssign)
        }

       // println!("previous is {:?} and current is {:?}", &self.parser.previous.lexeme(), &self.parser.current);
        // at this point the token has been consumed, previous is like literal, current is operator (like +)

        while (precedence.to_u8() <= self.getRule(self.parser.current.tokenType).precedence.to_u8()) {
            // move token to operator
            self.advance();
            let infixRule = self.getRule(self.parser.previous.tokenType).infix;
            match infixRule {
                None => (),
                Some(rule) => rule(self,canAssign)
            }

            if(canAssign && self.match_type(EQUAL)){
                self.error("Invalid assignment target.");
            }

        }
    }
    pub fn end(&mut self) {
        #[cfg(feature = "debug_trace_execution")]
            {
                if(!self.parser.hadError){
                    self.chunk.disassemble("code");
                }
            }
        self.emitReturn()

    }
    fn grouping(&mut self, chunk : &mut Chunk) {
        self.expression();
        self.consume(RIGHT_PAREN, "Expect a ')' after a grouping")
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

    pub fn emitConstant(&mut self, byte : u8) {
        self.emitBytes(OP_CONSTANT.to_u8(),byte);
    }


    pub fn emitReturn(&mut self) {
        self.chunk.write(OP_RETURN.to_u8(),self.parser.previous.line)
    }

    pub fn expression(&mut self) {
        self.parsePrecedence(Precedence::ASSIGNMENT)
    }

    pub fn advance(&mut self) {

        self.parser.previous =  self.parser.current;
        // We keep looping, reading tokens and reporting the errors,
        // until we hit a non-error one or reach the end.
        loop {
            self.parser.current = self.scanner.scanTokens();
            if (self.parser.current.tokenType != TokenType::ERROR) {
                break;
            }
            self.errorAtCurrent(&format!("invalid token {:?}",&self.parser.current.lexeme()))
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
            _ => eprint!(" at '{}'", std::str::from_utf8(token.start).unwrap())
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
    }

    fn setScanner(&mut self) {
        self.scanner = Scanner::new(&self.source)
    }
    pub fn new(sourcer: &'a [u8], chunk : &'a mut Chunk, vm : &'a mut VM) -> Self {
        Self {
            source: sourcer,
            parser: Parser::new(),
            scanner: Scanner::empty(),
            chunk,
            parseRules : ParseRule::rules(), // store default on the compiler
            vm
        }
    }
}



fn grouping<'a>(compiler: &mut Compiler<'a>,canAssign : bool) {
    compiler.expression();
    compiler.consume(RIGHT_PAREN, "Expect a ')' after a grouping")
}

fn unary<'a>(compiler: &mut Compiler<'a>,canAssign : bool){
    let operatorType = compiler.parser.previous.tokenType;
    compiler.parsePrecedence(Precedence::UNARY);

    match operatorType {
        TokenType::MINUS => compiler.emitOpcode(OpCode::OP_NEGATE),
        TokenType::BANG => compiler.emitOpcode(OpCode::OP_NOT),
        _ => ()
    }
}

fn literal<'a>(compiler: &mut Compiler<'a>,canAssign : bool){
    // token has been consumed by parseprecedence
   match  compiler.parser.previous.tokenType {
       TokenType::TRUE => compiler.emitOpcode(OP_TRUE),
       TokenType::FALSE => compiler.emitOpcode(OP_FALSE),
       TokenType::NIL => compiler.emitOpcode(OP_NIL),
       _ => ()
   }
}

fn binary<'a>(compiler: &mut Compiler<'a>,canAssign : bool){
    let operatorType = compiler.parser.previous.tokenType;
    let parseRule = compiler.getRule(operatorType);

    // parse rule here is binary


   compiler.parsePrecedence(parseRule.precedence.next());

    match operatorType {
        TokenType::BANG_EQUAL => compiler.emitBytes(OpCode::OP_EQUAL.to_u8(),OpCode::OP_NOT.to_u8()), // a != b same as !(a==b)
        TokenType::EQUAL_EQUAL => compiler.emitOpcode(OpCode::OP_EQUAL),
        TokenType::GREATER => compiler.emitOpcode(OpCode::OP_GREATER),
        TokenType::GREATER_EQUAL => compiler.emitBytes(OpCode::OP_LESS.to_u8(),OpCode::OP_NOT.to_u8()), // a >= b same as !(a < b)
        TokenType::LESS => compiler.emitOpcode(OpCode::OP_LESS),
        TokenType::LESS_EQUAL => compiler.emitBytes(OpCode::OP_GREATER.to_u8(),OpCode::OP_NOT.to_u8()), // a <= b same as !(a > b)
        TokenType::PLUS => compiler.emitOpcode(OpCode::OP_ADD),
        TokenType::MINUS => compiler.emitOpcode(OpCode::OP_SUBTRACT),
        TokenType::STAR => compiler.emitOpcode(OpCode::OP_MULTIPLY),
        TokenType::SLASH => compiler.emitOpcode(OpCode::OP_DIVIDE),
        _ => ()
    };




}

/**
static void string() {
 emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                 parser.previous.length - 2)));
}*/

fn string<'a>(compiler: &mut Compiler<'a>,canAssign : bool) {

    let len = compiler.parser.previous.start.len();
    let string_bytes = &compiler.parser.previous.start[1.. len - 1];

    let string = ObjString::from_buffer(string_bytes);
    let interned_string = compiler.get_interned_string(string);
    let string = Value::obj_value(Obj::STRING(interned_string));

    // add constant to constant pool
    let index = compiler.makeConstant(string);

    // emit constant to chunk
    compiler.emitConstant(index)

  //  compiler.emitBytes()

}
fn variable<'a>(compiler: &mut Compiler<'a>,canAssign : bool) {
    compiler.named_variable(canAssign)
}

// fn copy_string(buffer : &[u8]) -> ObjString {
//     let len_of_string = buffer.len();
//     // allocate memory of that length
//     unsafe  {
//         let layout = Layout::array::<u8>(len_of_string).expect("cannot create layout for string");
//         let ptr = alloc::alloc(layout);
//         let mut ptr_offset : isize = 0;
//         for byte in buffer.iter() {
//             ptr.offset(ptr_offset).write(*byte);
//             ptr_offset+=1;
//         };
//         ObjString {
//             length : len_of_string,
//             ptr,
//             hash : 0
//         }
//     }
// }

// pub unsafe fn concat_strings(str1 : &[u8], str2 : &[u8]) -> ObjString {
//     unsafe {
//        let length =  str1.len() + str2.len();
//         let layout = Layout::array::<u8>(length).expect("cannot create layour for string");
//         let ptr = alloc::alloc(layout);
//         let mut ptr_offset = 0;
//
//         for i in str1 {
//             ptr.offset(ptr_offset).write(*i);
//             ptr_offset+=1
//         }
//         for i in str2 {
//             ptr.offset(ptr_offset).write(*i);
//             ptr_offset+=1
//         }
//
//         ObjString {
//             length,
//             ptr,
//             hash: 0
//         }
//
//     }
// }
fn number<'a>(compiler: &mut Compiler<'a>,canAssign : bool){
    //println!("{}",std::str::from_utf8(compiler.parser.previous.start).unwrap());
    let value : Value = Value::number_value(f64::from_str(compiler.parser.previous.lexeme()).unwrap());
    let constantIndex = compiler.makeConstant(value);
    // write constant and constant index
    compiler.emitConstant(constantIndex);
}
pub fn compile(source: Vec<u8>, chunk: &mut Chunk) -> bool {
    ///advance();
//   expression();
//   consume(TOKEN_EOF, "Expect end of expression.");

    let mut scanner = Scanner::new(&source);

    true
}

