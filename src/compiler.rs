use std::str::FromStr;
use std::alloc;
use std::alloc::Layout;
use std::fmt::format;
use crate::scanner::*;
use crate::{Chunk, ObjFunction, OpCode, Value, VM};
use crate::object::{Obj, ObjString};
use crate::OpCode::{OP_CONSTANT, OP_FALSE, OP_JUMP, OP_JUMP_IF_FALSE, OP_LOOP, OP_NIL, OP_POP, OP_PRINT, OP_RETURN, OP_TRUE};
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
        rules[TokenType::OR.as_usize()] =  createParseRule( None,Some(or),Precedence::NONE);
        rules[TokenType::AND.as_usize()] =  createParseRule( None,Some(and),Precedence::NONE);


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


 enum FunctionType {
    FUNCTION,
    SCRIPT
}

#[derive(Copy, Clone)]
struct CompilerState<'a >{
    locals:[Local<'a>; u8::MAX as usize],
    localCount: i32,
    scopeDepth: i32,
    functionChunkIndex: usize
}

impl<'a> CompilerState<'a> {
    fn new() -> Self {
        CompilerState {
            locals: [Local::empty(); u8::MAX as usize],
            localCount: 1,
            scopeDepth: 0,
            functionChunkIndex:0
        }

    }
}

pub struct Compiler<'a> {
    source: &'a [u8],
    parser: Parser<'a>,
    scanner: Scanner<'a>,
    //currentChunk: usize,
    currentFunction: ObjFunction,
    functionType: FunctionType,
    parseRules : Vec<ParseRule>,
    vm: &'a mut VM,
    state: [CompilerState<'a>; u8::MAX as usize],
    //currentState: usize,
   //previousState:usize,
    //nextAvailableStateIndex: usize,
    previousFunction: ObjFunction
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

#[derive(Copy, Clone,Debug)]
pub struct Local<'a> {
    name: Token<'a>,
    depth: i32
}

impl <'a> Local<'a> {
    pub fn empty() -> Self {
        Local {
            name: Token::empty(),
            depth:0
        }
    }

}


impl<'a> Compiler<'a> {
    pub fn compile(&mut self) -> Option<ObjFunction> {
        self.setScanner();
        self.advance();
        while(!self.match_type(EOF)){
            self.declaration()
        }

       let returnedFunc = self.end();
        if self.parser.hadError {
            None
        }else {
            Some(returnedFunc)
        }
    }

    fn declaration(&mut self) {
        if (self.match_type(FUN)) {
            self.fun_declaration();
        }
        else if(self.match_type(VAR)){
            self.varDeclaration()
        } else {
            self.statement()
        }
        if(self.parser.panicMode) {
            self.synchronize()
        }

    }

    fn fun_declaration(&mut self) {

        println!("previous is {:?}",self.parser.previous);
        let  (global, functionName) = self.parse_variable("Expect function name.",true);
        let functionName = functionName.expect("expect a variable name when parsing a variable in a function declaration");
        self.markInitialized();

        self.function(FunctionType::FUNCTION, functionName);

        self.define_variable(global);
    }


    fn function(&mut self, functionType : FunctionType, functionName : ObjString) {

        // Compiler compiler;
        // initCompiler(&compiler, type);
        // save previous state here before

        self.createFunctionState(functionName);

        self.beginScope();

        self.consume(LEFT_PAREN, "Expect '(' after function name.");
        self.consume(RIGHT_PAREN, "Expect ')' after parameters.");
        self.consume(LEFT_BRACE, "Expect '{' before function body.");


        self.block();

        let function = self.end();
        // println!("current chunk is {}",self.currentFunction.chunkIndex as usize);
        // self.vm.functionChunks[self.currentFunction.chunkIndex as usize].disassemble("test");


        let constantIndex = self.makeConstant(Value::obj_value(Obj::FUNCTION(function)));
        self.emitBytes(OP_CONSTANT.to_u8(),constantIndex );


    }
    
    fn createFunctionState(&mut self, functionName : ObjString) {
        let newChunk = Chunk::new();
        let chunkIndex = self.vm.functionChunks.len();
        //println!("chunk index {}",self.vm.functionChunks.len());
        //self.previousChunk= self.currentFunction.chunkIndex as usize;
        self.previousFunction = self.currentFunction;
        // self.nextAvailableStateIndex = self.currentState+1;
        // self.currentState = self.nextAvailableStateIndex;
        
//        self.currentFunction.chunkIndex as usize  = chunkIndex;
        self.vm.functionChunks.push(newChunk);

        self.currentFunction = ObjFunction {
            arity: 0,
            chunkIndex: chunkIndex as i32,
            name: functionName
        };

        
        

    }
    fn varDeclaration(&mut self) {
        // parses the variable name, stores in constant pool and retrieves the index
        // this will consume the variable name
        let variable_name_index = self.parse_variable("Expect variable name.",false).0;
        if(self.match_type(EQUAL)){
            // var a = <expression>
            self.expression()
        } else {
            // var a
            self.emitOpcode(OP_NIL)
        }

        self.consume(SEMICOLON, "Expect ';' after variable declaration.");

        //value is on the stack as done by the expression or Nil value above
        self.define_variable(variable_name_index);
    }

    fn parse_variable(&mut self,errorMsg : &str, returnVariableName : bool) -> (u8,Option<ObjString>){
        self.consume(IDENTIFIER, errorMsg);
        self.declareVariable();
       // println!("inside parse: {}, current state is {}", self.state[self.currentFunction.chunkIndex as usize].scopeDepth, self.currentState);
        if (self.state[self.currentFunction.chunkIndex as usize].scopeDepth > 0) {
            // we're inside a scoped block, no need to make identifier
            // There’s no need to stuff the variable’s name into the constant table,
            // so if the declaration is inside a local scope, we return a dummy table index instead.
            return (0,None);
        }
        self.identifierConstant(returnVariableName)

    }

    fn declareVariable(&mut self) {
        if(self.state[self.currentFunction.chunkIndex as usize].scopeDepth == 0) {
            // we're in global scope, return
            return
        } else {
            let token = self.parser.previous;
            let mut i = self.state[self.currentFunction.chunkIndex as usize].localCount - 1;
            while (i >= 0){
                let local = &self.state[self.currentFunction.chunkIndex as usize].locals[i as usize];
                if (local.depth != -1 && local.depth < self.state[self.currentFunction.chunkIndex as usize].scopeDepth){
                    break
                }
                if( local.name == token) {
                    self.error("Already a variable with this name in this scope.")
                }
                i-=1

            }
            self.addLocal(token)
        }
    }

    fn addLocal(&mut self, token : Token<'a>) {

        if (self.state[self.currentFunction.chunkIndex as usize].localCount == u8::MAX as i32) {
           self.error("Too many local variables in function.");
            return;
        }

        let local = &mut self.state[self.currentFunction.chunkIndex as usize].locals[self.state[self.currentFunction.chunkIndex as usize].localCount as usize];
        local.name = token;
        //local.depth = self.state[self.currentFunction.chunkIndex as usize].scopeDepth;
        local.depth = -1;
        self.state[self.currentFunction.chunkIndex as usize].localCount+=1


    }

    fn define_variable(&mut self, index: u8) {

        if (self.state[self.currentFunction.chunkIndex as usize].scopeDepth > 0) {
            // no need to globally define variable if we're in a scoped block
            self.markInitialized();
            return;
        }
        self.emitBytes(OpCode::OP_DEFINE_GLOBAL.to_u8(), index)

    }
    fn markInitialized(&mut self) {
        let currentScopeDepth = self.state[self.currentFunction.chunkIndex as usize].scopeDepth;
        let currentLocalCount = self.state[self.currentFunction.chunkIndex as usize].localCount;

        //println!("scope depth {}, localCount : {}, currentState is {}", &currentScopeDepth, currentLocalCount,self.currentState);

        if (self.state[self.currentFunction.chunkIndex as usize].scopeDepth == 0){
            return;
        }
        // when declaring the variable, we set the depth of the local to be -1,
        // here we set it to the right scope depth

        //self.locals[(self.localCount - 1) as usize].depth = self.scopeDepth;

       //  let currentScopeDepth = self.state[self.currentFunction.chunkIndex as usize].scopeDepth;
       //  let currentLocalCount = self.state[self.currentFunction.chunkIndex as usize].localCount;
       //
       //  println!("scope depth {}, localCount : {}, currentState is {}", &currentScopeDepth, currentLocalCount,self.currentState);
       // // self.state[self.currentFunction.chunkIndex as usize].locals[(currentLocalCount - 1) as usize].depth = currentScopeDepth;
        self.state[self.currentFunction.chunkIndex as usize].locals[(self.state[self.currentFunction.chunkIndex as usize].localCount - 1) as usize].depth = self.state[self.currentFunction.chunkIndex as usize].scopeDepth;

    }

    // makes an indentifier constant, using parser.previous.lexeme as the string
    fn identifierConstant(&mut self, returnIdentifier: bool) -> (u8,Option<ObjString>) {
        let identifier = ObjString::from_str(self.parser.previous.lexeme());
        let interned_string = self.get_interned_string(identifier);
        let constantIndex = self.makeConstant(Value::obj_value( Obj::STRING(interned_string)));
        if(returnIdentifier){
            (constantIndex,Some(interned_string.clone()))
        } else{
            (constantIndex,None)
        }
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
        }else if (self.match_type(WHILE)){
            self.while_statement()
        } else if (self.match_type(FOR)){
            self.for_statement()
        } else if (self.match_type(LEFT_BRACE)){
            self.beginScope();
            self.block();
            self.endScope();

        } else if (self.match_type(IF)) {

            self.consume(LEFT_PAREN, "Expect '(' after 'if'.");
            self.expression();
            self.consume(RIGHT_PAREN,  "Expect ')' after condition.");

            let thenJump = self.emitJump(OP_JUMP_IF_FALSE);
            // remove condition on stack, not needed
            self.emitOpcode(OP_POP);
            self.statement();

            // We add an else jump telling the vm to jump to where the else is
            /**
             1. In the case that the condition is true, the vm will run the then branch and then encounter the else jump, which is patched later
             2. In the case that the condition is false, the vm will skip over the then branch and hit the beginning of the else statement
             */
            let elseJump = self.emitJump(OP_JUMP);

            // jump from if_false to after op_jump
            self.patchJump(thenJump);

            // in the case that the condition is false
            // pop either way
            self.emitOpcode(OP_POP);

            if(self.match_type(ELSE)) {
                self.statement();
            }
            // see https://craftinginterpreters.com/jumping-back-and-forth.html#else-clauses

            // jump from end of if block to end of else block
            self.patchJump(elseJump)
        }else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) {
        self.beginScope();
        self.consume(LEFT_PAREN, "Expect '(' after 'for'.");
       // self.consume(SEMICOLON, "Expect ';'.");

        // initialization clause:

        if(self.match_type(SEMICOLON)){
            // no initializer
        } else if (self.match_type(VAR)){
            self.varDeclaration()
        } else {
            self.expression_statement() // consumes semicolon and pops value off stack
        }


        let mut loopStart = self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code.len();
        //self.consume(TOKEN_SEMICOLON, "Expect ';'.");

        let mut exitJump = -1;
        if(!self.match_type(SEMICOLON)){
            // Then there's a condition
            self.expression(); // put condition on stack
            self.consume(SEMICOLON, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exitJump = self.emitJump(OP_JUMP_IF_FALSE) as i32;
            self.emitOpcode(OP_POP); // pop Condition off stack.
        }

        if(!self.match_type(RIGHT_PAREN)){
            // first, we emit an unconditional jump that hops over the increment clause’s code to the body of the loop.
            let bodyJump = self.emitJump(OP_JUMP);
            let incrementStart = self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code.len();
            self.expression(); // put condition on stack
            self.emitOpcode(OP_POP); // pop of the value afterwards

            self.consume(RIGHT_PAREN, "Expect ')' after for clauses.");

            self.emitLoop(loopStart); // take us back to top of for loop condition

            loopStart = incrementStart;
            self.patchJump(bodyJump) // jump to beginning of body loop

        }

        self.statement();
        self.emitLoop(loopStart); // after body, loop to increment, increment will loop back to condition
        if (exitJump != -1) {
            // if there's a condition, set exit jump
            self.patchJump(exitJump as u32);
            self.emitOpcode(OP_POP); // Condition.
        }
        // condition -> increment -> (body -> increment -> condition)*
        self.endScope()
    }
    fn while_statement(&mut self) {
        let mut loopStart = self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code.len();
        self.consume(LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.consume(RIGHT_PAREN, "Expect ')' after condition.");

        let exitJump = self.emitJump(OP_JUMP_IF_FALSE);
        self.emitOpcode(OP_POP);
        self.statement();

        self.emitLoop(loopStart);


        self.patchJump(exitJump);
        self.emitOpcode(OP_POP);
    }

    fn emitLoop(&mut self, loopStart : usize) {
        // It’s a bit like emitJump() and patchJump() combined.
        // It emits a new loop instruction, which unconditionally jumps backwards by a given offset.
        self.emitOpcode(OP_LOOP);

        let offset = self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code.len() - loopStart + 2;

        if(offset as u16 > u16::MAX) {
            self.error("Loop Body too large");
        }

        self.emitByte((offset >> 8) as u8);
        self.emitByte(offset as u8);


    }

    fn emitJump(&mut self, opCode : OpCode) -> u32 {

        self.emitOpcode(opCode);
        // We use two bytes for the jump offset operand.
        // A 16-bit offset lets us jump over up to 65,535 bytes of code, which should be plenty for our needs.
        self.emitByte(u8::MAX);
        self.emitByte(u8::MAX);

        (self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code.len() - 2) as u32
    }

    fn patchJump(&mut self, offset : u32) {
        // offset here is the index of the last byte that was handled
        // before jumping


        //println!("chunk length is {}",self.chunk.code.len());
        // this says jump `jump` bits over to the next point of execution

        // jump is how far ahead in the stack we jump ahead to.
        /**
         E.g, if the then branch of the if statement results in 400 instructions on the stack,
        then jump will be 400. The next challenge is how to encode the result 400 in our byte code,
        our stack is just a vec of bytes, in emit jump, we used two slots (16 bit) to store the value of this jump
        which shouldn't be more than u16::MAX. Now, how do we encode a 16 bit value in 2 (8 bit values)
         */
        let jump = self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code.len() as u32 - offset - 2;

        //println!("jump is {}, offset is {}",&jump,&offset);
        if(jump as u16 > u16::MAX){
            self.error("Too much code to jump over.")
        }


        /**
         Turns out the trick is quite simple, taking our example from above, let's say the jump is 400, we want to encode it.
        400 in binary is 0000000110010000 (16 bit).
        first thing, we shift the jump 8 bits to the right, to kinda eliminate the 2nd byte (8 bit) and have only the first byte,
        and then multiply (and) the result with 0xff (0000000011111111) which just makes every value outside the right most 8 bits 0 (thus eliminating overflows )
        1. Doing that (400 >> 8) & 0xff  will give us 0000000000000001 .. which is just 1 in decimal,
        2. Mutliply jump by 0xff to eliminate leftmost 8 bits , (400 & 0xff) which is 0000000010010000 ehich is 144 in decimal

        Now, we've encoded 400 into two numbers 1 and 144 , which when converted to binary, merged and then converted to decimal will give 400

        To decode this, we just do the inverse of what we did above,
        1. we get what's supposed to be the left most byte (8 bit) and shift it to the left first.
            1 << 8 gives 0000000100000000
        2. we take what's supposed to be the 2nd byte (144) and that in binary is 0000000010010000,
        now we do an or on them to pretty much merge them
        and that gives 0000000110010000, which was our number in the first place
         */


        // tl:dr , this encodes the jump in 16 bit binary into two 8 bit binary
        let first_half_of_16_bit_jump = ((jump >> 8) & 0xff) as u8;
        let second_half_of_16_bit_jump = (jump & 0xff) as u8;

        // another way is just to shift (if needed) and cast to u8
        // let first_half_of_16_bit_jump = (jump >> 8) as u8;
        // let second_half_of_16_bit_jump = jump as u8;

        // seee vm (fn read_16_bit_short) for how we decode
         self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code[offset as usize] = first_half_of_16_bit_jump;
         self.vm.functionChunks[self.currentFunction.chunkIndex as usize].code[(offset + 1) as usize] = second_half_of_16_bit_jump;

    }

    fn block(&mut self) {
        while (!self.check_current_type(RIGHT_BRACE) && !self.check_current_type(EOF)) {
            self.declaration();
        }
        self.consume(RIGHT_BRACE, "Expect '}' after block.");
    }

    fn beginScope(&mut self){
        self.state[self.currentFunction.chunkIndex as usize].scopeDepth+=1
    }
    fn endScope(&mut self){
        self.state[self.currentFunction.chunkIndex as usize].scopeDepth-=1;
        while(self.state[self.currentFunction.chunkIndex as usize].localCount > 0 && self.state[self.currentFunction.chunkIndex as usize].locals[(self.state[self.currentFunction.chunkIndex as usize].localCount - 1) as usize].depth > self.state[self.currentFunction.chunkIndex as usize].scopeDepth) {
            // we remove all local variables at the scope depth we just left
            // so for scope depth 2, we remove al loca variables with scope depth > 2
            self.emitOpcode(OpCode::OP_POP);
            self.state[self.currentFunction.chunkIndex as usize].localCount-=1

        }
    }

    fn expression_statement(&mut self){
        self.expression();
        self.consume(SEMICOLON, "Expect ';' after expression.");
        // Semantically, an expression statement evaluates the expression and discards the result
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

    fn resolveLocal(&mut self, token : Token<'a>) -> i32 {
        let mut i = self.state[self.currentFunction.chunkIndex as usize].localCount - 1;

        while(i >= 0) {
            // walk backwards from the local stack and if any local matches the token we're looking for
            // we return the index
            let local = &self.state[self.currentFunction.chunkIndex as usize].locals[i as usize];
            if (local.name == token){
                if (local.depth == -1) {
                    self.error("Can't read local variable in its own initializer.");
                }
                return  i
            }
            i-=1
        }

        return -1

    }
    pub fn named_variable(&mut self,canAssign :bool) {

        let mut getOp : OpCode = OpCode::OP_NIL;
        let mut setOp : OpCode = OpCode::OP_NIL;

        let mut arg = self.resolveLocal(self.parser.previous);
        if(arg != -1){
            getOp = OpCode::OP_GET_LOCAL;
            setOp= OpCode::OP_SET_LOCAL;
        } else {
            // this wil take the consumed identifier, stash it in the constant table and return the index
            // where this was saved
            arg = (self.identifierConstant(false)).0 as i32;
            getOp = OpCode::OP_GET_GLOBAL;
            setOp= OpCode::OP_SET_GLOBAL;
        }



        let index = arg as u8;
        if(canAssign && self.match_type(EQUAL)){
            // assignment of a variable
            //name = "samuel"
            self.expression(); // parse expression on the right hand and place on stack
            self.emitBytes(setOp.to_u8(),index);


        } else {
            self.emitBytes(getOp.to_u8(), index);
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
        self.vm.functionChunks[self.currentFunction.chunkIndex as usize].write(byte, self.parser.previous.line)
    }

    fn emitOpcode(&mut self, opCode : OpCode) {
        self.vm.functionChunks[self.currentFunction.chunkIndex as usize].write(opCode.to_u8(), self.parser.previous.line)
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
    pub fn end(&mut self) -> ObjFunction {
        #[cfg(feature = "debug_trace_execution")]
            {
                // for (i, code) in self.chunk.code.iter().enumerate() {
                //     let opcodeOpt = OpCode::try_from(code);
                //     println!("index: {}, code: {:?}", i, opcodeOpt)
                // }
                if(!self.parser.hadError){
                    //println!("current function {:?}",&self.currentFunction);
                    let funcName = if self.currentFunction.name.is_empty() {
                        "<script>"
                    } else {
                        self.currentFunction.name.as_str()
                    };
                   self.vm.functionChunks[self.currentFunction.chunkIndex as usize].disassemble(funcName);
                }
            }
        self.emitReturn();

        //self.currentState = self.previousState;

        // if(self.currentState > 1) {
        //     self.currentState -= 1;
        // }
       let toBeReturned =  self.currentFunction;
       // self.currentFunction.chunkIndex as usize = self.previousFunction.chunkIndex as usize;
        self.currentFunction = self.previousFunction;
//        self.currentFunction.chunkIndex as usize = self.previousFunction.chunkIndex as usize;

        toBeReturned

    }
    fn grouping(&mut self, chunk : &mut Chunk) {
        self.expression();
        self.consume(RIGHT_PAREN, "Expect a ')' after a grouping")
    }

    pub fn makeConstant(&mut self, value : Value) -> u8 {
        let constantIndex =  self.vm.functionChunks[self.currentFunction.chunkIndex as usize].addConstant(value);
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
        self.vm.functionChunks[self.currentFunction.chunkIndex as usize].write(OP_RETURN.to_u8(), self.parser.previous.line)
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
    pub fn new(sourcer: &'a [u8], vm : &'a mut VM) -> Self {

        // Local* local = &current->locals[current->localCount++];
        //   local->depth = 0;
        //   local->name.start = "";
        //   local->name.length = 0;

        // let localCount = 0;
        // let mut locals = [Local::empty(); u8::MAX as usize];
        // let local = &mut locals[localCount];
        //
        // local.depth = 0;
        // local.

        let mut compiler = Self {
            source: sourcer,
            parser: Parser::new(),
            scanner: Scanner::empty(),
            //currentChunk: 0,
            currentFunction: ObjFunction::new(),
            functionType: FunctionType::SCRIPT,
            parseRules : ParseRule::rules(), // store default on the compiler
            vm,
            state: [CompilerState::new(); u8::MAX as usize],
//            currentState: 0,
            //previousState:0,
            //nextAvailableStateIndex:0,
            previousFunction: ObjFunction::new()
        };
        
        let initState = &mut compiler.state[0];
        initState.localCount = 1;
        compiler.currentFunction.chunkIndex = 0;
        
        compiler
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
   // println!("calling named variable");
    compiler.named_variable(canAssign)
}

fn number<'a>(compiler: &mut Compiler<'a>,canAssign : bool){
    //println!("{}",std::str::from_utf8(compiler.parser.previous.start).unwrap());
    let value : Value = Value::number_value(f64::from_str(compiler.parser.previous.lexeme()).unwrap());
    let constantIndex = compiler.makeConstant(value);
    // write constant and constant index
    compiler.emitConstant(constantIndex);
}

fn or<'a>(compiler: &mut Compiler<'a>,canAssign : bool){

    // left hand is already on stack:
    // if LHS is false, we fall through to OP_POP, which pops the LHS and falls to parse the rhs
    //  if true we can ignore right hand side and jump to after precedence:or expression due to end jump

    let elseJump = compiler.emitJump(OP_JUMP_IF_FALSE);
    let endJump = compiler.emitJump(OP_JUMP);
    compiler.patchJump(elseJump);
    compiler.emitOpcode(OP_POP);

    compiler.parsePrecedence(Precedence::OR);
    compiler.patchJump(endJump);

}

fn and<'a>(compiler: &mut Compiler<'a>,canAssign : bool){
    //At the point this is called, the left-hand side expression has already been compiled. That means at runtime, i
    // ts value will be on top of the stack. If that value is falsey, then we know the entire and must be false,
    // so we skip the right operand and leave the left-hand side value as the result of the entire expression.
    // Otherwise, we discard the left-hand value and evaluate the right operand which becomes the result of the whole and expression.
    let endJump = compiler.emitJump(OP_JUMP_IF_FALSE);
    compiler.emitOpcode(OP_POP);
    compiler.parsePrecedence(Precedence::AND);

    compiler.patchJump(endJump);
}
pub fn compile(source: Vec<u8>, chunk: &mut Chunk) -> bool {
    ///advance();
//   expression();
//   consume(TOKEN_EOF, "Expect end of expression.");

    let mut scanner = Scanner::new(&source);

    true
}

