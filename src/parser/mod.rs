use ast::*;
use token::Token;
use lexer::Lexer;
use std::collections::HashMap;

type ParseError = String;
type ParseErrors = Vec<ParseError>;

pub type ParseResult<T> = Result<T, ParseError>;

type PrefixFn = fn(parser: &mut Parser) -> ParseResult<Expression>;
type InfixFn = fn(parse: &mut Parser, left: Expression) -> ParseResult<Expression>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl Precedence {
    fn token_precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq => Precedence::Equals,
            Token::Neq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            Token::Lparen => Precedence::Call,
            Token::Lbracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

pub fn parse(input: &str) -> Result<Node, ParseErrors> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;

    Ok(Node::Program(Box::new(program)))
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer) -> Parser {
        let cur = lexer.next_token();
        let next = lexer.next_token();

        Parser { lexer: lexer, cur_token: cur, peek_token: next }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseErrors> {
        let mut program = Program::new();
        let mut errors = ParseErrors::new();
        let mut token = self.cur_token.clone();

        while token != Token::EOF {
            match self.parse_statement() {
                Ok(s) => program.statements.push(s),
                Err(e) => errors.push(e),
            }

            self.next_token();
            token = self.cur_token.clone();
        }

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match &self.cur_token.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(Box::new(ExpressionStatement { expression })))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let mut left_exp;

        if let Some(f) = self.prefix_fn() {
            left_exp = f(self)?;
        } else { 
            return Err(format!("No prefix parse function for {} found", self.cur_token));
        }

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            match self.infix_fn() {
                Some(f) => {
                    self.next_token();
                    left_exp = f(self, left_exp)?;
                }
                None => return Ok(left_exp),
            }
        }

        Ok(left_exp)
    }

    fn prefix_fn(&mut self) -> Option<PrefixFn> {
        match self.cur_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            Token::Str(_) => Some(Parser::parse_string_literal),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            Token::True | Token::False => Some(Parser::parse_boolean),
            Token::Lparen => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_literal),
            Token::Lbracket => Some(Parser::parse_array_literal),
            Token::Lbrace => Some(Parser::parse_hash_literal),
            _ => None,
        }
    }

    fn parse_hash_literal(parser: &mut Parser) -> ParseResult<Expression> {
        let mut pairs: HashMap<Expression, Expression> = HashMap::new();

        while !parser.peek_token_is(&Token::Rbrace) {
            parser.next_token();
            let key = parser.parse_expression(Precedence::Lowest)?;

            parser.expect_peek(Token::Colon)?;
            parser.next_token();
            
            let value = parser.parse_expression(Precedence::Lowest)?;

            pairs.insert(key, value);

            if !parser.peek_token_is(&Token::Rbrace) {
                parser.expect_peek(Token::Comma)?;
            }
        }

        parser.expect_peek(Token::Rbrace)?;

        Ok(Expression::Hash(Box::new(HashLiteral { pairs } )))
    }

    fn parse_array_literal(parser: &mut Parser) -> ParseResult<Expression> {
        let elements = parser.parse_expression_list(Token::Rbracket)?;
        Ok(Expression::Array(Box::new(ArrayLiteral { elements } )))
    }

    fn parse_expression_list(&mut self, end: Token) -> ParseResult<Vec<Expression>> {
        let mut list: Vec<Expression> = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return Ok(list)
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(list)
    }

    fn parse_prefix_expression(parser: &mut Parser) -> ParseResult<Expression> {
        let operator = parser.cur_token.clone();
        parser.next_token();

        let right = parser.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(Box::new(PrefixExpression { operator, right } )))
    }

    fn parse_if_expression(parser: &mut Parser) -> ParseResult<Expression> {
        parser.expect_peek(Token::Lparen)?;
        parser.next_token();

        let condition = parser.parse_expression(Precedence::Lowest)?;

        parser.expect_peek(Token::Rparen)?;
        parser.expect_peek(Token::Lbrace)?;

        let consequence = parser.parse_block_statement()?;

        let alternative = if parser.peek_token_is(&Token::Else) {
            parser.next_token();

            parser.expect_peek(Token::Lbrace)?;

            let alt_block = parser.parse_block_statement()?;
            Some(alt_block)
        } else {
            None
        };

        Ok(Expression::If(Box::new(IfExpression{ condition, consequence, alternative } )))
    }

    fn parse_block_statement(&mut self) -> ParseResult<BlockStatement> {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(Token::Rbrace) && !self.cur_token_is(Token::EOF) {
            if let Ok(s) = self.parse_statement() {
                statements.push(s);
            }

            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_function_literal(parser: &mut Parser) -> ParseResult<Expression> {
        parser.expect_peek(Token::Lparen)?;

        let parameters = parser.parse_function_parameters()?;

        parser.expect_peek(Token::Lbrace)?;

        let body = parser.parse_block_statement()?;

        Ok(Expression::Function(Box::new(FunctionLiteral { parameters, body })))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierExpression>, ParseError> {
        let mut identifiers: Vec<IdentifierExpression> = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Ok(identifiers)
        }

        self.next_token();

        identifiers.push(self.parse_identifier_into_identifier_expression()?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier_into_identifier_expression()?);
        }

        self.expect_peek(Token::Rparen)?;

        Ok(identifiers)
    }

    fn parse_grouped_expression(parser: &mut Parser) -> ParseResult<Expression> {
        parser.next_token();

        let exp = parser.parse_expression(Precedence::Lowest);

        parser.expect_peek(Token::Rparen)?;

        exp
    }

    fn infix_fn(&mut self) -> Option<InfixFn> {
        match self.peek_token {
            Token::Plus | Token::Minus | Token::Slash | Token::Asterisk | Token::Eq | Token::Neq | Token::Lt | Token::Gt => Some(Parser::parse_infix_expression),
            Token::Lparen => Some(Parser::parse_call_expression),
            Token::Lbracket => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn parse_index_expression(parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        parser.next_token();

        let exp = IndexExpression { left, index: parser.parse_expression(Precedence::Lowest)? };

        parser.expect_peek(Token::Rbracket)?;

        Ok(Expression::Index(Box::new(exp)))
    }

    fn parse_call_expression(parser: &mut Parser, function: Expression) -> ParseResult<Expression> {
        let arguments = parser.parse_expression_list(Token::Rparen)?;
        Ok(Expression::Call(Box::new(CallExpression { function, arguments })))
    }

    fn parse_infix_expression(parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        let operator = parser.cur_token.clone();
        let precedence = parser.cur_precedence();

        parser.next_token();

        let right = parser.parse_expression(precedence)?;

        Ok(Expression::Infix(Box::new(InfixExpression { operator, left, right })))
    }

    fn parse_boolean(parser: &mut Parser) -> ParseResult<Expression> {
        match parser.cur_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),

            // We should never hit this catchall
            _ => panic!("Couldn't parse {:?} to boolean", parser.cur_token),
        }
    }

    fn parse_identifier_into_identifier_expression(&mut self) -> ParseResult<IdentifierExpression> {
        if let Token::Ident(ref name) = self.cur_token {
            return Ok(IdentifierExpression { name: name.to_string() });
        }

        Err(format!("Unexpected error when parsing identifier {}", self.cur_token))
    }

    fn parse_identifier(parser: &mut Parser) -> ParseResult<Expression> {
        if let Token::Ident(ref name) = parser.cur_token {
            return Ok(Expression::Identifier(name.to_string()));
        }

        Err(format!("Unexpected error when parsing identifier {}", parser.cur_token))
    }

    fn parse_string_literal(parser: &mut Parser) -> ParseResult<Expression> {
        if let Token::Str(ref s) = parser.cur_token {
            return Ok(Expression::Str(s.to_string()));
        }

        Err(format!("Unexpected error when parsing string {}", parser.cur_token))
    }

    fn parse_integer_literal(parser: &mut Parser) -> ParseResult<Expression> {
        if let Token::Int(value) = parser.cur_token {
            return Ok(Expression::Integer(value));
        }

        Err(format!("Error when parsing integer literal {}", parser.cur_token))
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(Box::new(ReturnStatement { value })))
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let name = self.expect_ident()?;

        self.expect_peek(Token::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }


        Ok(Statement::Let(Box::new(LetStatement { name, value })))
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.cur_token)
    }

    fn cur_token_is(&self, token: Token) -> bool {
        match (&token, &self.cur_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            _ => token == self.cur_token,
        }
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match (&token, &self.peek_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            _ => token == &self.peek_token,
        }
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), ParseError> {
        match self.peek_token_is(&token) {
            true => {
                self.next_token();
                Ok(())
            }
            false => Err(format!("Expected next token to be {} but got {} instead", token, self.peek_token))
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        let name = match &self.peek_token {
            Token::Ident(name) => name.to_string(),
            _ => return Err(format!("Invalid identifier {}", self.peek_token)),
        };

        self.next_token();
        Ok(name)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
}

#[cfg(test)]
mod tests;