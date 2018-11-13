use token::*;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        Lexer { input: input.chars().peekable() }
    }

    pub fn next_token(&mut self) -> Token {
        self.trim();

        match self.read_char() {
            Some('=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            },
            Some('+') => Token::Plus,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            Some('[') => Token::Lbracket,
            Some(']') => Token::Rbracket,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some(':') => Token::Colon,
            Some('-') => Token::Minus,
            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            },
            Some('*') => Token::Asterisk,
            Some('/') => Token::Slash,
            Some('<') => Token::Lt,
            Some('>') => Token::Gt,
            Some(ch) => {
                if is_letter(ch) {
                    let ident = self.read_identifier(ch);
                    let token = lookup_ident(ident);
                    token
                } else if ch.is_digit(10) {
                    Token::Int(self.read_int(ch))
                } else if ch == '"' {
                    Token::Str(self.read_string())
                } else {
                    Token::Illegal
                }
            },
            None => Token::EOF,
        }
    }

    fn read_string(&mut self) -> String {
        let mut str = String::new();
        while let Some(ch) = self.read_char() {
            if ch == '"' {
                return str;
            }
            str.push(ch);
        }
        str
    }

    fn trim(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn read_int(&mut self, ch: char) -> i64 {
        let mut s = String::new();
        s.push(ch);

        while let Some(&ch) = self.peek_char() {
            if ch.is_digit(10) {
                s.push(self.read_char().unwrap());
            } else {
                break;
            }
        }
        s.parse().unwrap()
    }

    fn read_identifier(&mut self, ch: char) -> String {
        let mut ident = String::new();
        ident.push(ch);

        while let Some(&ch) = self.peek_char() {
            if is_letter(ch) {
                ident.push(self.read_char().unwrap());
            } else {
                break;
            }
        }
        ident
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token == Token::EOF {
            None
        } else {
            Some(token)
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

#[cfg(test)]
mod tests;