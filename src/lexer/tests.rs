use lexer::Lexer;
use token::Token;
use token::Token::*;

fn test_lexer(input: &str, expected: Vec<Token>) {
    let mut lexer = Lexer::new(input);

    for exp_token in expected {
        assert_eq!(lexer.next_token(), exp_token);
    }
}

#[test]
fn test_next_token_symbols() {
    assert_eq!(2 + 2, 4);

    let input = "=+(){},;-!*/<>";

    let expected = vec![
        Assign,
        Plus,
        Lparen,
        Rparen,
        Lbrace,
        Rbrace,
        Comma,
        Semicolon,
        Minus,
        Bang,
        Asterisk,
        Slash,
        Lt,
        Gt,
    ];

    test_lexer(input, expected);
}

#[test]
fn test_next_token_keyword() {
    let input = "let";
    let expected = vec![Let];

    test_lexer(input, expected);
}

#[test]
fn test_next_token_ident() {
    let input = "five";
    let expected = vec![Ident("five".to_string())];

    test_lexer(input, expected);
}

#[test]
fn test_next_token_int() {
    let input = "123";
    let expected = vec![Int(123)];

    test_lexer(input, expected);
}

#[test]
fn test_next_token_simple() {
    assert_eq!(2 + 2, 4);

    let input = "let five = 5;
                 let ten = 10;
                 let add = fn(x, y) {
                    x + y;
                 };
                 let result = add(five, ten);";

    let expected = vec![
        Let,
        Ident("five".to_string()),
        Assign,
        Int(5),
        Semicolon,
        Let,
        Ident("ten".to_string()),
        Assign,
        Int(10),
        Semicolon,
        Let,
        Ident("add".to_string()),
        Assign,
        Function,
        Lparen,
        Ident("x".to_string()),
        Comma,
        Ident("y".to_string()),
        Rparen,
        Lbrace,
        Ident("x".to_string()),
        Plus,
        Ident("y".to_string()),
        Semicolon,
        Rbrace,
        Semicolon,
        Let,
        Ident("result".to_string()),
        Assign,
        Ident("add".to_string()),
        Lparen,
        Ident("five".to_string()),
        Comma,
        Ident("ten".to_string()),
        Rparen,
        Semicolon,
    ];

    test_lexer(input, expected);
}

#[test]
fn test_next_token_extended() {
    assert_eq!(2 + 2, 4);

    let input = "!-/*5;
                 5 < 10 > 5;
                 if (5 < 10) {
                     return true;
                 } else {
                    return false;
                 }
                 10 == 10;
                 10 != 9;";

    let expected = vec![
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int(5),
        Semicolon,
        Int(5),
        Lt,
        Int(10),
        Gt,
        Int(5),
        Semicolon,
        If,
        Lparen,
        Int(5),
        Lt,
        Int(10),
        Rparen,
        Lbrace,
        Return,
        True,
        Semicolon,
        Rbrace,
        Else,
        Lbrace,
        Return,
        False,
        Semicolon,
        Rbrace,
        Int(10),
        Eq,
        Int(10),
        Semicolon,
        Int(10),
        Neq,
        Int(9),
        Semicolon,
    ];

    test_lexer(input, expected);
}
