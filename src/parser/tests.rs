use parser::*;
use lexer::Lexer;

fn let_statement_parse_and_verify<'a>(program: &'a Program, expected_ident: &str) -> &'a Expression {
    let statement = program.statements.first().unwrap();

    match statement {
        Statement::Let(s) => {
            assert_eq!(s.name.as_str(), expected_ident);
            return &s.value;
        },
        s => panic!("Expected let statement but got {:?} instead", s)
    }
}

fn return_statement_parse_and_verify<'a>(program: &'a Program) -> &'a Expression {
    let statement = program.statements.first().unwrap();

    match statement {
        Statement::Return(statement) => {
            return &statement.value
        },
        statement => panic!("Expected return statement but got {:?} instead", statement)
    }
}

fn test_infix(exp: &Expression, left: i64, op: Token, right: i64) {
    match exp {
        Expression::Infix(infix) => {
            assert_eq!(op, infix.operator, "Expected {} operator but got {}", op, infix.operator);
            test_integer_literal(&infix.left, left);
            test_integer_literal(&infix.right, right);
        }
        exp => panic!("Expected prefix expression bu got {:?}", exp)
    }
}

fn test_if_condition(exp: &Expression, operator: Token, left: &str, right: &str) {
    match exp {
        Expression::Infix(infix) => {
            test_identifier(&infix.left, left);
            test_identifier(&infix.right, right);

            if operator != infix.operator {
                panic!("Expected {} operator but got {} instead", operator, infix.operator)
            }
        }
        _ => panic!("Expected infix expression but got {:?} instead", exp)
    }
}

fn setup(input: &str, statement_count: usize) -> Program {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();

    if statement_count != 0 && program.statements.len() != statement_count {
        panic!("Expected 1 statement for '{}' but got {:?} instead", input, program.statements)
    }

    program
}

fn unwrap_expression(program: &Program) -> &Expression {
    match program.statements.first().unwrap() {
        Statement::Expression(s) => &s.expression,
        s => panic!("{:?} isn't an expression statement", s)
    }
}

fn test_integer_literal(exp: &Expression, value: i64) {
    match exp {
        Expression::Integer(int) => assert_eq!(value, *int, "Expected {} but got {} instead", value, int),
        _ => panic!("Expected integer literal {} but got {} instead", value, exp)
    }
}

fn test_boolean_literal(exp: &Expression, value: bool) {
    match exp {
        Expression::Boolean(val) => assert_eq!(value, *val, "Expected {} but got {} instead", value, val),
        _ => panic!("Expected boolean literal {} but got {:?} instead", value, exp)
    }
}

fn test_identifier(exp: &Expression, value: &str) {
    match exp {
        Expression::Identifier(ident) => assert_eq!(value, ident, "Expected {} but got {} instead", value, ident),
        _ => panic!("Expected identifier expression but got {:?}", exp)
    }
}

#[test]
fn let_statement() {
    let input = "\
        let x = 5;
        let y = 10;
        let foobar = 838383;";

    let program = setup(input, 3);

    let tests = vec![
        "x",
        "y",
        "foobar",
    ];

    let mut itr = program.statements.iter();

    for t in tests {
        match itr.next().unwrap() {
            Statement::Let(ref l) => {
                assert_eq!(l.name, t);
            },
            _ => panic!("Unknown node")
        }
    }
}

#[test]
fn let_statement_bool() {
    let program = setup("let y = true;", 1);
    let exp = let_statement_parse_and_verify(&program, "y");

    match exp {
        Expression::Boolean(b) => assert_eq!(*b, true),
        _ => panic!("Expected boolean expression")
    }
}

#[test]
fn let_statement_ident() {
    let program = setup("let foobar = y;", 1);
    let exp = let_statement_parse_and_verify(&program, "foobar");

    match exp {
        Expression::Identifier(_) => test_identifier(&exp, "y"),
        _ => panic!("Expected identifier expression")
    }
}

#[test]
fn let_statement_error() {
    let input = "\
        let x 5;
        let = 10;
        let y = 23;
        let 23456";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(_) => panic!("Should have returned a parse failure"),
        Err(errors) => {
            if errors.len() != 4 {
                panic!("Got {} errors instead of 3\n{:?}", errors.len(), errors)
            }

            let expected_errors = vec![
                "Expected next token to be = but got 5 instead",
                "Invalid identifier =",
                "No prefix parse function for = found",
                "Invalid identifier 23456"
            ];

            let mut itr = errors.iter();

            for err in expected_errors {
                let message = itr.next().unwrap();
                assert_eq!(message, err, "Expected error '{}' but got '{}' instead", err, message)
            }
        }
    }
}

#[test]
fn return_statement() {
    let input = "\
        return 5;
        return 10;
        return 98765;";

    let program = setup(input, 3);

    for s in program.statements {
        match s {
            Statement::Return(_) => {}
            _ => panic!("Statement {:?} isn't a valid return statement", s)
        }
    }
}

#[test]
fn return_statement_bool() {
    let program = setup("return true;", 1);
    let exp = return_statement_parse_and_verify(&program);

    match exp {
        Expression::Boolean(b) => assert_eq!(*b, true),
        _ => panic!("Expected boolean expression")
    }
}

#[test]
fn return_statement_ident() {
    let program = setup("return foobar;", 1);
    let exp = return_statement_parse_and_verify(&program);

    match exp {
        Expression::Identifier(_) => test_identifier(&exp, "foobar"),
        _ => panic!("Expected identifier expression")
    }
}

#[test]
fn identifier_expression() {
    let input = "foobar;";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    test_identifier(exp, "foobar");
}

#[test]
fn integer_literal() {
    let input = "5";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Integer(int) => assert_eq!(*int, 5, "Expected value to be 5 but got {} instead", int),
        exp => panic!("Expected integer literal expression but got {:?} instead", exp)
    }
}

#[test]
fn prefix_expressions() {
    struct Test<'a> {
        input: &'a str,
        operator: Token,
        value: i64,
    }
    
    let tests = vec![
        Test { input: "!5", operator: Token::Bang, value: 5 },
        Test { input: "-15", operator: Token::Minus, value: 15 },
    ];

    for test in tests {
        let program = setup(test.input, 1);
        let exp = unwrap_expression(&program);

        match exp {
            Expression::Prefix(prefix) => {
                assert_eq!(test.operator, prefix.operator, "Expected {} operator but got {} instead", test.operator, prefix.operator);
                test_integer_literal(&prefix.right, test.value);
            }
            exp => panic!("Expected prefix expression but got {:?} instead", exp)
        }
    }
}

fn infix_expressions() {
    struct Test<'a> {
        input: &'a str,
        left_value: i64,
        operator: Token,
        right_value: i64,
    }

    let tests = vec![
        Test { input: "5 + 5;", left_value: 5, operator: Token::Plus, right_value: 5 },
        Test { input: "5 - 5;", left_value: 5, operator: Token::Minus, right_value: 5 },
        Test { input: "5 * 5;", left_value: 5, operator: Token::Asterisk, right_value: 5 },
        Test { input: "5 / 5;", left_value: 5, operator: Token::Slash, right_value: 5 },
        Test { input: "5 > 5;", left_value: 5, operator: Token::Gt, right_value: 5 },
        Test { input: "5 < 5;", left_value: 5, operator: Token::Lt, right_value: 5 },
        Test { input: "5 == 5", left_value: 5, operator: Token::Eq, right_value: 5 },
        Test { input: "5 != 5;", left_value: 5, operator: Token::Neq, right_value: 5 },
    ];

    for test in tests {
        let program = setup(test.input, 1);
        let exp = unwrap_expression(&program);

        match exp {
            Expression::Infix(infix) => {
                assert_eq!(test.operator, infix.operator, "Expected {} operator but got {} instead", test.operator, infix.operator);
                test_integer_literal(&infix.left, test.left_value);
                test_integer_literal(&infix.right, test.right_value);
            }
            exp => panic!("Expected prefix expression but got {:?} instead", exp)
        }
    }
}

#[test]
fn infix_boolean_literal_expressions() {
    struct Test<'a> {
        input: &'a str,
        left_value: bool,
        operator: Token,
        right_value: bool,
    }

    let tests = vec![
        Test { input: "true == true", left_value: true, operator: Token::Eq, right_value: true },
        Test { input: "true != false", left_value: true, operator: Token::Neq, right_value: false },
        Test { input: "false == false", left_value: false, operator: Token::Eq, right_value: false },
    ];

    for test in tests {
        let program = setup(test.input, 1);
        let exp = unwrap_expression(&program);

        match exp {
            Expression::Infix(infix) => {
                assert_eq!(test.operator, infix.operator, "Expected {} operator but got {} instead", test.operator, infix.operator);
                test_boolean_literal(&infix.left, test.left_value);
                test_boolean_literal(&infix.right, test.right_value);
            }
            exp => panic!("Expected infix expression but got {:?} instead", exp)
        }
    }
}

#[test]
fn operator_precedence() {
    struct Test<'a> {
        input: &'a str,
        expected: &'a str,
    }

    let tests = vec![
        Test { input: "-a * b", expected: "((-a) * b)" },
        Test { input: "!-a", expected: "(!(-a))" },
        Test { input: "a + b + c", expected: "((a + b) + c)" },
        Test { input: "a + b - c", expected: "((a + b) - c)" },
        Test { input: "a * b * c", expected: "((a * b) * c)" },
        Test { input: "a * b / c", expected: "((a * b) / c)" },
        Test { input: "a + b / c", expected: "(a + (b / c))" },
        Test { input: "a + b * c + d / e - f", expected: "(((a + (b * c)) + (d / e)) - f)" },
        Test { input: "3 + 4; -5 * 5", expected: "(3 + 4)((-5) * 5)" },
        Test { input: "5 > 4 == 3 < 4", expected: "((5 > 4) == (3 < 4))" },
        Test { input: "5 < 4 != 3 > 4", expected: "((5 < 4) != (3 > 4))" },
        Test { input: "3 + 4 * 5 == 3 * 1 + 4 * 5", expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        Test { input: "true", expected: "true" },
        Test { input: "false", expected: "false" },
        Test { input: "3 > 5 == false", expected: "((3 > 5) == false)" },
        Test { input: "3 < 5 == true", expected: "((3 < 5) == true)" },
        Test { input: "1 + (2 + 3) + 4", expected: "((1 + (2 + 3)) + 4)" },
        Test { input: "(5 + 5) * 2", expected: "((5 + 5) * 2)" },
        Test { input: "2 / (5 + 5)", expected: "(2 / (5 + 5))" },
        Test { input: "-(5 + 5)", expected: "(-(5 + 5))" },
        Test { input: "!(true == true)", expected: "(!(true == true))" },
        Test { input: "a + add(b * c) + d", expected: "((a + add((b * c))) + d)" },
        Test { input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        Test { input: "add(a + b + c * d / f + g)", expected: "add((((a + b) + ((c * d) / f)) + g))" },
        Test { input: "a * [1, 2, 3, 4][b * c] * d", expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)" },
        Test { input: "add(a * b[2], b[1], 2 * [1, 2][1])", expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))" },
    ];

    for test in tests {
        let program = setup(test.input, 0).to_string();
        assert_eq!(test.expected, program, "Expected '{}' but got '{}' instead", test.expected, program)
    }
}

#[test]
fn boolean_expression() {
    struct Test<'a> {
        input: &'a str,
        expected: bool,
    }

    let tests = vec![
        Test { input: "true;", expected: true },
        Test { input: "false;", expected: false },
    ];

    for test in tests {
        let program = setup(test.input, 1);
        let exp = unwrap_expression(&program);
        test_boolean_literal(&exp, test.expected);
    }
}

#[test]
fn if_expression() {
    let input = "if (x < y) { x }";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::If(i) => {
            test_if_condition(&i.condition, Token::Lt, "x", "y");
            assert_eq!(i.consequence.statements.len(), 1, "Expected only 1 statement");

            match i.consequence.statements.first().unwrap() {
                Statement::Expression(s) => test_identifier(&s.expression, "x"),
                s => panic!("Expected expression statement but got {:?} instead", s)
            }

            if let Some(s) = &i.alternative {
                panic!("Expected alternative to be None but got {:?} instead", s)
            }
        }
        _ => panic!("Expected if expression but got {:?} instead", exp)
    }
}

#[test]
fn if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::If(i) => {
            test_if_condition(&i.condition, Token::Lt, "x", "y");
            assert_eq!(i.consequence.statements.len(), 1);

            match &i.consequence.statements.first().unwrap() {
                Statement::Expression(s) => test_identifier(&s.expression, "x"),
                s => panic!("Expected expression statement but got {:?} instead", s)
            }

            if let Some(s) = &i.alternative {
                assert_eq!(s.statements.len(), 1);

                match s.statements.first().unwrap() {
                    Statement::Expression(s) => test_identifier(&s.expression, "y"),
                    s => panic!("Expected expression statement but got {:?} instead", s)
                }
            } else {
                panic!("Expected alternative block")
            }
        }
        _ => panic!("Expected if expression but got {:?} instead", exp)
    }
}

#[test]
fn function_literal() {
    let input = "fn(x, y) { x + y; }";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Function(func) => {
            assert_eq!(2, func.parameters.len(), "Expected 2 parameters but got {:?} instead", func.parameters);
            assert_eq!(func.parameters.first().unwrap().name, "x");
            assert_eq!(func.parameters.last().unwrap().name, "y");
            assert_eq!(1, func.body.statements.len(), "Expected 1 body statement but got {:?} instead", func.body.statements);

            match func.body.statements.first().unwrap() {
                Statement::Expression(s) => match &s.expression {
                    Expression::Infix(infix) => {
                        assert_eq!(infix.operator, Token::Plus, "Expected + but got {} instead", infix.operator);
                        test_identifier(&infix.left, "x");
                        test_identifier(&infix.right, "y");
                    },
                    _ => panic!("Expected infix expression but got {:?} instead", s.expression)
                },
                s => panic!("Expected expression statement but got {:?} instead", s)
            }
        },
        _ => panic!("{} is not a function literal", exp)
    }
}

fn function_parameters() {
    struct Test<'a> {
        input: &'a str,
        expected_params: Vec<&'a str>,
    }

    let tests = vec![
        Test { input: "fn() {};", expected_params: vec![] },
        Test { input: "fn(x) {};", expected_params: vec!["x"] },
        Test { input: "fn(x, y, z) {};", expected_params: vec!["x", "y", "z"] },
    ];

    for test in tests {
        let program = setup(test.input, 1);
        let exp = unwrap_expression(&program);

        match exp {
            Expression::Function(func) => {
                assert_eq!(func.parameters.len(), test.expected_params.len());

                let mut params = test.expected_params.into_iter();

                for param in &func.parameters {
                    let expected_param = params.next().unwrap();
                    assert_eq!(expected_param, param.name.as_str());
                }
            },
            _ => panic!("{:?} is not a function literal", exp)
        }
    }
}

#[test]
fn call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Call(call) => {
            test_identifier(&call.function, "add");
            assert_eq!(call.arguments.len(), 3);

            let mut args = (&call.arguments).into_iter();
            test_integer_literal(&args.next().unwrap(), 1);
            test_infix(&args.next().unwrap(), 2, Token::Asterisk, 3);
            test_infix(&args.next().unwrap(), 4, Token::Plus, 5)
        },
        _ => panic!("{} is not a call expression", exp)
    }
}

#[test]
fn call_expression_parameter_parsing() {
    struct Test<'a> {
        input: &'a str,
        expected_ident: &'a str,
        expected_args: Vec<&'a str>,
    }

    let tests = vec![
        Test { input: "add();", expected_ident: "add", expected_args: vec![] },
        Test { input: "add(1);", expected_ident: "add", expected_args: vec!["1"] },
        Test { input: "add(1, 2 * 3, 4 + 5);", expected_ident: "add", expected_args: vec!["1", "(2 * 3)", "(4 + 5)"] },
    ];

    for test in tests {
        let program = setup(test.input, 1);
        let exp = unwrap_expression(&program);

        match exp {
            Expression::Call(call) => {
                test_identifier(&call.function, test.expected_ident);
                assert_eq!(call.arguments.len(), test.expected_args.len());

                let mut args = (&call.arguments).into_iter();

                for arg in test.expected_args {
                    assert_eq!(arg.to_string(), args.next().unwrap().to_string());
                }
            },
            _ => panic!("{:?} is not a call expression", exp)
        }
    }
}

#[test]
fn string_literal_expression() {
    let input = r#""hello world""#;
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Str(s) => assert_eq!(s, "hello world"),
        _ => panic!("Expected string literal but got {:?} instead", exp)
    }
}

#[test]
fn array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Array(a) => {
            test_integer_literal(a.elements.first().unwrap(), 1);
            test_infix(a.elements.get(1).unwrap(), 2, Token::Asterisk, 2);
            test_infix(a.elements.last().unwrap(), 3, Token::Plus, 3);
        },
        _ => panic!("Expected array literal but got {:?} instead", exp)
    }
}

 #[test]
fn index_expressions() {
    let input = "myArray[1 + 1]";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Index(i) => {
            test_identifier(&i.left, "myArray");
            test_infix(&i.index, 1, Token::Plus, 1);
        },
        _ => panic!("Expected an index expression but got {:?} instead", exp)
    }
}

#[test]
fn hash_literals() {
    let input = r#"{"one": 1, "two": 2, "three": 3, 4: 4, true: true}"#;
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Hash(h) => {
            assert_eq!(h.pairs.len(), 5);

            for (k, v) in &h.pairs {
                match (&k, &v) {
                    (Expression::Str(key), Expression::Integer(int)) => {
                        match key.as_str() {
                            "one" => assert_eq!(1, *int),
                            "two" => assert_eq!(2, *int),
                            "three" => assert_eq!(3, *int),
                            _ => panic!("Unexpected key {}", key)
                        }
                    },
                    (Expression::Integer(key), Expression::Integer(int)) => {
                        assert_eq!(*key, *int);
                        assert_eq!(*int, 4);
                    },
                    (Expression::Boolean(key), Expression::Boolean(val)) => {
                        assert_eq!(key, val)
                    },
                    _ => panic!("Expected key to be a string and value to be an int but got {:?} and {:?} instead", k, v)
                }
            }
        },
        _ => panic!("Expected a hash literal but got {:?} instead", exp)
    }
}

#[test]
fn hash_literal_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Hash(h) => {
            assert_eq!(h.pairs.len(), 3);

            for (k, v) in &h.pairs {
                match (&k, &v) {
                    (Expression::Str(key), Expression::Infix(_)) => {
                        match key.as_str() {
                            "one" => test_infix(v, 0, Token::Plus, 1),
                            "two" => test_infix(v, 10, Token::Minus, 8),
                            "three" => test_infix(v, 15, Token::Slash, 5),
                            _ => panic!("Unexpected key {}", key)
                        }
                    },
                    _ => panic!("Expected key to be a string and value to be an infix expression but got {:?} and {:?} instead", k, v)
                }
            }
        },
        _ => panic!("Expected a hash literal but got {:?} instead", exp)
    }
}

#[test]
fn empty_hash_literal() {
    let input = "{}";
    let program = setup(input, 1);
    let exp = unwrap_expression(&program);

    match exp {
        Expression::Hash(h) => {
            assert_eq!(h.pairs.len(), 0);
        },
        _ => panic!("Expected a hash literal but got {:?} instead", exp)
    }
}