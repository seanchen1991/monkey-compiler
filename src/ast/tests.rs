use ast::*;

#[test]
fn display() {
    let p = Program {
        statements: vec![
            Statement::Let(Box::new(
                LetStatement {
                    name: "asdf".to_string(),
                    value: Expression::Identifier("bar".to_string())
                }
            ))
        ],
    };

    let expected = "let asdf = bar;";

    assert_eq!(p.to_string(), expected);
}