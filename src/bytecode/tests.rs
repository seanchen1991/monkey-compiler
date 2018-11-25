use bytecode::*;

#[test]
fn test_make_instructions() {
    struct Test {
        op: Opcode,
        operands: Vec<i64>,
        expected: Vec<u8>,
    }

    let def = Definitions::new();

    let tests = vec![
        Test { op: OpConstant, operands: vec![65534], expected: vec![OpConstant, 255, 254] }
    ];

    for test in tests {
        let Instructions(instructions) = def.make_instructions(test.op, test.operands.as_slice()).expect("Expected instructions to be made");
        assert_eq!(instructions.len(), test.expected.len(), "Expected instructions length of {} but got {} instead", test.expected.len(), instructions.len());

        for (i, b) in test.expected.iter().enumerate() {
            assert_eq!(instructions[i], test.expected[i], "Expected byte {} at position {} but got {} instead", b, i, instructions[i]);
        }
    }
}