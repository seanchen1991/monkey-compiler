use bytecode::*;

#[test]
fn test_make_instruction() {
    struct Test {
        op: Opcode,
        operands: Vec<i64>,
        expected: Vec<u16>,
    }

    let def = Definitions::new();

    let tests = vec![
        Test { op: OpConstant, operands: vec![65534], expected: vec![OpConstant, 255, 254] }
    ];

    for test in tests {
        let Instruction(instruction) = def.make_instruction(test.op, test.operands.as_slice()).expect("Expected instruction to be made");
        assert_eq!(instruction.len(), test.expected.len(), "Expected instruction length of {} but got {} instead", test.expected.len(), instruction.len());

        for (i, b) in test.expected.iter().enumerate() {
            assert_eq!(instruction[i], test.expected[i], "Expected byte {} at position {} but got {} instead", b, i, instruction[i]);
        }
    }
}