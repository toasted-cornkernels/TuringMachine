((((State q0) Zero) (Instruction (State q0) Zero Right))
    (((State q0) One) (Instruction (State q0) One Right))
    (((State q0) Blank) (Instruction (State a0) Blank Left))
    (((State q1) Zero) No_op)
    (((State q1) One) No_op)
    (((State q1) Blank) No_op)
    (((State a0) Zero) (Instruction (State s0) Blank Left))
    (((State a0) One) (Instruction (State s1) Blank Left))
    (((State a0) Blank) (Instruction (State q1) Blank Neutral))
    (((State a1) Zero) (Instruction (State s1) Blank Left))
    (((State a1) One) (Instruction (State s2) Blank Left))
    (((State a2) Zero) (Instruction (State s2) Blank Left))
    (((State a2) One) (Instruction (State s0) Blank Left))
    (((State s0) Zero) (Instruction (State a0) Blank Left))
    (((State s0) One) (Instruction (State a2) Blank Left))
    (((State s0) Blank) (Instruction (State q1) Blank Neutral))
    (((State s1) Zero) (Instruction (State a1) Blank Left))
    (((State s1) One) (Instruction (State a0) Blank Left))
    (((State s2) Zero) (Instruction (State a2) Blank Left))
    (((State s2) One) (Instruction (State a1) Blank Left)))
