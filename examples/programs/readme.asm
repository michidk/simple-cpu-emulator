%endianess(le)
0xff1f:
    %endianess(be)
    PUSHC
    !10
    # print the number
    PRINTN
    # subtract one
    PUSHC
    !1
    NEG
    ADD
    DUP
    # check if we are done
    JUMPZ
    !W0x200B
    # jump back to the print statement
    JUMP
    !W0x2001
    # halt
    HCF
