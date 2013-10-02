Chip name: CPU

Inputs:
    inM[16],           // M value input (M = contents of RAM[A])
    instruction[16],   // Instruction for execution
    reset              // Whether to restart current program (reset=1) or continue executing

Outputs:
    outM[16],          // M value output
    writeM,            // Write to M?
    addressM[15],      // Address of M in data memory
    pc[15]             // Address of next instruction

Function:
    Executes the instruction according to the Hack machine language specification. The D and A in
    the language specification refer to the CPU-resident registers, while M refers to the memory
    location addressed by A (inM holds the value of this location).

    If the instruction needs to write a value to M, the value is placed in outM, the address is
    placed in addressM, and the writeMbit is asserted (when writeM=0, any value may appear in outM)

    If reset=1, then the CPU jumps to address 0 (i.e., sets pc=0 in the next time unit) rather than
    to the address resulting from executing the current instruction.
