Chip name: ALU
Inputs:
    x[16], y[16],    // Two 16-bit data inputs
    zx,              // Zero the x input
    nx,              // Negate the x input
    zy,              // Zero the y input
    ny,              // Negate the y input
    f,               // Function to use: 1 for addition, 0 for bitwise and
    no,              // Negate the output

Outputs:
    out[16],         // 16-bit output
    zr,              // True iff out=0
    ng,              // True iff out<0

Function:
    if zx then x = 0          // 16-bit zero constant
    if nx then x = !x         // Bit-wise negation
    if zy then y = 0          // 16-bit zero constant
    if ny then u = !y         // Bit-wise negation
    if f then out = x + y     // Integer 2's complement addition
         else out = x & y     // Bit-wise And
    if no then out = !out     // Bit-wise negation
    if out=0 then zr = 1 else zr = 0    // 16-bit equality comparison
    if out<0 then ng = 1 else ng = 0    // 16-bit negative comparison

Comment:
    Overflow is neither detected nor handled
