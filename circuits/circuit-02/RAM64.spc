Chip name: RAM64

Inputs:
    in[16],        // 16-bit word to be written to RAM
    load,          // Whether the input should be written to the selected address
    address[6]     // Address selected for reading or writing

Outputs:
    out[16]        // The value stored currently in the memory cell specified by address

Function:
    A memory bank of 64 registers, with each register 16-bit wide. The output "out" holds the value
    stored at the memory location specified by "address". If load=1, then the value at "in" is
    loaded into the memory location specified by "address". The loaded value will be emitted on the
    output at the NEXT clock cycle.
