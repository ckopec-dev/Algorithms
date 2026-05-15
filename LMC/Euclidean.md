# Euclidean Algorithm in LMC (Little Man Computer)

Here's an implementation of the Euclidean algorithm (GCD) in LMC:

```assembly
        INP         // Read first number (A)
        STA NUM1    // Store A
        INP         // Read second number (B)
        STA NUM2    // Store B
        LDA NUM1    // Load A
        STA A       // Store A in A register
        LDA NUM2    // Load B
        STA B       // Store B in B register

LOOP    LDA A       // Load A
        SUB B       // A - B
        BRZ DONE    // If A = B, goto DONE
        BRN NEG     // If A < B, goto NEG

        STA A       // Store result back in A
        BRA LOOP    // Repeat loop

NEG     LDA B       // Load B
        SUB A       // B - A
        STA B       // Store result back in B
        BRA LOOP    // Repeat loop

DONE    LDA A       // Load final result (GCD)
        OUT         // Output result
        HLT         // Halt

NUM1    DAT 0       // First input number
NUM2    DAT 0       // Second input number
A       DAT 0       // Working register A
B       DAT 0       // Working register B
```

## How it works:

1. **Input**: Takes two numbers A and B from user input
2. **Loop**: Repeatedly subtracts the smaller number from the larger
3. **Branching**: 
   - If A = B, we found the GCD
   - If A < B, swap roles and subtract A from B
   - If A > B, subtract B from A
4. **Output**: The final value in A is the GCD

## Example execution:
- Input: A = 48, B = 18
- Step 1: 48 - 18 = 30, A = 30, B = 18
- Step 2: 30 - 18 = 12, A = 12, B = 18
- Step 3: 18 - 12 = 6, A = 12, B = 6
- Step 4: 12 - 6 = 6, A = 6, B = 6
- Step 5: A = B, output 6 (GCD of 48 and 18)

This implementation uses the fundamental principle that GCD(A,B) = GCD(B, A mod B) and repeatedly applies subtraction until the two numbers become equal.

