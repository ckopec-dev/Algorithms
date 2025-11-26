# Quadratic Sieve Algorithm in Little Man Computer

Below is a simplified example of a quadratic sieve algorithm implementation in Little Man Computer (LMC). This is a conceptual implementation that demonstrates the core concepts of the algorithm.

```assembly
    INP         // Input number to factor
    STA NUM     // Store number to factor
    LDA ONE     // Load 1
    STA X       // Initialize x = 1
    LDA TWO     // Load 2
    STA Y       // Initialize y = 2

LOOP    LDA X       // Load x
        STA TEMP1   // Store x
        LDA X       // Load x
        MUL X       // x * x
        STA TEMP2   // Store x^2
        LDA TEMP2   // Load x^2
        LDA NUM     // Load number
        SUB NUM     // x^2 - n
        STA DIFF    // Store difference
        LDA DIFF    // Load difference
        LDA ZERO    // Load 0
        SUB DIFF    // Check if difference < 0
        BRZ FOUND   // If difference = 0, we found a factor
        LDA X       // Load x
        ADD ONE     // x + 1
        STA X       // Store new x
        LDA Y       // Load y
        ADD ONE     // y + 1
        STA Y       // Store new y
        BRA LOOP    // Go back to loop

FOUND   LDA X       // Load x
        OUT         // Output x
        LDA Y       // Load y
        OUT         // Output y
        HLT         // Halt

NUM     DAT 0     // Number to factor (input)
X       DAT 0     // x variable
Y       DAT 0     // y variable
DIFF    DAT 0     // Difference storage
TEMP1   DAT 0     // Temporary storage
TEMP2   DAT 0     // Temporary storage
ONE     DAT 1     // Constant 1
TWO     DAT 2     // Constant 2
ZERO    DAT 0     // Constant 0
```

## Simplified Quadratic Sieve Logic

Here's a more realistic LMC implementation focusing on the core quadratic sieve concepts:

```assembly
    INP         // Input number to factor
    STA N       // Store N
    LDA ONE     // Load 1
    STA X       // Initialize x = 1

MAIN    LDA X       // Load x
        STA X2      // Store x
        LDA X       // Load x
        MUL X2      // x * x = x^2
        STA X2      // Store x^2
        LDA X2      // Load x^2
        LDA N       // Load N
        SUB N       // x^2 - N
        STA DIFF    // Store difference
        LDA DIFF    // Load difference
        LDA ZERO    // Load 0
        SUB DIFF    // Check if difference < 0
        BRZ CHECK   // If difference = 0, check if factor
        LDA X       // Load x
        ADD ONE     // x + 1
        STA X       // Store new x
        BRA MAIN    // Continue loop

CHECK   LDA X       // Load x
        OUT         // Output x
        LDA X       // Load x
        MUL X       // x * x
        STA X2      // Store x^2
        LDA X2      // Load x^2
        LDA N       // Load N
        SUB N       // x^2 - N
        STA DIFF    // Store difference
        LDA DIFF    // Load difference
        LDA ZERO    // Load 0
        SUB DIFF    // Check if difference = 0
        BRZ FACTOR  // If difference = 0, found factor
        LDA X       // Load x
        ADD ONE     // x + 1
        STA X       // Store new x
        BRA MAIN    // Continue loop

FACTOR  LDA X       // Load x
        OUT         // Output x
        HLT         // Halt

N       DAT 0     // Number to factor
X       DAT 0     // x variable
X2      DAT 0     // x squared
DIFF    DAT 0     // Difference storage
ONE     DAT 1     // Constant 1
ZERO    DAT 0     // Constant 0
```

## Key Components Explained

1. **Input/Output**: Uses `INP` and `OUT` for user interaction
2. **Variables**: Store values in memory locations (N, X, X2, DIFF)
3. **Loop Control**: Uses branching (`BRA`, `BRZ`) to control program flow
4. **Arithmetic**: Implements multiplication and subtraction operations
5. **Factor Detection**: Checks if x² - N = 0 (simplified factor finding)

## Limitations

This is a simplified educational example. A full quadratic sieve algorithm would require:
- More complex modular arithmetic
- Polynomial evaluation
- Smooth number detection
- Linear algebra for combining relations
- Much more memory for storing large numbers

The LMC implementation demonstrates the basic structure but cannot handle the full complexity of actual quadratic sieve computations due to LMC's limited memory and arithmetic capabilities.

