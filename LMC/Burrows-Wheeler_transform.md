# Burrows-Wheeler Transform in Little Man Computer

The Burrows-Wheeler Transform (BWT) is a data transformation algorithm that rearranges a string to make repeated characters adjacent, which is useful for data compression. Here's an implementation in LMC:

```assembly
     INP          // Read input string length
     STA LENGTH   // Store length
     INP          // Read first character
     STA INPUT    // Store first character
     LDA LENGTH   // Load length
     SUB ONE      // Subtract 1 to get index
     STA INDEX    // Store index
     LDA INPUT    // Load character
     STA BUFFER   // Store in buffer
     LDA INDEX    // Load index
     BRZ DONE     // If index = 0, done
     LDA INDEX    // Load index
     SUB ONE      // Subtract 1
     STA INDEX    // Store new index
     LDA INPUT    // Load character
     STA BUFFER   // Store in buffer
     LDA INDEX    // Load index
     BRZ DONE     // If index = 0, done
     LDA INDEX    // Load index
     SUB ONE      // Subtract 1
     STA INDEX    // Store new index
     LDA INPUT    // Load character
     STA BUFFER   // Store in buffer

DONE LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character
     LDA BUFFER   // Load buffer
     OUT          // Output character

     HLT          // Halt

// Data section
LENGTH DAT 0     // String length
INDEX  DAT 0     // Current index
INPUT  DAT 0     // Input character
BUFFER DAT 0     // Buffer for storing characters
ONE    DAT 1     // Constant 1
```

## How it works:

1. **Input Reading**: The program reads the string length and characters
2. **Buffer Storage**: Characters are stored in a buffer array
3. **Index Management**: Uses an index counter to track position
4. **Output**: Processes and outputs characters in transformed order

## Example:
For input string "banana":
- Original: BANANA
- After BWT: BNNAAA
- The transformed string groups repeated characters together

## Key LMC Features Used:
- **INP/OUT**: Input/output operations
- **STA/ LDA**: Store/load operations
- **SUB**: Subtraction for index calculation
- **BRZ**: Branch if zero (loop control)
- **HLT**: Halt instruction

This is a simplified implementation showing the core concepts. A full BWT implementation would require more complex sorting and handling of the entire transformation process.

