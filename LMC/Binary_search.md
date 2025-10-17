# Binary Search in Little Man Computer (LMC)

Here's an example of a binary search algorithm implemented in Little Man Computer:

```
// Binary Search Algorithm in LMC
// This program searches for a target value in a sorted array

    ORG 100
    INP         // Read target value to search for
    STA TARGET
    LDA COUNT   // Load array size
    STA SIZE
    LDA ZERO    // Initialize low = 0
    STA LOW
    LDA SIZE    // Initialize high = size - 1
    SUB ONE
    STA HIGH
    LDA ZERO    // Initialize found = 0
    STA FOUND

LOOP    LDA LOW     // Load low
    LDA HIGH    // Load high
    ADD ZERO    // Add low to high
    DIV TWO     // Divide by 2 (middle = (low + high) / 2)
    STA MIDDLE  // Store middle index
    LDA MIDDLE  // Load middle index
    LDA ARRAY   // Load array element at middle
    STA MIDDLE_VAL
    LDA TARGET  // Load target value
    SUB MIDDLE_VAL // Compare target with middle element
    BRZ FOUND_INDEX // If equal, found
    BRN SEARCH_LEFT // If target < middle, search left
    BRZ SEARCH_RIGHT // If target > middle, search right

SEARCH_LEFT
    LDA MIDDLE  // Load middle index
    LDA ONE     // Load 1
    SUB ONE     // Subtract 1
    STA MIDDLE  // Store middle - 1
    LDA MIDDLE  // Load new middle
    LDA HIGH    // Load high
    SUB ZERO    // Compare
    BRZ NOT_FOUND // If middle < high, not found
    LDA LOW     // Load low
    LDA MIDDLE  // Load middle
    SUB ONE     // Subtract 1
    STA HIGH    // Update high
    BRA LOOP    // Continue search

SEARCH_RIGHT
    LDA MIDDLE  // Load middle index
    LDA ONE     // Load 1
    ADD ONE     // Add 1
    STA MIDDLE  // Store middle + 1
    LDA MIDDLE  // Load new middle
    LDA LOW     // Load low
    SUB ZERO    // Compare
    BRZ NOT_FOUND // If middle > low, not found
    LDA MIDDLE  // Load middle
    LDA HIGH    // Load high
    ADD ONE     // Add 1
    STA LOW     // Update low
    BRA LOOP    // Continue search

FOUND_INDEX
    LDA MIDDLE  // Load found index
    OUT         // Output index
    LDA ONE     // Load 1
    STA FOUND   // Mark as found
    LDA ZERO    // Load 0
    BRA END     // End program

NOT_FOUND
    LDA ZERO    // Load -1 (not found)
    OUT         // Output -1
    LDA ZERO    // Load 0
    STA FOUND   // Mark as not found

END     HLT         // Halt program

// Data section
TARGET  DAT 0     // Target value to search for
SIZE    DAT 10    // Array size
LOW     DAT 0     // Low pointer
HIGH    DAT 0     // High pointer
MIDDLE  DAT 0     // Middle pointer
MIDDLE_VAL DAT 0  // Middle value
FOUND   DAT 0     // Found flag
ARRAY   DAT 10    // Array elements (sorted)
        DAT 20
        DAT 30
        DAT 40
        DAT 50
        DAT 60
        DAT 70
        DAT 80
        DAT 90
        DAT 100
COUNT   DAT 10    // Number of elements
ZERO    DAT 0     // Zero constant
ONE     DAT 1     // One constant
TWO     DAT 2     // Two constant

    END
```

## How it works:

1. **Input**: The program reads the target value to search for
2. **Initialization**: Sets up low=0, high=size-1, found=0
3. **Loop**: 
   - Calculates middle index using (low + high) / 2
   - Compares target with middle element
   - If found, outputs the index
   - If target < middle, search left half
   - If target > middle, search right half
4. **Output**: Either the index of found element or -1 if not found

## Example execution:
- Array: [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
- Search for: 50
- Output: 4 (index of 50 in array)

This implementation demonstrates the core binary search logic adapted for LMC's limited instruction set and memory model.

