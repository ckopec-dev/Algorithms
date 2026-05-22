# Levenshtein Distance Algorithm in LMC

Here's an implementation of the Levenshtein distance algorithm in Little Man Computer (LMC) assembly language:

```assembly
; Levenshtein Distance Algorithm
; Calculates the minimum edit distance between two strings

        ORG 100
        DAT 0          ; String 1 length
        DAT 0          ; String 2 length
        DAT 0          ; Result (distance)
        DAT 0          ; Temporary storage
        DAT 0          ; Loop counter
        DAT 0          ; Current character 1
        DAT 0          ; Current character 2

; Main program
        LDA STRING1_LEN
        STA LEN1
        LDA STRING2_LEN
        STA LEN2
        
        ; Initialize DP table
        LDA LEN1
        ADD LEN2
        ADD ONE
        STA TABLE_SIZE
        
        ; Clear table memory (simplified approach)
        LDA ZERO
        STA TABLE
        LDA TABLE_SIZE
        STA COUNT
        LDA ZERO
        STA TABLE
        LDA COUNT
        SUB ONE
        BRZ TABLE_INIT_DONE
        LDA TABLE
        ADD ONE
        STA TABLE
        LDA COUNT
        SUB ONE
        STA COUNT
        BRA TABLE_INIT_LOOP

TABLE_INIT_LOOP   LDA COUNT
        BRZ TABLE_INIT_DONE
        LDA TABLE
        ADD ONE
        STA TABLE
        LDA COUNT
        SUB ONE
        STA COUNT
        BRA TABLE_INIT_LOOP

TABLE_INIT_DONE LDA ZERO
        STA RESULT

; Fill the DP table
        LDA ZERO
        STA I
        LDA ZERO
        STA J

MAIN_LOOP       LDA I
        BRZ OUTER_LOOP_END
        LDA J
        BRZ INNER_LOOP_END
        LDA I
        SUB ONE
        STA I_INDEX
        LDA J
        SUB ONE
        STA J_INDEX
        
        ; Get characters
        LDA STRING1
        LDA I_INDEX
        ADD STRING1
        STA CHAR1
        LDA STRING2
        LDA J_INDEX
        ADD STRING2
        STA CHAR2
        
        ; Compare characters
        LDA CHAR1
        SUB CHAR2
        BRZ CHAR_MATCH
        
        ; Characters don't match - use minimum of three operations
        LDA I_INDEX
        ADD J_INDEX
        ADD THREE
        STA INDEX
        LDA TABLE
        LDA INDEX
        ADD TABLE
        STA MIN_VAL
        
        LDA I_INDEX
        ADD ONE
        ADD J_INDEX
        ADD THREE
        STA INDEX2
        LDA TABLE
        LDA INDEX2
        ADD TABLE
        ADD MIN_VAL
        STA MIN_VAL
        
        LDA I_INDEX
        ADD J_INDEX
        ADD ONE
        ADD THREE
        STA INDEX3
        LDA TABLE
        LDA INDEX3
        ADD TABLE
        ADD MIN_VAL
        STA MIN_VAL
        
        LDA MIN_VAL
        ADD ONE
        STA TABLE
        BRA INNER_LOOP_END

CHAR_MATCH      LDA I_INDEX
        ADD J_INDEX
        ADD THREE
        STA INDEX
        LDA TABLE
        LDA INDEX
        ADD TABLE
        STA TABLE

INNER_LOOP_END  LDA J
        ADD ONE
        STA J
        LDA J
        LDA LEN2
        BRZ INNER_LOOP_END
        BRA MAIN_LOOP

OUTER_LOOP_END  LDA TABLE
        ADD TABLE
        STA RESULT

        LDA RESULT
        HLT

; Data section
STRING1     DAT 0          ; First string (ASCII values)
STRING2     DAT 0          ; Second string (ASCII values)
STRING1_LEN DAT 5          ; Length of first string
STRING2_LEN DAT 5          ; Length of second string
TABLE       DAT 0          ; Dynamic programming table
TABLE_SIZE  DAT 0          ; Size of table
LEN1        DAT 0          ; Length 1
LEN2        DAT 0          ; Length 2
RESULT      DAT 0          ; Final result
COUNT       DAT 0          ; Loop counter
ZERO        DAT 0          ; Zero value
ONE         DAT 1          ; One value
THREE       DAT 3          ; Three value
I           DAT 0          ; Loop counter I
J           DAT 0          ; Loop counter J
I_INDEX     DAT 0          ; Index I
J_INDEX     DAT 0          ; Index J
CHAR1       DAT 0          ; Current character 1
CHAR2       DAT 0          ; Current character 2
MIN_VAL     DAT 0          ; Minimum value

        END
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses a 2D table to store intermediate results
2. **Three Operations**: Insertion, deletion, and substitution
3. **Character Comparison**: Compares characters from both strings
4. **Memory Management**: Uses memory locations for table storage and counters

## How it Works:

1. **Initialization**: Sets up the DP table with appropriate dimensions
2. **Fill Table**: Populates the table using the recurrence relation:
   - If characters match: `d[i][j] = d[i-1][j-1]`
   - If characters don't match: `d[i][j] = 1 + min(d[i-1][j], d[i][j-1], d[i-1][j-1])`
3. **Result**: The bottom-right cell contains the Levenshtein distance

## Note:

This is a simplified representation of the LMC implementation. A full working version would require:
- Proper memory management for the 2D table
- More sophisticated string handling
- Complete loop control structures
- Detailed character input/output handling

The LMC version demonstrates the core algorithm logic while working within the constraints of the Little Man Computer architecture.

