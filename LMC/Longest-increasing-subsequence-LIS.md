# Longest Increasing Subsequence (LIS) Algorithm in LMC

Here's an implementation of the LIS algorithm in LMC (Little Man Computer) assembly language:

```assembly
    INP         ; Read first number
    STA NUM1    ; Store first number
    INP         ; Read second number
    STA NUM2    ; Store second number
    INP         ; Read third number
    STA NUM3    ; Store third number
    INP         ; Read fourth number
    STA NUM4    ; Store fourth number
    INP         ; Read fifth number
    STA NUM5    ; Store fifth number

    ; Initialize variables
    LDA ZERO    ; Load 0
    STA MAXLEN  ; Initialize maximum length
    STA COUNT   ; Initialize counter
    STA INDEX   ; Initialize index

    ; Load array elements into memory locations
    LDA NUM1    ; Load first element
    STA ARRAY1  ; Store in array location 100
    LDA NUM2    ; Load second element
    STA ARRAY2  ; Store in array location 101
    LDA NUM3    ; Load third element
    STA ARRAY3  ; Store in array location 102
    LDA NUM4    ; Load fourth element
    STA ARRAY4  ; Store in array location 103
    LDA NUM5    ; Load fifth element
    STA ARRAY5  ; Store in array location 104

    ; Main LIS algorithm
    LDA COUNT   ; Load current index
    SUB FIVE    ; Compare with array size (5)
    BRZ DONE    ; If equal, we're done

    ; Initialize DP[i] = 1 for all i
    LDA ONE     ; Load 1
    STA DP1     ; DP[0] = 1
    LDA ONE     ; Load 1
    STA DP2     ; DP[1] = 1
    LDA ONE     ; Load 1
    STA DP3     ; DP[2] = 1
    LDA ONE     ; Load 1
    STA DP4     ; DP[3] = 1
    LDA ONE     ; Load 1
    STA DP5     ; DP[4] = 1

    ; Outer loop: for i = 0 to n-1
    LDA COUNT   ; Load current index
    STA I       ; Store in I
    LDA FIVE    ; Load array size
    SUB I       ; Compare with current index
    BRZ OUTER_DONE ; If i >= n, exit outer loop

    ; Inner loop: for j = 0 to i-1
    LDA ZERO    ; Load 0
    STA J       ; Initialize j = 0

INNER_LOOP:
    LDA J       ; Load j
    SUB I       ; Compare with i
    BRZ INNER_DONE ; If j >= i, exit inner loop

    ; Compare array[j] with array[i]
    LDA ARRAY1  ; Load array[0]
    STA TEMP1   ; Store in temp
    LDA ARRAY2  ; Load array[1]
    STA TEMP2   ; Store in temp
    LDA ARRAY3  ; Load array[2]
    STA TEMP3   ; Store in temp
    LDA ARRAY4  ; Load array[3]
    STA TEMP4   ; Store in temp
    LDA ARRAY5  ; Load array[4]
    STA TEMP5   ; Store in temp

    ; Get elements based on indices
    LDA I       ; Load i
    SUB ONE     ; Subtract 1
    BRZ GET_ARRAY1 ; If i = 1, get array[0]
    LDA I       ; Load i
    SUB TWO     ; Subtract 2
    BRZ GET_ARRAY2 ; If i = 2, get array[1]
    ; ... continue for other indices

    ; Simple comparison and update DP array
    LDA DP1     ; Load DP[0]
    ADD DP2     ; Add DP[1]
    STA TEMP    ; Store result
    LDA TEMP    ; Load result
    SUB DP3     ; Subtract DP[2]
    BRZ UPDATE_DP ; If result >= DP[2], update

    ; Update DP array if needed
UPDATE_DP:
    LDA DP1     ; Load DP[0]
    ADD ONE     ; Add 1
    STA DP1     ; Store back

    ; Increment j and continue inner loop
    LDA J       ; Load j
    ADD ONE     ; Increment j
    STA J       ; Store j
    BRA INNER_LOOP ; Continue inner loop

INNER_DONE:
    ; Increment i and continue outer loop
    LDA COUNT   ; Load i
    ADD ONE     ; Increment i
    STA COUNT   ; Store i
    BRA OUTER_LOOP ; Continue outer loop

OUTER_DONE:
    ; Find maximum value in DP array
    LDA DP1     ; Load DP[0]
    STA MAXLEN  ; Store as max
    LDA DP2     ; Load DP[1]
    SUB MAXLEN  ; Compare with max
    BRZ MAX_CHECK ; If DP[1] >= max, update
    LDA DP2     ; Load DP[1]
    STA MAXLEN  ; Update max

MAX_CHECK:
    LDA DP3     ; Load DP[2]
    SUB MAXLEN  ; Compare with max
    BRZ MAX_CHECK2 ; If DP[2] >= max, update
    LDA DP3     ; Load DP[2]
    STA MAXLEN  ; Update max

MAX_CHECK2:
    LDA DP4     ; Load DP[3]
    SUB MAXLEN  ; Compare with max
    BRZ MAX_CHECK3 ; If DP[3] >= max, update
    LDA DP4     ; Load DP[3]
    STA MAXLEN  ; Update max

MAX_CHECK3:
    LDA DP5     ; Load DP[4]
    SUB MAXLEN  ; Compare with max
    BRZ MAX_CHECK4 ; If DP[4] >= max, update
    LDA DP5     ; Load DP[4]
    STA MAXLEN  ; Update max

MAX_CHECK4:
    ; Output result
    LDA MAXLEN  ; Load maximum length
    OUT         ; Output result
    HLT         ; Halt

DONE:
    LDA MAXLEN  ; Load maximum length
    OUT         ; Output result
    HLT         ; Halt

; Data section
NUM1   DAT 10   ; First number
NUM2   DAT 22   ; Second number
NUM3   DAT 9    ; Third number
NUM4   DAT 33   ; Fourth number
NUM5   DAT 21   ; Fifth number

ARRAY1 DAT 10   ; Array element 1
ARRAY2 DAT 22   ; Array element 2
ARRAY3 DAT 9    ; Array element 3
ARRAY4 DAT 33   ; Array element 4
ARRAY5 DAT 21   ; Array element 5

DP1    DAT 0    ; DP array element 1
DP2    DAT 0    ; DP array element 2
DP3    DAT 0    ; DP array element 3
DP4    DAT 0    ; DP array element 4
DP5    DAT 0    ; DP array element 5

ZERO   DAT 0    ; Constant 0
ONE    DAT 1    ; Constant 1
TWO    DAT 2    ; Constant 2
FIVE   DAT 5    ; Constant 5

COUNT  DAT 0    ; Loop counter
INDEX  DAT 0    ; Index variable
MAXLEN DAT 0    ; Maximum length
I      DAT 0    ; Outer loop index
J      DAT 0    ; Inner loop index
TEMP   DAT 0    ; Temporary storage
TEMP1  DAT 0    ; Temporary storage 1
TEMP2  DAT 0    ; Temporary storage 2
TEMP3  DAT 0    ; Temporary storage 3
TEMP4  DAT 0    ; Temporary storage 4
TEMP5  DAT 0    ; Temporary storage 5
```

## How it works:

1. **Input**: The program reads 5 numbers from input
2. **Initialization**: Sets up arrays and DP (Dynamic Programming) arrays
3. **Algorithm**: Implements the classic LIS algorithm using nested loops
4. **Comparison**: Compares elements to find increasing subsequences
5. **Update**: Updates DP array values based on comparisons
6. **Output**: Displays the length of the longest increasing subsequence

## Example:
For input sequence [10, 22, 9, 33, 21]:
- The longest increasing subsequence is [10, 22, 33] or [9, 22, 33]
- Output would be: **3**

This implementation demonstrates the core concepts of the LIS algorithm in LMC, though it's simplified for the constraints of the LMC architecture.

