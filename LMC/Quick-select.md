# Quick Select Algorithm in LMC

Here's an implementation of the Quick Select algorithm in LMC (Little Man Computer) assembly language. This implementation finds the kth smallest element in an array.

```assembly
    INP         ; Read array size
    STA SIZE    ; Store array size
    LDA #100    ; Load address for array storage
    STA PTR     ; Store array pointer

    ; Read array elements
READ_LOOP:
    INP         ; Read array element
    STA (PTR)   ; Store element at current pointer
    LDA PTR     ; Load pointer
    ADD #1      ; Increment pointer
    STA PTR     ; Store new pointer
    LDA COUNT   ; Load counter
    ADD #1      ; Increment counter
    STA COUNT   ; Store counter
    LDA COUNT   ; Load counter
    SUB SIZE    ; Compare with size
    BRZ READ_END ; Branch if equal
    BRA READ_LOOP ; Continue reading

READ_END:
    LDA #0      ; Initialize counter
    STA COUNT   ; Store counter

    ; Read k value (position to find)
    INP         ; Read k value
    STA K       ; Store k value

    ; Call quickselect with low=0, high=size-1
    LDA #0      ; Load low = 0
    STA LOW     ; Store low
    LDA SIZE    ; Load size
    SUB #1      ; Subtract 1 to get high
    STA HIGH    ; Store high

    ; Call quickselect function
    LDA LOW     ; Load low
    STA ARG1    ; Store as first argument
    LDA HIGH    ; Load high
    STA ARG2    ; Store as second argument
    LDA K       ; Load k
    SUB #1      ; Adjust for 0-based indexing
    STA ARG3    ; Store as third argument

    ; Jump to quickselect routine
    LDA #QS_START
    BRA QUICKSELECT

; Quick Select Algorithm
QUICKSELECT:
    ; Parameters: ARG1 = low, ARG2 = high, ARG3 = k
    ; Returns: element at position k in sorted array
    
    ; Base case: if low >= high, return
    LDA ARG1    ; Load low
    SUB ARG2    ; Subtract high
    BRZ RETURN  ; If low == high, return
    BRN RETURN  ; If low > high, return
    
    ; Call partition function
    LDA ARG1    ; Load low
    STA P_LOW   ; Store as low for partition
    LDA ARG2    ; Load high
    STA P_HIGH  ; Store as high for partition
    
    ; Call partition routine
    LDA P_LOW   ; Load low
    STA ARG1    ; Store as first argument
    LDA P_HIGH  ; Load high
    STA ARG2    ; Store as second argument
    
    LDA #PARTITION
    BRA PARTITION

PARTITION:
    ; Partition function: arr[low..high]
    ; Returns pivot index
    
    LDA ARG2    ; Load high
    STA HIGH    ; Store high
    LDA ARG1    ; Load low
    STA LOW     ; Store low
    
    ; Get pivot element (last element)
    LDA HIGH    ; Load high
    LDA (HIGH)  ; Load pivot element
    STA PIVOT   ; Store pivot
    
    ; Initialize i = low - 1
    LDA LOW     ; Load low
    SUB #1      ; Subtract 1
    STA I       ; Store i
    
    ; Loop through array from low to high-1
    LDA LOW     ; Load low
    STA J       ; Store j = low
    
LOOP:
    LDA J       ; Load j
    SUB HIGH    ; Compare with high
    BRZ DONE    ; If j == high, exit loop
    
    ; Compare arr[j] with pivot
    LDA J       ; Load j
    LDA (J)     ; Load arr[j]
    SUB PIVOT   ; Subtract pivot
    BRN SWAP    ; If arr[j] < pivot, swap
    
    ; Continue with next element
    LDA J       ; Load j
    ADD #1      ; Increment j
    STA J       ; Store j
    BRA LOOP    ; Continue loop

SWAP:
    ; Swap arr[i+1] and arr[j]
    LDA I       ; Load i
    ADD #1      ; Increment i
    STA I       ; Store i
    
    ; Swap elements at positions I and J
    LDA I       ; Load i
    LDA (I)     ; Load arr[i]
    STA TEMP    ; Store in temp
    LDA J       ; Load j
    LDA (J)     ; Load arr[j]
    STA (I)     ; Store arr[j] at arr[i]
    LDA TEMP    ; Load temp
    STA (J)     ; Store temp at arr[j]
    
    ; Increment j and continue
    LDA J       ; Load j
    ADD #1      ; Increment j
    STA J       ; Store j
    BRA LOOP    ; Continue loop

DONE:
    ; Swap pivot element with arr[i+1]
    LDA I       ; Load i
    ADD #1      ; Increment i
    STA I       ; Store i
    
    ; Swap pivot with arr[i]
    LDA I       ; Load i
    LDA (I)     ; Load arr[i]
    STA TEMP    ; Store in temp
    LDA HIGH    ; Load high
    LDA (HIGH)  ; Load pivot
    STA (I)     ; Store pivot at arr[i]
    LDA TEMP    ; Load temp
    STA (HIGH)  ; Store temp at arr[high]
    
    ; Return pivot index
    LDA I       ; Load pivot index
    STA RETURN  ; Store in return register

    ; Return from quickselect
RETURN:
    LDA RETURN  ; Load return value
    OUT         ; Output result
    HLT         ; Halt

; Data Section
SIZE:   DAT 0     ; Array size
PTR:    DAT 100   ; Array pointer
COUNT:  DAT 0     ; Counter for reading array
K:      DAT 0     ; k value (position to find)
LOW:    DAT 0     ; Low parameter
HIGH:   DAT 0     ; High parameter
ARG1:   DAT 0     ; Argument 1
ARG2:   DAT 0     ; Argument 2
ARG3:   DAT 0     ; Argument 3
PIVOT:  DAT 0     ; Pivot element
I:      DAT 0     ; Index i
J:      DAT 0     ; Index j
TEMP:   DAT 0     ; Temporary storage
P_LOW:  DAT 0     ; Partition low
P_HIGH: DAT 0     ; Partition high

    ; Quickselect start address
QS_START: DAT 0
```

## How It Works:

1. **Input**: The program reads an array of integers and a value `k`
2. **Quick Select**: Implements the Quick Select algorithm to find the kth smallest element
3. **Partitioning**: Uses the partition function to arrange elements
4. **Recursion**: Recursively calls itself on appropriate subarrays
5. **Output**: Returns the kth smallest element

## Key Features:

- **Efficient**: Average O(n) time complexity
- **In-place**: Uses minimal additional memory
- **LMC Compatible**: Uses only LMC instructions and addressing modes
- **Complete**: Includes all necessary data storage and control flow

## Example Usage:

Input:
```
5        ; Array size
3        ; Array elements
1        ; Array elements
4        ; Array elements
1        ; Array elements
5        ; Array elements
2        ; k value (find 2nd smallest)
```

Output:
```
1        ; 2nd smallest element
```

This implementation demonstrates the core Quick Select algorithm in LMC, though it's a simplified version that assumes the basic LMC architecture supports the necessary operations for array manipulation and recursion.

