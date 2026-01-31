# Assembly Algorithm Example: Sum of Array Elements

Here's an example of an assembly algorithm that calculates the sum of elements in an array:

## x86-64 Assembly Implementation

```assembly
.section .data
    array:      .long 10, 20, 30, 40, 50    # Array of 5 integers
    array_size: .long 5                     # Size of array

.section .text
    .global _start

sum_array:
    movl    array_size(%rip), %ecx      # Load array size into ECX (loop counter)
    movl    $0, %eax                    # Initialize sum to 0 (EAX = sum)
    movl    $array, %esi                # Load array address into ESI (pointer)

sum_loop:
    cmpl    $0, %ecx                    # Compare counter with 0
    jz      sum_done                    # Jump if counter is 0 (end loop)
    
    addl    (%esi), %eax                # Add current array element to sum
    addl    $4, %esi                    # Move pointer to next element (4 bytes)
    decl    %ecx                        # Decrement counter
    jmp     sum_loop                    # Jump back to loop start

sum_done:
    # Result is now in EAX register
    ret                                 # Return from function

# Main program execution
_start:
    call    sum_array                   # Call sum_array function
    # Exit program (Linux system call)
    movl    $1, %eax                    # sys_exit system call number
    movl    %eax, %ebx                  # Exit status
    int     $0x80                       # Invoke system call
```

## Algorithm Explanation

**Purpose:** Calculate the sum of all elements in an array

**Steps:**
1. Initialize sum accumulator to 0
2. Load array address and size
3. Loop through each array element:
   - Add current element to sum
   - Move to next element
   - Decrement counter
4. Return final sum

**Key Assembly Concepts Demonstrated:**
- **Register usage**: EAX (accumulator), ECX (counter), ESI (pointer)
- **Memory addressing**: Direct and indexed addressing modes
- **Control flow**: Conditional jumps (JZ, JMP)
- **Arithmetic operations**: ADD, DEC, CMP
- **Function calling**: CALL, RET instructions

**Expected Output:** The sum of 10+20+30+40+50 = 150 will be stored in EAX register.

