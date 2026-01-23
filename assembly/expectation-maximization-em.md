# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86 Assembly language:

```assembly
.section .data
    array: .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    push %ebp
    mov %esp, %ebp
    
    movl array_size(%ebp), %ecx    # Load array size into ECX (outer loop counter)
    movl $0, %esi                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if array size is 0
    je sort_done
    
    movl $0, %edi                  # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner counter with array size
    jge inner_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    
    # Compare array[i] and array[i+1]
    movl array(,%edi,4), %ebx      # Load array[i+1] into EBX
    cmpl %ebx, %eax                # Compare array[i] with array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %eax, %ebx                # Store array[i] in EBX
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %ebx, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    incl %edi                      # Increment inner loop counter
    jmp inner_loop
    
inner_loop_end:
    decl %ecx                      # Decrement outer loop counter
    jmp outer_loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation

This bubble sort implementation:
1. **Outer loop**: Controls the number of passes through the array
2. **Inner loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Moves values between array positions
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Registers**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`, `%esp`
- **Memory addressing**: `array(,%edi,4)` for array access with scaling
- **Control flow**: `cmp`, `jle`, `jmp`, `je` instructions
- **Function calls**: `call` and `ret` for subroutine execution
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

