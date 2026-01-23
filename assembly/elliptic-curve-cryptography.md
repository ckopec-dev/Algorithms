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
    movl $0, %edx                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if array size is 0
    je end_sort                    # If yes, exit
    
    movl $0, %esi                  # Initialize inner loop counter
    
inner_loop:
    cmp %esi, %ecx                 # Compare inner counter with array size
    je outer_decrement             # If equal, decrement outer loop
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[i] into EAX
    movl array(,%esi,4), %ebx      # Load array[i] into EBX
    addl $4, %ebx                  # Load array[i+1] into EBX
    cmpl %ebx, %eax                # Compare array[i] and array[i+1]
    
    jg swap                        # If array[i] > array[i+1], swap them
    
    jmp continue_inner             # Continue inner loop
    
swap:
    # Swap elements
    movl array(,%esi,4), %ebx      # Load array[i] into EBX
    movl array(,%esi,4), %edi      # Load array[i] into EDI
    addl $4, %edi                  # Load array[i+1] into EDI
    movl %ebx, (%edi)              # array[i+1] = array[i]
    movl %edi, (%esi,4)            # array[i] = array[i+1]
    
continue_inner:
    inc %esi                       # Increment inner loop counter
    jmp inner_loop                 # Continue inner loop
    
outer_decrement:
    dec %ecx                       # Decrement outer loop counter
    jmp outer_loop                 # Continue outer loop
    
end_sort:
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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Exchanges elements when the condition is met
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: `cmp`, `je`, `jg`, `jmp` instructions
- **Function calls**: `call` and `ret` for subroutine execution
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

