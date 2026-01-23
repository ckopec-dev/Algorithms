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
    
    movl array_size(%ebp), %ecx    # Load array size into ecx (outer loop counter)
    movl $0, %esi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    jle end_sort                   # If ecx <= 0, we're done
    
    movl $0, %edi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array_size to edx for inner loop
    
inner_loop:
    cmp %edi, %edx                 # Compare j and (array_size - i)
    jge end_inner                  # If j >= (array_size - i), exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[j] into eax
    movl array(,%edi,4), %ebx      # Load array[j] into ebx
    movl array(,%edi,4), %ebx      # Load array[j+1] into ebx
    
    # Compare array[j] and array[j+1]
    cmp %ebx, %eax                 # Compare elements
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[j] = array[j+1]
    movl %eax, array(,%edi,4)      # array[j+1] = array[j]
    
no_swap:
    inc %edi                       # j++
    jmp inner_loop                 # Continue inner loop
    
end_inner:
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
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses memory operations to exchange element values
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: `cmp`, `jle`, `jmp`, `jge` instructions
- **Function calling**: Stack management with `push`/`pop`
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are implemented using low-level assembly instructions and addressing modes.

