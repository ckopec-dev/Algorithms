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
    movl $0, %edi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if array size is 0
    je end_sort                    # If size = 0, exit
    
    dec %ecx                       # Decrement outer counter
    movl $0, %esi                  # Initialize inner loop counter j = 0
    
inner_loop:
    movl array(,%esi,4), %eax      # Load array[j] into eax
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (copy)
    cmp %ebx, %eax                 # Compare array[j] with array[j+1]
    jge next_iteration             # If array[j] >= array[j+1], continue
    
    # Swap elements
    movl array(,%esi,4), %ebx      # Load array[j] into ebx
    movl array(4,%esi,4), %eax     # Load array[j+1] into eax
    movl %eax, array(,%esi,4)      # array[j] = array[j+1]
    movl %ebx, array(4,%esi,4)     # array[j+1] = array[j]
    
next_iteration:
    inc %esi                       # Increment j
    cmp %ecx, %esi                 # Compare j with (n-1)
    jl inner_loop                  # If j < (n-1), continue inner loop
    
    jmp outer_loop                 # Continue with outer loop

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
3. **Swapping**: Uses temporary registers to exchange element values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: Array elements accessed via base + index scaling
- **Control flow**: `cmp`, `jl`, `jge`, `jmp` instructions
- **Function calling**: Stack management with `push`/`pop`
- **System calls**: Exit syscall using `int $0x80`

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

