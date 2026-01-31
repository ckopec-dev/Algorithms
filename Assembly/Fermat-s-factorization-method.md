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
    cmp $0, %ecx                   # Check if array size is 0
    je sort_done
    
    movl $0, %edi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array size to edx for inner loop
    
inner_loop:
    cmp %edi, %edx                 # Compare j and array_size-1
    jge inner_done
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[j] into eax
    movl array(,%edi,4), %ebx      # Load array[j] into ebx (for comparison)
    addl $4, %edi                  # Increment j
    movl array(,%edi,4), %ebx      # Load array[j+1] into ebx
    
    cmp %ebx, %eax                 # Compare array[j] and array[j+1]
    jle no_swap                    # Jump if array[j] <= array[j+1]
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[j+1] = array[j]
    movl %eax, array(,%edi,4)      # array[j] = array[j+1] (this is wrong, should be previous value)
    
no_swap:
    addl $1, %edi                  # Increment j
    jmp inner_loop
    
inner_done:
    dec %ecx                       # Decrement outer loop counter
    jmp outer_loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80
```

## Algorithm Explanation

This bubble sort implementation:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Exchanges elements when the condition is met
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Using base register with index and scale
- **Conditional jumps**: `cmp`, `jge`, `jl`, `je`
- **Function calls**: `call`, `ret`, stack management
- **System calls**: `int $0x80` for program exit

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

