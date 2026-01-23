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
    
    movl array_size(%ebp), %ecx      # Load array size into ECX (outer loop counter)
    movl $0, %esi                    # Initialize outer loop counter i = 0
    
outer_loop:
    cmpl $0, %ecx                    # Check if array size is 0
    jle end_sort                     # If size <= 0, exit
    
    movl $0, %edi                    # Initialize inner loop counter j = 0
    movl %ecx, %edx                  # Copy array size to EDX for inner loop
    
inner_loop:
    cmpl $1, %edx                    # Check if we need to compare
    jle inner_loop_end               # If only one element, exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax        # Load array[j] into EAX
    movl array(,%edi,4), %ebx        # Load array[j] into EBX
    movl array(,%edi,4), %ebx        # Load array[j+1] into EBX
    
    # Compare and swap if needed
    cmpl %ebx, %eax                  # Compare array[j] and array[j+1]
    jle no_swap                      # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)        # array[j] = array[j+1]
    movl %eax, array(,%edi,4)        # array[j+1] = array[j]
    
no_swap:
    incl %edi                        # j++
    decl %edx                        # decrement inner loop counter
    jmp inner_loop                   # continue inner loop
    
inner_loop_end:
    decl %ecx                        # decrement outer loop counter
    jmp outer_loop                   # continue outer loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80
```

## Algorithm Explanation:

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Exchanges elements when they're out of order
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array access using base+offset addressing
- **Control flow**: `jmp`, `jle`, `jge` conditional jumps
- **Function calls**: `call` and `ret` instructions
- **System calls**: `int $0x80` for program termination

The algorithm sorts an array of integers in ascending order using the classic bubble sort approach.

