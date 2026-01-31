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
    movl $0, %esi                    # Initialize outer loop index
    
outer_loop:
    cmp $0, %ecx                     # Check if we've completed all passes
    je sort_done
    
    movl $0, %edi                    # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                   # Compare inner index with array size
    jge inner_loop_end
    
    # Load two adjacent elements
    movl array(,%edi,4), %eax       # Load array[i] into EAX
    movl array(,%edi,4), %ebx       # Load array[i] into EBX (for comparison)
    
    # Compare adjacent elements
    cmpl 4(%ebx), %eax              # Compare array[i] with array[i+1]
    jle no_swap                      # Jump if array[i] <= array[i+1]
    
    # Swap elements
    movl 4(%ebx), %edx              # Load array[i+1] into EDX
    movl %eax, 4(%ebx)              # array[i+1] = array[i]
    movl %edx, (%ebx)               # array[i] = array[i+1]
    
no_swap:
    incl %edi                        # Increment inner loop index
    jmp inner_loop                   # Continue inner loop
    
inner_loop_end:
    decl %ecx                        # Decrement outer loop counter
    jmp outer_loop                   # Continue outer loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80
```

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Uses register operations to exchange element values
5. **Loop Control**: Uses conditional jumps (`je`, `jge`, `jle`) for loop control

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array access using base + index + scale
- **Conditional jumps**: `je`, `jge`, `jle` for loop and condition control
- **Function calls**: `call` and `ret` for subroutine execution
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

