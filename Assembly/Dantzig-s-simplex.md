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
    
    movl array_size(%ebp), %ecx      # Load array size into ecx (outer loop counter)
    movl $0, %esi                    # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                     # Check if we've completed all passes
    je sort_done
    
    movl $0, %edi                    # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                   # Compare inner counter with array size
    jge inner_done
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax       # Load array[i] into eax
    movl array(,%edi,4), %ebx       # Load array[i] into ebx (copy)
    addl $4, %ebx                   # Load array[i+1] into ebx
    movl (%ebx), %ebx               # Get value of array[i+1]
    
    cmp %ebx, %eax                  # Compare array[i] with array[i+1]
    jle no_swap                     # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)       # array[i] = array[i+1]
    movl %eax, array(,%edi,4)       # array[i+1] = array[i]
    
no_swap:
    incl %edi                        # Increment inner counter
    jmp inner_loop
    
inner_done:
    decl %ecx                        # Decrement outer counter
    jmp outer_loop
    
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

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange array elements

## Key Assembly Concepts Demonstrated:

- **Registers**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`, `%esp`
- **Memory Operations**: Loading from and storing to memory addresses
- **Conditional Jumps**: `je`, `jg`, `jle`, `jmp` for control flow
- **Function Calls**: Stack management with `push`/`pop` and `call`/`ret`
- **Data Section**: Declaring initialized data for the array

This algorithm has O(n²) time complexity, which is typical for bubble sort regardless of implementation language.

