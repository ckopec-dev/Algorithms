# Floyd-Warshall Algorithm in Assembly (x86-64)

Here's an implementation of the Floyd-Warshall algorithm in x86-64 assembly language:

```assembly
.section .data
    # Distance matrix (4x4 example)
    dist_matrix: .long 0, 3, 8, 999999
                 .long 999999, 0, 999999, 1
                 .long 999999, 999999, 0, 7
                 .long 999999, 999999, 999999, 0
    
    # Matrix dimensions
    matrix_size: .long 4

.section .text
    .global _start

# Floyd-Warshall algorithm implementation
# void floyd_warshall(int* dist, int size)
floyd_warshall:
    push    %rbp
    mov     %rsp, %rbp
    push    %rbx
    push    %r12
    push    %r13
    push    %r14
    push    %r15

    # Parameters:
    # %rdi = dist (pointer to matrix)
    # %rsi = size (number of vertices)

    mov     %rsi, %r12          # size in r12
    mov     %rdi, %r13          # dist pointer in r13

    # k = 0 to size-1 (outer loop)
outer_loop:
    cmp     %r12, %rax
    jge     end_outer_loop

    # i = 0 to size-1 (middle loop)
middle_loop:
    cmp     %r12, %rcx
    jge     end_middle_loop

    # j = 0 to size-1 (inner loop)
inner_loop:
    cmp     %r12, %rdx
    jge     end_inner_loop

    # Calculate matrix indices
    # dist[i][j] = dist[i][j] < dist[i][k] + dist[k][j] ? dist[i][j] : dist[i][k] + dist[k][j]
    
    # Calculate indices: offset = row * size + col
    mov     %rax, %r8           # i
    mov     %rcx, %r9           # j
    mov     %r8, %r10           # i
    imul    %r12, %r10          # i * size
    add     %r9, %r10           # i * size + j
    shl     $2, %r10            # * 4 (bytes per int)
    mov     %r10, %r11          # offset for dist[i][j]

    # Load dist[i][j]
    mov     (%r13, %r11), %r14

    # Calculate dist[i][k] and dist[k][j]
    mov     %r8, %r10           # i
    mov     %r12, %r11          # k
    imul    %r12, %r10          # i * size
    add     %r11, %r10          # i * size + k
    shl     $2, %r10            # * 4 (bytes per int)
    mov     (%r13, %r10), %r15  # dist[i][k]

    mov     %r11, %r10          # k
    mov     %r9, %r11           # j
    imul    %r12, %r10          # k * size
    add     %r11, %r10          # k * size + j
    shl     $2, %r10            # * 4 (bytes per int)
    mov     (%r13, %r10), %r10  # dist[k][j]

    # Calculate dist[i][k] + dist[k][j]
    add     %r10, %r15

    # Compare and update
    cmp     %r15, %r14
    jle     skip_update

    # Update dist[i][j] = dist[i][k] + dist[k][j]
    mov     %r15, (%r13, %r11)

skip_update:
    inc     %rdx                # j++
    jmp     inner_loop

end_inner_loop:
    inc     %rcx                # i++
    jmp     middle_loop

end_middle_loop:
    inc     %rax                # k++
    jmp     outer_loop

end_outer_loop:
    # Restore registers
    pop     %r15
    pop     %r14
    pop     %r13
    pop     %r12
    pop     %rbx
    pop     %rbp
    ret

# Main program
_start:
    # Initialize
    mov     $0, %rax            # k = 0
    mov     $0, %rcx            # i = 0
    mov     $0, %rdx            # j = 0

    # Call Floyd-Warshall
    mov     $dist_matrix, %rdi  # pointer to distance matrix
    mov     $4, %rsi            # size = 4
    call    floyd_warshall

    # Exit program
    mov     $60, %rax           # sys_exit
    mov     $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph. The assembly implementation follows this logic:

1. **Three nested loops**: 
   - Outer loop (k): intermediate vertex
   - Middle loop (i): source vertex  
   - Inner loop (j): destination vertex

2. **Key operation**: For each triple (i,j,k), update `dist[i][j]` if `dist[i][k] + dist[k][j]` is smaller

3. **Memory access**: Matrix elements are accessed using row-major indexing: `offset = i * size + j`

## Key Assembly Concepts Used

- **Register usage**: 
  - `%rax`, `%rbx`, `%rcx`, `%rdx` for loop counters
  - `%r12`-`%r15` for temporary storage
  - `%rdi`, `%rsi` for function parameters

- **Memory addressing**: 
  - `(%r13, %r11)` for indexed memory access
  - Bit shifting (`shl $2`) for byte calculations

- **Control flow**: 
  - Conditional jumps (`jge`, `jle`) for loop termination
  - Function calls and returns with proper stack management

This implementation demonstrates how the Floyd-Warshall algorithm's three nested loops translate to assembly instructions with careful register management and memory addressing.

