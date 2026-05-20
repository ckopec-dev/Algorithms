# Prim's Algorithm in Assembly Language

Here's an example implementation of Prim's algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Graph representation as adjacency matrix
    graph_matrix:
        .long 0, 2, 0, 6, 0    # Node 0
        .long 2, 0, 3, 8, 5    # Node 1
        .long 0, 3, 0, 0, 7    # Node 2
        .long 6, 8, 0, 0, 9    # Node 3
        .long 0, 5, 7, 9, 0    # Node 4
    
    # Array to track included vertices
    included:
        .long 0, 0, 0, 0, 0    # 5 vertices
    
    # Array to store key values
    key_values:
        .long 0, 0, 0, 0, 0    # 5 vertices
    
    # Number of vertices
    num_vertices: .long 5
    
    # Minimum key value
    min_key: .long 0x7FFFFFFF  # Max integer value

.section .text
    .global _start

prim_algorithm:
    # Function to implement Prim's algorithm
    # Input: graph matrix, num_vertices
    # Output: minimum spanning tree
    
    push %rbp
    mov %rsp, %rbp
    
    # Initialize variables
    movl num_vertices(%rip), %ecx          # Load number of vertices
    movl $0, %eax                          # Initialize vertex counter
    
    # Initialize key values array with infinity (0x7FFFFFFF)
    movl $0, %edi                          # Index counter
init_loop:
    cmpl %ecx, %edi                        # Compare with number of vertices
    jge init_done
    movl min_key(%rip), key_values(,%rdi,4) # Set key to infinity
    incl %edi
    jmp init_loop
init_done:
    
    # Set first vertex key to 0
    movl $0, key_values(,%rax,4)
    
    # Main Prim's algorithm loop
main_loop:
    # Find minimum key vertex not yet included
    call find_min_key
    movl %eax, %esi                        # Store minimum vertex
    
    # Mark vertex as included
    movl $1, included(,%rsi,4)
    
    # Update key values of adjacent vertices
    call update_adjacent_keys
    
    # Check if all vertices are included
    call check_all_included
    test %eax, %eax
    jz main_loop                           # Continue if not all included
    
    # Exit program
    movl $60, %eax                         # sys_exit
    movl $0, %ebx                          # exit status
    int $0x80

find_min_key:
    # Find vertex with minimum key value that is not included
    push %rbp
    mov %rsp, %rbp
    
    movl num_vertices(%rip), %ecx          # Number of vertices
    movl min_key(%rip), %edx               # Initialize min key
    movl $-1, %edi                         # Initialize min vertex index
    
    movl $0, %esi                          # Index counter
find_loop:
    cmpl %ecx, %esi
    jge find_done
    
    # Check if vertex is included
    movl included(,%rsi,4), %eax
    test %eax, %eax
    jnz find_continue                      # Skip if included
    
    # Check if key is smaller
    movl key_values(,%rsi,4), %eax
    cmpl %edx, %eax
    jge find_continue
    
    movl %eax, %edx                        # Update min key
    movl %esi, %edi                        # Update min vertex
    
find_continue:
    incl %esi
    jmp find_loop
find_done:
    movl %edi, %eax                        # Return vertex index
    pop %rbp
    ret

update_adjacent_keys:
    # Update key values of adjacent vertices
    push %rbp
    mov %rsp, %rbp
    
    movl num_vertices(%rip), %ecx          # Number of vertices
    movl $0, %esi                          # Index counter
    
update_loop:
    cmpl %ecx, %esi
    jge update_done
    
    # Check if vertex is included
    movl included(,%rsi,4), %eax
    test %eax, %eax
    jnz update_continue                    # Skip if not included
    
    # Check if there's an edge and update key
    movl graph_matrix(,%rdi,4), %eax      # Load edge weight
    test %eax, %eax
    jz update_continue                     # Skip if no edge
    
    # Update key if edge weight is smaller
    movl key_values(,%rsi,4), %ebx
    cmpl %eax, %ebx
    jle update_continue                    # Skip if not smaller
    
    movl %eax, key_values(,%rsi,4)         # Update key
    
update_continue:
    incl %esi
    jmp update_loop
update_done:
    pop %rbp
    ret

check_all_included:
    # Check if all vertices are included
    push %rbp
    mov %rsp, %rbp
    
    movl num_vertices(%rip), %ecx
    movl $0, %esi
    movl $1, %eax                          # Assume all included
    
check_loop:
    cmpl %ecx, %esi
    jge check_done
    
    movl included(,%rsi,4), %eax
    test %eax, %eax
    jz check_not_done                      # Not all included
    
    incl %esi
    jmp check_loop
check_not_done:
    movl $0, %eax                          # Not all included
check_done:
    pop %rbp
    ret

# Main program entry point
_start:
    call prim_algorithm
    
    # Exit program
    movl $60, %eax                         # sys_exit
    movl $0, %ebx                          # exit status
    int $0x80
```

## Explanation

This assembly implementation of Prim's algorithm demonstrates:

1. **Data Structures**:
   - Adjacency matrix representation of the graph
   - Arrays to track included vertices and key values
   - Memory allocation for graph data

2. **Key Components**:
   - `prim_algorithm`: Main function implementing Prim's algorithm
   - `find_min_key`: Finds vertex with minimum key value
   - `update_adjacent_keys`: Updates key values of adjacent vertices
   - `check_all_included`: Verifies if all vertices are processed

3. **Algorithm Steps**:
   - Initialize all vertices with infinite key values
   - Set first vertex key to 0
   - Repeatedly find minimum key vertex not yet included
   - Update key values of adjacent vertices
   - Continue until all vertices are included

4. **Assembly Features Used**:
   - Register usage for efficient computation
   - Memory addressing for array operations
   - Conditional jumps for control flow
   - System calls for program termination

This implementation shows how Prim's algorithm can be translated into low-level assembly code while maintaining the core algorithmic logic.

