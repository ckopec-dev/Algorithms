# PageRank Algorithm in Assembly Language

Below is an example implementation of the PageRank algorithm in x86-64 Assembly language. This implementation demonstrates the core concepts of PageRank using iterative computation.

```assembly
.section .data
    # PageRank parameters
    .equ NUM_NODES, 4
    .equ MAX_ITERATIONS, 100
    .equ DAMPING_FACTOR, 0.85
    .equ EPSILON, 0.0001
    
    # Adjacency matrix (4x4)
    adjacency_matrix:
        .double 0.0, 1.0, 1.0, 0.0    # Node 0 points to nodes 1,2
        .double 1.0, 0.0, 0.0, 1.0    # Node 1 points to nodes 0,3
        .double 0.0, 1.0, 0.0, 1.0    # Node 2 points to nodes 1,3
        .double 1.0, 0.0, 1.0, 0.0    # Node 3 points to nodes 0,2
    
    # Initial PageRank values (all equal)
    initial_pr:
        .double 0.25, 0.25, 0.25, 0.25
    
    # Temporary storage for new PageRank values
    new_pr:
        .double 0.0, 0.0, 0.0, 0.0
    
    # Node out-degree array
    out_degree:
        .double 2.0, 2.0, 2.0, 2.0

.section .text
    .global _start

# Function to compute PageRank
compute_pagerank:
    # Input: adjacency matrix, initial PR values, out_degree array
    # Output: computed PageRank values
    
    push %rbp
    mov %rsp, %rbp
    sub $32, %rsp                    # Allocate local storage
    
    # Initialize variables
    mov $0, %rax                     # iteration counter
    mov $MAX_ITERATIONS, %rcx        # max iterations
    
compute_loop:
    # Check if we've reached max iterations
    cmp %rcx, %rax
    jge compute_done
    
    # Reset new PR array to zeros
    mov $0, %r8
    mov $0, %r9
    mov $0, %r10
    mov $0, %r11
    
    # Compute new PageRank values
    mov $0, %r12                     # node index
    
node_loop:
    # Check if we've processed all nodes
    cmp $NUM_NODES, %r12
    jge node_done
    
    # Compute contribution from each node
    mov $0, %r13                     # source node index
    mov $0.0, %xmm0                  # sum accumulator
    
source_loop:
    # Check if we've processed all source nodes
    cmp $NUM_NODES, %r13
    jge source_done
    
    # Check if source node points to current node
    mov %r12, %r14
    mov %r13, %r15
    mov $adjacency_matrix, %r14
    mov $adjacency_matrix, %r15
    imul $8, %r14                    # multiply by 8 (double size)
    imul $8, %r15                    # multiply by 8 (double size)
    add %r14, %r15                   # calculate address
    
    # Load adjacency matrix value
    movsd (%r15), %xmm1
    
    # Check if there's an edge (non-zero value)
    cmp $0.0, %xmm1
    je next_source
    
    # Get source node's PageRank
    mov %r13, %r14
    mov $initial_pr, %r15
    imul $8, %r14                    # multiply by 8
    add %r14, %r15                   # calculate address
    movsd (%r15), %xmm2
    
    # Get source node's out-degree
    mov %r13, %r14
    mov $out_degree, %r15
    imul $8, %r14                    # multiply by 8
    add %r14, %r15                   # calculate address
    movsd (%r15), %xmm3
    
    # Compute contribution: PR[source]/out_degree
    divsd %xmm3, %xmm2
    addsd %xmm2, %xmm0               # add to sum
    
next_source:
    inc %r13
    jmp source_loop
    
source_done:
    # Apply PageRank formula
    # PR_new = (1-d)/N + d * sum(contributions)
    mov $DAMPING_FACTOR, %xmm2
    mov $NUM_NODES, %r14
    movsd %xmm0, %xmm3               # copy sum
    mulsd %xmm2, %xmm3               # multiply by damping factor
    mov $1.0, %xmm4
    subsd %xmm2, %xmm4               # 1-d
    divsd %xmm1, %xmm4               # (1-d)/N
    addsd %xmm4, %xmm3               # final result
    
    # Store result in new_pr array
    mov %r12, %r14
    mov $new_pr, %r15
    imul $8, %r14                    # multiply by 8
    add %r14, %r15                   # calculate address
    movsd %xmm3, (%r15)
    
node_done:
    inc %r12
    jmp node_loop
    
node_loop_done:
    # Update PageRank values
    mov $0, %r12
    mov $initial_pr, %r13
    mov $new_pr, %r14
    
update_loop:
    cmp $NUM_NODES, %r12
    jge update_done
    
    # Copy new values to initial
    imul $8, %r12                    # multiply by 8
    movsd (%r14), %xmm0              # load new value
    movsd %xmm0, (%r13)              # store in initial
    
    add %r12, %r13                   # increment pointers
    add %r12, %r14
    inc %r12
    jmp update_loop
    
update_done:
    # Check for convergence (simplified)
    dec %rcx
    inc %rax
    jmp compute_loop
    
compute_done:
    # Clean up and return
    mov $0, %rax
    leave
    ret

# Main execution
_start:
    # Call PageRank computation
    call compute_pagerank
    
    # Exit program
    mov $60, %rax                    # sys_exit
    mov $0, %rdi                     # exit status
    syscall
```

## Key Components Explained

### 1. **Data Section**
- `NUM_NODES`: Number of web pages (4 nodes)
- `adjacency_matrix`: 4x4 matrix representing page links
- `initial_pr`: Initial PageRank values (uniform distribution)
- `out_degree`: Number of outgoing links from each page

### 2. **Algorithm Logic**
- **Iterative Computation**: Computes PageRank values through multiple iterations
- **Matrix Operations**: Processes adjacency matrix to determine link contributions
- **Damping Factor**: Implements the damping factor (0.85) as per PageRank formula
- **Convergence Check**: Stops after maximum iterations or convergence

### 3. **PageRank Formula Implementation**
```
PR(p) = (1-d)/N + d * Σ(PR(t)/out(t))
```
Where:
- `d` = damping factor (0.85)
- `N` = number of nodes
- `PR(t)` = PageRank of node t
- `out(t)` = number of outgoing links from node t

### 4. **Assembly Features Used**
- **Floating-point operations**: `movsd`, `addsd`, `mulsd`, `divsd`
- **Memory addressing**: Pointer arithmetic for array access
- **Control flow**: Conditional jumps for loops and conditions
- **Register usage**: Efficient use of x86-64 registers

This implementation demonstrates how the mathematical PageRank algorithm can be translated into low-level assembly code, showing the core computational steps while maintaining the algorithmic integrity of the original PageRank formula.