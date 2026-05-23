# Union-Find Algorithm in Assembly

Here's an example implementation of the Union-Find (Disjoint Set Union) algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Array to store parent pointers
    parent: .space 1000    # Space for 1000 elements
    rank: .space 1000      # Space for 1000 rank values

.section .text
    .global union_find_init
    .global union_find_find
    .global union_find_union

# Initialize Union-Find structure
# Input: n (number of elements)
# Output: None (modifies global arrays)
union_find_init:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rax        # n in rax
    
    # Initialize parent array
    movq $0, %rcx          # i = 0
init_loop:
    cmpq %rax, %rcx        # compare i with n
    jge init_done          # if i >= n, done
    
    # parent[i] = i
    movq %rcx, %rdx
    movq %rcx, parent(,%rdx,8)  # parent[i] = i
    
    # rank[i] = 0
    movq $0, rank(,%rdx,8)      # rank[i] = 0
    
    incq %rcx              # i++
    jmp init_loop
init_done:
    popq %rbp
    ret

# Find operation with path compression
# Input: x (element index)
# Output: root element (index)
union_find_find:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rax        # x in rax
    
    # Store original x
    movq %rax, %r8
    
    # Find root with path compression
find_loop:
    # Check if x is root (parent[x] == x)
    movq parent(,%rax,8), %rdx  # parent[x]
    cmpq %rax, %rdx             # parent[x] == x?
    je find_done                # if yes, we found root
    
    # Path compression: make x point to grandparent
    movq parent(,%rax,8), %rdx  # parent[x]
    movq parent(,%rdx,8), %r9   # parent[parent[x]]
    movq %r9, parent(,%rax,8)   # parent[x] = parent[parent[x]]
    
    # Move to grandparent
    movq %r9, %rax              # x = parent[x]
    jmp find_loop
    
find_done:
    # Return root
    movq %rax, %rax
    popq %rbp
    ret

# Union operation with union by rank
# Input: x, y (element indices)
# Output: None
union_find_union:
    pushq %rbp
    movq %rsp, %rbp
    
    # Find roots of both elements
    movq %rdi, %rax        # x in rax
    call union_find_find
    movq %rax, %r8         # root_x in r8
    
    movq %rsi, %rax        # y in rax
    call union_find_find
    movq %rax, %r9         # root_y in r9
    
    # If roots are same, already in same set
    cmpq %r8, %r9
    je union_done
    
    # Union by rank: attach smaller tree under root of larger tree
    movq rank(,%r8,8), %rax  # rank[root_x]
    movq rank(,%r9,8), %rcx  # rank[root_y]
    
    # If rank[root_x] < rank[root_y]
    cmpq %rax, %rcx
    jg union_swap
    
    # If rank[root_x] > rank[root_y]
    cmpq %rax, %rcx
    jl union_no_swap
    
    # If ranks are equal, increment rank of root_x
    incq rank(,%r8,8)
    jmp union_set_parent
    
union_swap:
    # Swap roots (make root_y the parent of root_x)
    movq %r8, %r10
    movq %r9, %r8
    movq %r10, %r9
    
union_no_swap:
    # Set parent of root_y to root_x
union_set_parent:
    movq %r8, parent(,%r9,8)  # parent[root_y] = root_x
    
union_done:
    popq %rbp
    ret

# Example usage function
# Demonstrates basic operations
example_usage:
    pushq %rbp
    movq %rsp, %rbp
    
    # Initialize with 5 elements
    movq $5, %rdi
    call union_find_init
    
    # Union elements 0 and 1
    movq $0, %rdi
    movq $1, %rsi
    call union_find_union
    
    # Union elements 2 and 3
    movq $2, %rdi
    movq $3, %rsi
    call union_find_union
    
    # Union elements 1 and 2 (connecting two sets)
    movq $1, %rdi
    movq $2, %rsi
    call union_find_union
    
    # Find root of element 0
    movq $0, %rdi
    call union_find_find
    
    # Result is stored in rax
    # Should be 0 (since 0 is connected to 1, 2, 3)
    
    popq %rbp
    ret
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `parent`: Array storing parent pointers for each element
- `rank`: Array storing rank values for union by rank optimization

### 2. **Core Operations**

**Initialization (`union_find_init`)**:
- Sets each element to be its own parent
- Initializes all ranks to 0

**Find Operation (`union_find_find`)**:
- Uses path compression to optimize future queries
- Returns the root element of the set

**Union Operation (`union_find_union`)**:
- Uses union by rank to keep trees balanced
- Combines two sets into one

### 3. **Optimizations**
- **Path Compression**: Makes future find operations faster
- **Union by Rank**: Keeps the tree depth minimal
- **Efficient Memory Access**: Uses indexed addressing for arrays

### 4. **Usage Example**
```assembly
# Initialize 5 elements
movq $5, %rdi
call union_find_init

# Union elements 0 and 1
movq $0, %rdi
movq $1, %rsi
call union_find_union

# Find root of element 0
movq $0, %rdi
call union_find_find
# Result in %rax
```

This implementation demonstrates the fundamental concepts of Union-Find in assembly while maintaining the algorithm's efficiency characteristics.

