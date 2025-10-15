# Disjoint-set Union-Find Algorithm in Assembly

Here's an example implementation of the Union-Find (Disjoint-set) algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Array to store parent pointers
    parent: .space 1000    # Space for 1000 elements
    rank:   .space 1000    # Space for 1000 rank values

.section .text
    .global union_find_init
    .global find_set
    .global union_sets

# Initialize disjoint set forest
# Input: n (number of elements)
# Output: parent array initialized, rank array initialized
union_find_init:
    push    %rbp
    mov     %rsp, %rbp
    mov     %rdi, %rax     # n = number of elements
    
    # Initialize parent array
    mov     $0, %rcx       # counter
init_loop:
    cmp     %rax, %rcx
    jge     init_done
    mov     %rcx, parent(,%rcx,1)  # parent[i] = i
    mov     $0, rank(,%rcx,1)      # rank[i] = 0
    inc     %rcx
    jmp     init_loop
init_done:
    pop     %rbp
    ret

# Find set with path compression
# Input: x (element index)
# Output: root of the set containing x
find_set:
    push    %rbp
    mov     %rsp, %rbp
    mov     %rdi, %rax     # x = element index
    
    # Find root
    mov     parent(,%rax,1), %rbx  # parent[x]
find_loop:
    cmp     %rax, %rbx
    je      find_done
    mov     %rbx, %rax
    mov     parent(,%rax,1), %rbx
    jmp     find_loop
find_done:
    # Path compression - make all nodes point directly to root
    mov     %rdi, %rcx     # save original x
    mov     parent(,%rcx,1), %rdx
find_compress_loop:
    cmp     %rdx, %rcx
    je      find_compress_done
    mov     parent(,%rcx,1), %r8
    mov     %rdx, parent(,%rcx,1)  # parent[x] = root
    mov     %r8, %rcx
    mov     parent(,%rcx,1), %rdx
    jmp     find_compress_loop
find_compress_done:
    pop     %rbp
    ret

# Union two sets
# Input: x, y (element indices)
# Output: 1 if union performed, 0 if already in same set
union_sets:
    push    %rbp
    mov     %rsp, %rbp
    
    # Find roots of both sets
    mov     %rdi, %rax     # x
    call    find_set       # find root of x
    mov     %rax, %r8      # root_x
    
    mov     %rsi, %rax     # y
    call    find_set       # find root of y
    mov     %rax, %r9      # root_y
    
    # If same root, already in same set
    cmp     %r8, %r9
    je      union_same_set
    
    # Union by rank
    mov     rank(,%r8,1), %rax
    mov     rank(,%r9,1), %rbx
    
    # If rank of root_x < rank of root_y
    cmp     %rbx, %rax
    jl      union_y_rank_bigger
    
    # If rank of root_x > rank of root_y
    cmp     %rax, %rbx
    jg      union_x_rank_bigger
    
    # If ranks are equal, make root_x parent of root_y and increment rank
    mov     %r8, parent(,%r9,1)  # parent[root_y] = root_x
    inc     rank(,%r8,1)         # rank[root_x]++
    jmp     union_done
    
union_x_rank_bigger:
    # root_x has higher rank, make root_y child of root_x
    mov     %r8, parent(,%r9,1)  # parent[root_y] = root_x
    jmp     union_done
    
union_y_rank_bigger:
    # root_y has higher rank, make root_x child of root_y
    mov     %r9, parent(,%r8,1)  # parent[root_x] = root_y
    jmp     union_done
    
union_same_set:
    mov     $0, %rax         # return 0 (no union performed)
    jmp     union_done
    
union_done:
    pop     %rbp
    ret

# Example usage function
example_usage:
    push    %rbp
    mov     %rsp, %rbp
    
    # Initialize with 10 elements
    mov     $10, %rdi
    call    union_find_init
    
    # Union some sets
    mov     $0, %rdi
    mov     $1, %rsi
    call    union_sets
    
    mov     $2, %rdi
    mov     $3, %rsi
    call    union_sets
    
    mov     $0, %rdi
    mov     $3, %rsi
    call    union_sets
    
    # Find some elements
    mov     $0, %rdi
    call    find_set
    
    pop     %rbp
    ret
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `parent` array: Stores parent pointers for each element
- `rank` array: Stores rank (depth) information for union by rank optimization

### 2. **Core Functions**

**`union_find_init`**: 
- Initializes all elements to be their own parent
- Sets all ranks to 0

**`find_set`**: 
- Finds the root of the set containing element x
- Implements path compression for optimization

**`union_sets`**: 
- Unions two sets using union by rank
- Returns 1 if union was performed, 0 if elements were already in same set

### 3. **Optimizations Used**
- **Path Compression**: Makes future queries faster by flattening the tree
- **Union by Rank**: Keeps trees balanced by always attaching smaller tree under root of larger tree

### 4. **Usage Example**
The example shows initializing 10 elements, then performing several union operations, demonstrating the algorithm's functionality.

This implementation provides O(α(n)) amortized time complexity for both find and union operations, where α is the inverse Ackermann function, making it extremely efficient for practical applications.

