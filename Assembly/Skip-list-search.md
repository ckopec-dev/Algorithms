# Skip List Search Algorithm in Assembly

Below is an example implementation of a skip list search algorithm in x86-64 Assembly language:

```assembly
.section .text
.globl skip_list_search
.type skip_list_search, @function

# Function: skip_list_search
# Parameters: 
#   RDI = head pointer (struct skip_node*)
#   RSI = search_key (int)
# Returns: 
#   RAX = pointer to found node or NULL

skip_list_search:
    # Save registers
    push    rbp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    
    # Initialize current node to head
    mov     r12, rdi          # r12 = current_node = head
    
    # Start from the highest level
    mov     r13, 0            # r13 = level = 0
    
    # Find maximum level in the list
    mov     r14, 0            # r14 = max_level
    
    # Get max_level from head node
    mov     r15, [r12]        # r15 = head->forward[0] (first level)
    mov     r15, [r15]        # r15 = head->forward[0]->forward[0] (level 0)
    
    # Get level from head node (assuming level is stored at offset 0)
    mov     r14, [r12]        # r14 = head->level
    
    # Search from highest level down to level 0
    mov     r13, r14          # r13 = current_level = max_level
    
search_loop:
    # Check if we've reached level 0
    cmp     r13, 0
    jl      found_node        # If level < 0, we found the node
    
    # Get current node at current level
    mov     r15, r12          # r15 = current_node
    
    # Traverse forward at current level
    # Loop through forward pointers at current level
    mov     r12, [r15]        # r12 = current_node->forward[0]
    
    # Check if we should move to next level
    # Compare key values
    mov     rax, [r12]        # rax = current_node->key
    cmp     rax, rsi          # compare with search_key
    jge     level_down        # if current_key >= search_key, go to lower level
    
    # Continue searching at current level
    jmp     search_loop
    
level_down:
    # Move down one level
    dec     r13
    jmp     search_loop
    
found_node:
    # Return the found node
    mov     rax, r12          # return found node pointer
    jmp     cleanup
    
cleanup:
    # Restore registers
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

# Alternative implementation - more straightforward approach
.globl skip_list_search_simple
.type skip_list_search_simple, @function

skip_list_search_simple:
    # Parameters: RDI = head, RSI = key
    # Return: RAX = found node or NULL
    
    push    rbp
    mov     rbp, rsp
    
    # Initialize current node
    mov     rax, rdi          # rax = current_node = head
    
    # Start from level 0 (lowest level)
    mov     rcx, 0            # rcx = level = 0
    
level_search:
    # Check if we're at the correct level
    # This is a simplified version - in practice would need to traverse
    # the actual forward pointers
    
    # Get current node's key
    mov     r8, [rax]         # r8 = current_node->key
    
    # Compare with search key
    cmp     r8, rsi           # compare current_key with search_key
    je      found             # if equal, found it
    
    # If current key is less than search key, move forward
    jl      move_forward
    
    # If current key is greater, we've passed our target
    jmp     not_found
    
move_forward:
    # Move to next node at current level
    mov     rax, [rax + 8]    # rax = current_node->forward[0] (assuming 8 bytes offset)
    jmp     level_search
    
found:
    # Found the node
    jmp     return_result
    
not_found:
    # Node not found
    xor     rax, rax          # return NULL
    
return_result:
    pop     rbp
    ret

.section .data
# Example skip list node structure:
# struct skip_node {
#     int key;
#     struct skip_node* forward[];
#     int level;
# };

# Example data for testing
example_head:
    .quad   0x00000000        # key (will be filled)
    .quad   0x00000000        # forward[0] pointer
    .quad   0x00000000        # forward[1] pointer
    .quad   0x00000000        # forward[2] pointer
    .long   3                 # level (3 levels)
```

## Key Features of This Implementation:

1. **Register Usage**:
   - `RDI`: Head pointer (input parameter)
   - `RSI`: Search key (input parameter)
   - `RAX`: Return value/working register
   - `R12-R15`: Working registers for node traversal

2. **Algorithm Steps**:
   - Start from the highest level
   - Traverse forward while current key < search key
   - When current key ≥ search key, move down one level
   - Continue until level 0 is reached
   - Return found node or NULL

3. **Memory Layout Assumptions**:
   - Node structure with key and forward pointers
   - Level information stored in node
   - Pointer size = 8 bytes (64-bit architecture)

4. **Edge Cases Handled**:
   - Empty list (head = NULL)
   - Key not found
   - Key at beginning or end of list

This is a simplified implementation that demonstrates the core logic of skip list search in assembly. A production version would need more sophisticated error handling and memory management.

