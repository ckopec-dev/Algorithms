# Bellman-Ford Algorithm in Assembly (x86-64 NASM)

Here's a complete implementation of the Bellman-Ford algorithm in x86-64 assembly language:

```assembly
section .data
    ; Graph representation: edges array
    edges dd 0, 1, 4        ; edge from vertex 0 to vertex 1 with weight 4
    edges dd 0, 2, 2        ; edge from vertex 0 to vertex 2 with weight 2
    edges dd 1, 2, -1       ; edge from vertex 1 to vertex 2 with weight -1
    edges dd 1, 3, 5        ; edge from vertex 1 to vertex 3 with weight 5
    edges dd 2, 3, 8        ; edge from vertex 2 to vertex 3 with weight 8
    edges dd 2, 4, 10       ; edge from vertex 2 to vertex 4 with weight 10
    edges dd 3, 4, -7       ; edge from vertex 3 to vertex 4 with weight -7
    
    num_edges equ 7         ; number of edges
    num_vertices equ 5      ; number of vertices
    
    ; Distance array initialization
    distances dd 0, 2147483647, 2147483647, 2147483647, 2147483647
                            ; initialize with infinity (0x7FFFFFFF)
    
    ; Source vertex
    source equ 0
    
    ; Output messages
    msg_result db "Shortest distances from vertex 0:", 10, 0
    msg_newline db 10, 0
    msg_negative_cycle db "Negative cycle detected!", 10, 0

section .text
    global _start

bellman_ford:
    ; Input: edges array, num_edges, num_vertices, source vertex
    ; Output: distances array updated with shortest paths
    
    push rbp
    mov rbp, rsp
    
    ; Initialize distances array
    mov r12, num_vertices   ; r12 = number of vertices
    mov r13, 0              ; r13 = current vertex index
    
init_loop:
    cmp r13, r12
    jge init_done
    
    ; Set distance to source as 0
    cmp r13, source
    jne init_continue
    
    mov dword [distances + r13*4], 0
    jmp init_continue
    
init_continue:
    ; For all other vertices, set to infinity
    mov eax, 2147483647    ; 0x7FFFFFFF (infinity)
    mov [distances + r13*4], eax
    
    inc r13
    jmp init_loop
    
init_done:
    ; Main Bellman-Ford algorithm
    ; Outer loop: relax edges for (V-1) iterations
    mov r12, num_vertices   ; r12 = number of vertices
    dec r12                 ; r12 = V-1
    mov r14, 0              ; r14 = iteration counter
    
outer_loop:
    cmp r14, r12
    jge bellman_done
    
    ; Inner loop: process all edges
    mov r15, 0              ; r15 = edge index
    mov r13, 0              ; r13 = current vertex
    
inner_loop:
    cmp r15, num_edges
    jge inner_done
    
    ; Load edge data
    mov eax, [edges + r15*12]     ; src vertex (first 4 bytes)
    mov ebx, [edges + r15*12 + 4] ; dst vertex (next 4 bytes)
    mov ecx, [edges + r15*12 + 8] ; weight (last 4 bytes)
    
    ; Get current distance to source
    mov edx, [distances + eax*4]
    
    ; Check if we can relax the edge
    cmp edx, 2147483647           ; check if src is infinity
    je skip_relax
    
    ; Calculate new distance: dist[src] + weight
    add edx, ecx                  ; edx = dist[src] + weight
    
    ; Get current distance to destination
    mov esi, [distances + ebx*4]
    
    ; Relax edge if new distance is smaller
    cmp edx, esi
    jge skip_relax
    
    ; Update distance
    mov [distances + ebx*4], edx
    
skip_relax:
    inc r15
    jmp inner_loop
    
inner_done:
    inc r14
    jmp outer_loop
    
bellman_done:
    ; Check for negative cycles
    ; Run one more iteration to detect negative cycles
    mov r15, 0              ; edge index
    
check_negative_cycle:
    cmp r15, num_edges
    jge no_negative_cycle
    
    ; Load edge data
    mov eax, [edges + r15*12]     ; src vertex
    mov ebx, [edges + r15*12 + 4] ; dst vertex
    mov ecx, [edges + r15*12 + 8] ; weight
    
    ; Get current distance to source
    mov edx, [distances + eax*4]
    
    ; Check if we can relax the edge
    cmp edx, 2147483647           ; check if src is infinity
    je skip_cycle_check
    
    ; Calculate new distance: dist[src] + weight
    add edx, ecx                  ; edx = dist[src] + weight
    
    ; Get current distance to destination
    mov esi, [distances + ebx*4]
    
    ; If we can still relax, there's a negative cycle
    cmp edx, esi
    jge skip_cycle_check
    
    ; Negative cycle detected
    mov rax, 1                  ; return error code
    jmp end_bellman
    
skip_cycle_check:
    inc r15
    jmp check_negative_cycle
    
no_negative_cycle:
    mov rax, 0                  ; return success code
    
end_bellman:
    pop rbp
    ret

; Print results function (simplified)
print_results:
    push rbp
    mov rbp, rsp
    
    ; Print result message
    mov rdi, msg_result
    call print_string
    
    ; Print distances for each vertex
    mov r12, 0                  ; vertex counter
    mov r13, 0                  ; character counter
    
print_loop:
    cmp r12, num_vertices
    jge print_done
    
    ; Print vertex number
    mov eax, r12
    call print_int
    
    ; Print colon and space
    mov rdi, 58                 ; ':'
    call print_char
    mov rdi, 32                 ; ' '
    call print_char
    
    ; Print distance
    mov eax, [distances + r12*4]
    call print_int
    
    ; Print newline
    mov rdi, 10
    call print_char
    
    inc r12
    jmp print_loop
    
print_done:
    pop rbp
    ret

; Helper functions (simplified)
print_string:
    ; This would be implemented to output a string
    ; Simplified version for demonstration
    ret

print_int:
    ; This would be implemented to convert integer to string and print
    ; Simplified version for demonstration
    ret

print_char:
    ; This would be implemented to output a character
    ; Simplified version for demonstration
    ret

; Main program entry point
_start:
    ; Call Bellman-Ford algorithm
    call bellman_ford
    
    ; Check return value
    cmp rax, 0
    je normal_exit
    
    ; Print negative cycle message
    mov rdi, msg_negative_cycle
    call print_string
    
normal_exit:
    ; Exit program
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `edges`: Array storing edges as [src, dst, weight]
- `distances`: Array storing shortest distances from source
- `num_edges` and `num_vertices`: Graph parameters

### 2. **Algorithm Steps**
1. **Initialization**: Set all distances to infinity except source (0)
2. **Relaxation**: For V-1 iterations, relax all edges
3. **Negative Cycle Detection**: One final iteration to detect negative cycles

### 3. **Assembly-Specific Features**
- Uses x86-64 registers efficiently
- Proper stack management with `push`/`pop`
- Memory addressing for arrays and structures
- Conditional jumps for loop control
- Function calling convention

### 4. **Key Instructions Used**
- `mov`: Data movement
- `add`: Arithmetic operations
- `cmp`: Comparison operations
- `jmp`: Unconditional jumps
- `je/jne/jg/jl`: Conditional jumps
- `call/ret`: Function calls

This implementation demonstrates how the Bellman-Ford algorithm can be translated into low-level assembly code while maintaining the same logical structure and functionality as a high-level language version.

