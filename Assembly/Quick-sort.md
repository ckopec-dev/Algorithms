# Quick Sort Algorithm in Assembly (x86-64 NASM Syntax)

```assembly
section .text
global _start

; QuickSort function
; Parameters: array pointer in RDI, low index in RSI, high index in RDX
; Returns: sorted array in place
quicksort:
    ; Base case: if low >= high, return
    cmp rsi, rdx
    jge quicksort_end
    
    ; Partition the array and get pivot index
    push rdi        ; Save array pointer
    push rsi        ; Save low index
    push rdx        ; Save high index
    call partition
    pop rdx         ; Restore high index
    pop rsi         ; Restore low index
    pop rdi         ; Restore array pointer
    
    ; Recursively sort elements before partition
    mov rcx, rax    ; pivot index to RCX
    dec rcx         ; pivot - 1
    push rdi        ; Save array pointer
    push rsi        ; Save low index
    push rcx        ; Save high index
    call quicksort
    pop rcx         ; Restore high index
    pop rsi         ; Restore low index
    pop rdi         ; Restore array pointer
    
    ; Recursively sort elements after partition
    inc rax         ; pivot + 1
    push rdi        ; Save array pointer
    push rax        ; Save low index
    push rdx        ; Save high index
    call quicksort
    pop rdx         ; Restore high index
    pop rax         ; Restore low index
    pop rdi         ; Restore array pointer
    
quicksort_end:
    ret

; Partition function
; Parameters: array pointer in RDI, low index in RSI, high index in RDX
; Returns: pivot index in RAX
partition:
    ; Choose last element as pivot
    mov rax, rdx            ; pivot index = high
    mov rbx, [rdi + rax*8] ; pivot value = array[high]
    
    ; Initialize i to low - 1
    mov rcx, rsi
    dec rcx                 ; i = low - 1
    
    ; Iterate through array from low to high-1
partition_loop:
    cmp rsi, rdx
    jge partition_end
    
    ; If current element <= pivot
    mov r8, [rdi + rsi*8]
    cmp r8, rbx
    jg skip_swap
    
    ; Swap array[i+1] with array[si]
    inc rcx                 ; i = i + 1
    mov r9, [rdi + rcx*8]   ; temp = array[i]
    mov [rdi + rcx*8], r8   ; array[i] = array[si]
    mov [rdi + rsi*8], r9   ; array[si] = temp
    
skip_swap:
    inc rsi                 ; si = si + 1
    jmp partition_loop
    
partition_end:
    ; Swap pivot to its correct position
    inc rcx                 ; i = i + 1
    mov r8, [rdi + rcx*8]   ; temp = array[i]
    mov [rdi + rcx*8], rbx  ; array[i] = pivot
    mov [rdi + rdx*8], r8   ; array[high] = temp
    
    mov rax, rcx            ; return pivot index
    ret

; Example usage with test array
section .data
    test_array dd 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
    array_size equ 11

section .bss
    ; Buffer for output
    output_buffer resb 256

section .text
    global main
main:
    ; Initialize stack
    push rbp
    mov rbp, rsp
    
    ; Call quicksort on test array
    lea rdi, [test_array]   ; array pointer
    mov rsi, 0              ; low = 0
    mov rdx, array_size - 1 ; high = array_size - 1
    call quicksort
    
    ; Clean up and exit
    mov rsp, rbp
    pop rbp
    ret

; Exit system call
exit:
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall
```

## Key Features of this Implementation:

1. **Recursive Structure**: Uses stack-based recursion for partitioning
2. **In-place Sorting**: Modifies the original array without extra space
3. **Lomuto Partition Scheme**: Uses the last element as pivot
4. **Parameter Passing**: Uses standard x86-64 calling convention
5. **Register Usage**: 
   - RDI: Array pointer
   - RSI: Low index
   - RDX: High index
   - RAX: Return value/pivot index

## Algorithm Complexity:
- **Time Complexity**: O(n log n) average case, O(n²) worst case
- **Space Complexity**: O(log n) due to recursion stack
- **Sorting Method**: In-place, unstable

This implementation demonstrates the core quicksort algorithm in assembly language, showing how the partitioning and recursive calls are handled at the machine level.

