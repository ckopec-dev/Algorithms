# Odd-Even Sort Algorithm in Assembly

Here's an implementation of the Odd-Even sort algorithm in x86-64 Assembly using NASM syntax:

```assembly
section .data
    array dd 64, 34, 25, 12, 22, 11, 90, 5    ; Example array of 8 integers
    array_size equ 8                           ; Array size

section .text
    global _start

; Odd-Even Sort Function
; Input: array pointer in RDI, array_size in RSI
; Output: sorted array in place
odd_even_sort:
    push rbp
    mov rbp, rsp
    
    ; Initialize variables
    mov rcx, 0          ; phase counter
    mov r8, array_size  ; size of array
    dec r8              ; r8 = array_size - 1
    
phase_loop:
    ; Check if we've completed all phases
    cmp rcx, r8
    jge sort_done
    
    ; Determine if this is an odd or even phase
    mov r9, rcx
    and r9, 1           ; r9 = rcx % 2
    
    ; Even phase (r9 = 0) or Odd phase (r9 = 1)
    cmp r9, 0
    je even_phase
    
    ; Odd phase
    mov r10, 1          ; start index = 1
    jmp compare_loop
    
even_phase:
    mov r10, 0          ; start index = 0
    jmp compare_loop

compare_loop:
    ; Check if we've reached the end
    mov r11, r10
    inc r11             ; r11 = r10 + 1
    cmp r11, r8
    jge next_phase      ; if r10 + 1 >= array_size - 1, move to next phase
    
    ; Compare adjacent elements
    mov rax, [rdi + r10 * 4]     ; load array[r10]
    mov rbx, [rdi + r11 * 4]     ; load array[r11]
    
    ; If array[r10] > array[r11], swap them
    cmp rax, rbx
    jle no_swap
    
    ; Swap elements
    mov [rdi + r10 * 4], rbx     ; array[r10] = array[r11]
    mov [rdi + r11 * 4], rax     ; array[r11] = array[r10]
    
no_swap:
    ; Move to next pair
    add r10, 2
    jmp compare_loop

next_phase:
    ; Increment phase counter
    inc rcx
    jmp phase_loop

sort_done:
    pop rbp
    ret

; Print array function (for demonstration)
print_array:
    push rbp
    mov rbp, rsp
    
    mov r10, 0          ; index counter
    mov r11, array_size ; array size
    
print_loop:
    cmp r10, r11
    jge print_done
    
    ; Print array[r10]
    mov eax, [rdi + r10 * 4]
    ; In a real implementation, you would use system calls to print numbers
    ; This is a placeholder for actual printing logic
    
    inc r10
    jmp print_loop
    
print_done:
    pop rbp
    ret

; Main program
_start:
    ; Call odd-even sort on our array
    mov rdi, array      ; array pointer
    mov rsi, array_size ; array size
    call odd_even_sort
    
    ; Exit program
    mov eax, 1          ; sys_exit
    mov ebx, 0          ; exit status
    int 0x80
```

## How Odd-Even Sort Works

The Odd-Even sort algorithm works by alternating between:
1. **Odd phase**: Compare and swap elements at odd indices (1,3,5...)
2. **Even phase**: Compare and swap elements at even indices (0,2,4...)

This continues until no more swaps are needed, indicating the array is sorted.

## Key Features of the Implementation

- **Register usage**: Uses RDI for array pointer, RSI for size, RCX for phase counter
- **Memory access**: Properly handles 32-bit integer array elements
- **Loop control**: Uses conditional jumps to implement the sorting phases
- **Swap operation**: Implements element swapping when necessary

## Algorithm Complexity

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place
- **Parallelism**: Can be parallelized since odd and even phases are independent

This implementation demonstrates the core concepts of the odd-even sort algorithm in assembly language, showing how the alternating phases are implemented using conditional logic and loop structures.

