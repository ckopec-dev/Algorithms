# Run-Length Encoding (RLE) in Assembly

Here's an example implementation of Run-Length Encoding algorithm in x86-64 Assembly using NASM syntax:

```assembly
section .data
    input_string db "AAAABBBCCDAA", 0
    output_buffer db 50 dup(0)
    input_len equ 10

section .text
    global _start

; RLE function
; Input: esi = pointer to input string
;        ecx = length of input string
; Output: edi = pointer to encoded string
rle_encode:
    push rsi            ; Save input pointer
    push rcx            ; Save length
    push rdi            ; Save output pointer
    
    mov rdi, output_buffer  ; Set output pointer
    mov rsi, input_string   ; Set input pointer
    mov rcx, input_len      ; Set length
    
    xor rax, rax            ; Clear rax for counting
    mov al, [rsi]           ; Get first character
    inc rsi                 ; Move to next character
    dec rcx                 ; Decrement counter
    
    mov bl, 1               ; Initialize counter for current character
    
rle_loop:
    ; Check if we've reached end of string
    cmp rcx, 0
    jz rle_finish
    
    ; Compare current character with next character
    cmp al, [rsi]
    jne rle_store
    
    ; Characters are same, increment counter
    inc bl
    inc rsi
    dec rcx
    jmp rle_loop
    
rle_store:
    ; Store count (in ASCII)
    mov [rdi], bl
    inc rdi
    
    ; Store character
    mov [rdi], al
    inc rdi
    
    ; Reset for next character
    mov al, [rsi]
    inc rsi
    dec rcx
    mov bl, 1
    
    jmp rle_loop
    
rle_finish:
    ; Store final count and character
    mov [rdi], bl
    inc rdi
    mov [rdi], al
    inc rdi
    
    ; Null terminate output
    mov byte [rdi], 0
    
    ; Restore registers
    pop rdi
    pop rcx
    pop rsi
    
    ret

; Main program
_start:
    ; Call RLE encoding function
    mov rsi, input_string
    mov rcx, input_len
    mov rdi, output_buffer
    call rle_encode
    
    ; Exit program
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall
```

## Example Output

For input string `"AAAABBBCCDAA"`:
- **Input**: `AAAABBBCCDAA` (10 characters)
- **Output**: `4A3B2C1D2A` 
  - 4 A's, 3 B's, 2 C's, 1 D, 2 A's

## Algorithm Explanation

1. **Initialization**: Start with first character and counter set to 1
2. **Loop**: Compare current character with next character
3. **Same Character**: Increment counter and continue
4. **Different Character**: Store counter and character, reset counter
5. **End**: Store final counter and character

## Key Assembly Concepts Used

- **Register usage**: RSI (input pointer), RDI (output pointer), RCX (length counter)
- **Memory operations**: Loading/storing bytes from memory
- **Conditional jumps**: JZ, JNE for flow control
- **String handling**: Character-by-character processing
- **Stack operations**: Push/pop for register preservation

This implementation demonstrates how RLE works at the assembly level, showing the fundamental operations needed for run-length encoding.

