# Shamir's Secret Sharing in Assembly

Here's an implementation of Shamir's Secret Sharing algorithm in x86-64 Assembly using NASM syntax:

```assembly
section .data
    ; Secret to be shared
    secret dd 0x12345678        ; 32-bit secret value
    num_shares dd 5            ; Number of shares to generate
    threshold dd 3             ; Minimum shares needed to reconstruct
    prime dd 0xFFFFFFFF        ; Large prime number (2^32 - 1)
    
    ; Arrays for storing coefficients and shares
    coefficients dd 5 dup(0)   ; Coefficients for polynomial
    shares dd 5 dup(0)         ; Generated shares

section .text
    global _start

; Function to perform modular multiplication
; Input: eax = a, ebx = b, ecx = modulus
; Output: eax = (a * b) mod modulus
mod_multiply:
    push rax
    push rbx
    push rcx
    push rdx
    
    ; Multiply a and b
    imul eax, ebx
    
    ; Perform modulo operation
    mov ebx, ecx
    xor edx, edx
    div ebx
    
    ; Result in eax
    mov eax, eax
    
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

; Function to evaluate polynomial using Horner's method
; Input: eax = x coordinate, ebx = coefficients array, ecx = degree
; Output: eax = polynomial value
evaluate_polynomial:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    
    mov esi, [ebx]             ; Load first coefficient (constant term)
    mov edx, ecx               ; Load degree
    
    ; Horner's method: start with constant term
    dec edx                    ; Decrease degree
    cmp edx, 0
    jz done_eval
    
    ; Loop through remaining coefficients
eval_loop:
    ; Multiply current result by x
    imul esi, eax              ; result = result * x
    
    ; Add next coefficient
    add esi, [ebx + 4*edx]     ; Add coefficient at index edx
    
    ; Modulo operation
    mov ebx, [prime]
    xor edx, edx
    div ebx
    mov esi, eax
    
    dec edx
    cmp edx, 0
    jnz eval_loop
    
done_eval:
    mov eax, esi
    
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

; Main Shamir's Secret Sharing function
shamir_share:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    
    ; Initialize coefficients array
    mov eax, [secret]          ; First coefficient is the secret
    mov [coefficients], eax
    
    ; Generate random coefficients for polynomial (excluding constant term)
    mov ecx, [num_shares]
    dec ecx                    ; We already set the first coefficient
    mov esi, 1
    
gen_coeffs_loop:
    ; Generate random coefficient (simplified - in practice use proper RNG)
    mov eax, esi
    add eax, 0x10000000        ; Simple pseudo-random
    mov [coefficients + 4*esi], eax
    
    inc esi
    dec ecx
    jnz gen_coeffs_loop
    
    ; Generate shares
    mov ecx, [num_shares]
    mov esi, 1
    
gen_shares_loop:
    ; Calculate share for x = esi
    mov eax, esi               ; x coordinate
    mov ebx, coefficients      ; coefficients array
    mov edx, [num_shares]
    dec edx                    ; degree of polynomial
    
    call evaluate_polynomial
    
    ; Store result as share
    mov [shares + 4*esi], eax
    
    inc esi
    dec ecx
    jnz gen_shares_loop
    
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

; Function to reconstruct secret using Lagrange interpolation
; Input: shares array, number of shares, threshold
reconstruct_secret:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    
    ; For simplicity, we'll use a basic reconstruction with threshold shares
    ; In practice, this would implement Lagrange interpolation
    mov eax, [shares]          ; Get first share (should be secret)
    
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

; Main program entry point
_start:
    ; Initialize
    mov eax, [secret]
    mov ebx, [num_shares]
    mov ecx, [threshold]
    
    ; Generate shares
    call shamir_share
    
    ; Reconstruct secret (using first share for demonstration)
    call reconstruct_secret
    
    ; Exit program
    mov eax, 1                 ; sys_exit
    mov ebx, 0                 ; exit status
    int 0x80
```

## Key Components of the Implementation:

### 1. **Data Section**
- `secret`: The secret value to be shared
- `num_shares`: Total number of shares to generate
- `threshold`: Minimum shares required for reconstruction
- `prime`: Large prime number for modular arithmetic

### 2. **Core Functions**

#### `mod_multiply`
Performs modular multiplication: `(a × b) mod p`

#### `evaluate_polynomial`
Uses Horner's method to evaluate the polynomial at a given point

#### `shamir_share`
Main function that:
1. Sets the secret as the constant term
2. Generates random coefficients
3. Evaluates polynomial at different x-coordinates
4. Stores results as shares

#### `reconstruct_secret`
Demonstrates reconstruction (simplified version)

### 3. **Algorithm Overview**

1. **Setup**: Create a polynomial of degree (threshold-1) where the constant term is the secret
2. **Share Generation**: Evaluate polynomial at different x-coordinates to create shares
3. **Reconstruction**: Use Lagrange interpolation with threshold shares to recover secret

### 4. **Important Notes**

- This is a simplified implementation for demonstration
- Real-world implementation would require proper random number generation
- Modular arithmetic is crucial for security
- Full Lagrange interpolation implementation would be needed for complete reconstruction
- Error handling and edge cases should be added for production use

This assembly implementation demonstrates the core mathematical operations of Shamir's Secret Sharing while showing how the algorithm can be translated into low-level machine code.

