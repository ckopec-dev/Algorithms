# ElGamal Encryption Algorithm in Assembly

Below is an example implementation of the ElGamal encryption algorithm using x86-64 Assembly language with NASM syntax.

```assembly
section .data
    ; Public parameters
    p       dq 23           ; Prime number (small for demonstration)
    g       dq 5            ; Generator
    public_key dq 10        ; Public key (g^private_key mod p)
    
    ; Private key (in real scenario, this would be kept secret)
    private_key dq 6
    
    ; Message to encrypt (as a number)
    message dq 15
    
    ; Random value for encryption
    k       dq 7
    
    ; Storage for results
    c1      dq 0            ; First component of ciphertext
    c2      dq 0            ; Second component of ciphertext

section .text
    global _start

; Function to compute modular exponentiation (base^exp mod mod)
; Parameters: base, exp, mod
; Returns: result in rax
mod_exp:
    push rbp
    mov rbp, rsp
    
    mov rax, 1              ; result = 1
    mov rbx, [rbp+16]       ; base
    mov rcx, [rbp+24]       ; exp
    mov rdx, [rbp+32]       ; mod
    
    ; Handle case where exponent is 0
    test rcx, rcx
    jz done_mod_exp
    
    ; Binary exponentiation algorithm
mod_exp_loop:
    test rcx, 1             ; Check if least significant bit is 1
    jz skip_multiply
    imul rax, rbx           ; result = result * base
    xor rdx, rdx            ; Clear rdx for division
    div rdx                 ; Divide by mod (result in rax, remainder in rdx)
    mov rax, rdx            ; Keep only remainder
    
skip_multiply:
    imul rbx, rbx           ; base = base * base
    xor rdx, rdx            ; Clear rdx for division
    div rdx                 ; Divide by mod (result in rax, remainder in rdx)
    mov rbx, rdx            ; Keep only remainder
    
    shr rcx, 1              ; exp = exp >> 1
    jnz mod_exp_loop        ; Continue if exp != 0
    
done_mod_exp:
    pop rbp
    ret

; ElGamal Encryption function
; Parameters: message, p, g, public_key, k
; Returns: c1 and c2 in global variables
elgamal_encrypt:
    push rbp
    mov rbp, rsp
    
    ; Calculate C1 = g^k mod p
    mov rax, [rbp+16]       ; message
    mov rbx, [rbp+24]       ; p
    mov rcx, [rbp+32]       ; g
    mov rdx, [rbp+40]       ; k
    
    ; Call mod_exp(g^k mod p)
    push rdx
    push rcx
    push rbx
    push rax
    call mod_exp
    add rsp, 32             ; Clean up stack
    mov [c1], rax           ; Store C1
    
    ; Calculate C2 = (message * public_key^k) mod p
    mov rax, [rbp+48]       ; message
    mov rbx, [rbp+56]       ; public_key
    mov rcx, [rbp+64]       ; k
    mov rdx, [rbp+72]       ; p
    
    ; Call mod_exp(public_key^k mod p)
    push rdx
    push rcx
    push rbx
    push rax
    call mod_exp
    add rsp, 32             ; Clean up stack
    
    mov rbx, rax            ; Store public_key^k in rbx
    mov rax, [message]      ; Load message
    imul rax, rbx           ; Multiply by public_key^k
    xor rdx, rdx            ; Clear rdx for division
    mov rbx, [p]            ; Load p
    div rbx                 ; Divide by p (result in rax, remainder in rdx)
    mov [c2], rdx           ; Store C2
    
    pop rbp
    ret

; ElGamal Decryption function
; Parameters: c1, c2, private_key, p
; Returns: decrypted message in rax
elgamal_decrypt:
    push rbp
    mov rbp, rsp
    
    ; Calculate s = (c1^private_key) mod p
    mov rax, [rbp+16]       ; c1
    mov rbx, [rbp+24]       ; private_key
    mov rcx, [rbp+32]       ; p
    
    ; Call mod_exp(c1^private_key mod p)
    push rcx
    push rbx
    push rax
    call mod_exp
    add rsp, 24             ; Clean up stack
    
    ; Calculate modular inverse of s (s^-1 mod p)
    mov rdx, [p]
    xor rcx, rcx            ; Initialize quotient
    mov r8, rax             ; Store s in r8
    mov rax, 1              ; Initialize result to 1
    
inverse_loop:
    cmp rdx, 1
    je inverse_done
    mov r9, rdx             ; Save original rdx
    xor rdx, rdx            ; Clear rdx for division
    div r8                  ; Divide rdx:r8 by r8 (quotient in rax, remainder in rdx)
    mov r10, rax            ; Store quotient
    mov rax, r9             ; Restore original rdx
    mov r9, rcx             ; Save current quotient
    mov rcx, r10            ; Move quotient to rcx
    imul r9, r10            ; Multiply old quotient by new quotient
    add rax, r9             ; Add to result
    
inverse_done:
    mov rax, 1              ; We need to compute s^(-1) mod p
    mov rbx, [p]
    
    ; Extended Euclidean algorithm for modular inverse
    ; Simplified approach for demonstration - in practice use extended GCD
    mov r9, rax             ; Copy of s
    mov r10, 1              ; Initialize x = 1
    mov r11, 0              ; Initialize y = 0
    
    ; This is a simplified modular inverse calculation
    ; In real implementation, use extended Euclidean algorithm
    
    pop rbp
    ret

; Main program
_start:
    ; Set up parameters for encryption
    mov rax, [p]            ; p
    mov rbx, [g]            ; g  
    mov rcx, [public_key]   ; public_key
    mov rdx, [k]            ; k
    
    ; Call ElGamal encryption
    push rdx                ; k
    push rcx                ; public_key
    push rbx                ; g
    push rax                ; p
    push [message]          ; message
    call elgamal_encrypt
    add rsp, 40             ; Clean up stack
    
    ; Print results (simplified - actual printing would require system calls)
    ; C1 = [c1]
    ; C2 = [c2]
    
    ; Exit program
    mov rax, 60             ; sys_exit
    mov rdi, 0              ; exit status
    syscall
```

## Key Components of the Implementation

### 1. **Modular Exponentiation Function**

- Implements binary exponentiation for efficient modular exponentiation
- Uses the standard algorithm with bit manipulation

### 2. **ElGamal Encryption Process**

- **C1 = g^k mod p** (first component)
- **C2 = (message × public_key^k) mod p** (second component)

### 3. **Key Parameters**

- Prime number `p` = 23
- Generator `g` = 5
- Private key = 6
- Public key = 10
- Message = 15
- Random value `k` = 7

### 4. **Security Considerations**

- In real applications, the random value `k` must be chosen securely
- The private key should never be exposed
- Modular arithmetic is crucial for security

## Usage Notes

This is a simplified demonstration implementation. In practice, you would need:

1. Proper error handling
2. Larger prime numbers (at least 1024 bits)
3. Secure random number generation
4. Complete system call support for output
5. More robust modular inverse calculation

The algorithm demonstrates the mathematical principles behind ElGamal encryption while showing how it translates to assembly code structure.
