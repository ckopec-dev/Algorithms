# RSA Encryption in Assembly Language

Here's an example of RSA encryption implemented in x86-64 Assembly language using NASM syntax:

```assembly
section .data
    ; RSA parameters (example values)
    modulus     dq 0x0123456789abcdef  ; n = p * q (2048-bit example)
    public_exp  dq 0x0000000000000003  ; e = 3 (common public exponent)
    message     dq 0x0000000000000012  ; plaintext message
    
    ; Buffers for calculations
    result      dq 0
    temp1       dq 0
    temp2       dq 0

section .text
    global _start

; RSA Encryption function: c = m^e mod n
; Input: message (m), public_exp (e), modulus (n)
; Output: encrypted_result (c)
rsa_encrypt:
    push rbp
    mov rbp, rsp
    
    ; Load parameters
    mov rax, [message]      ; Load message (m)
    mov rbx, [public_exp]   ; Load public exponent (e)
    mov rcx, [modulus]      ; Load modulus (n)
    
    ; Initialize result to 1
    mov rdx, 1              ; result = 1
    
    ; Modular exponentiation using binary exponentiation
    ; This implements: result = m^e mod n
binary_exp_mod:
    ; Check if exponent is 0
    cmp rbx, 0
    je encrypt_done
    
    ; If exponent is odd, multiply result by current base
    test rbx, 1
    jz skip_multiply
    
    ; result = (result * m) mod n
    mov r8, rdx             ; Save current result
    mov r9, rax             ; Save base (m)
    mul r8                  ; rax = r8 * r9 (result * m)
    xor rdx, rdx            ; Clear rdx for division
    div rcx                 ; rax = quotient, rdx = remainder
    mov rdx, rdx            ; rdx = result mod n
    
skip_multiply:
    ; Square the base and halve the exponent
    mov r8, rax             ; Save base
    mul r8                  ; rax = base * base
    xor rdx, rdx            ; Clear rdx for division
    div rcx                 ; rax = quotient, rdx = remainder
    mov rax, rdx            ; rax = base^2 mod n
    
    shr rbx, 1              ; exponent = exponent / 2
    jmp binary_exp_mod

encrypt_done:
    ; Store final result
    mov [result], rdx
    
    pop rbp
    ret

; Simple modular multiplication function (for demonstration)
mod_multiply:
    push rbp
    mov rbp, rsp
    
    mov rax, [message]      ; Load first operand
    mov rbx, [public_exp]   ; Load second operand
    mov rcx, [modulus]      ; Load modulus
    
    mul rbx                 ; Multiply operands
    xor rdx, rdx            ; Clear remainder
    div rcx                 ; Divide by modulus
    mov rax, rdx            ; Result = remainder
    
    pop rbp
    ret

; Main program execution
_start:
    ; Call RSA encryption
    call rsa_encrypt
    
    ; Exit program
    mov eax, 1              ; sys_exit
    mov ebx, 0              ; exit status
    int 0x80
```

## Key Components Explained:

### 1. **RSA Parameters**
- `modulus`: The RSA modulus n = p × q
- `public_exp`: The public exponent e (typically 3 or 65537)
- `message`: The plaintext message to encrypt

### 2. **Algorithm Implementation**
- **Binary Exponentiation**: Uses the efficient method to compute m^e mod n
- **Modular Arithmetic**: Performs all calculations modulo n
- **Loop Structure**: Implements the exponentiation by squaring algorithm

### 3. **Assembly Features Used**
- **Register Operations**: Uses RAX, RBX, RCX, RDX for calculations
- **Conditional Jumps**: `cmp`, `test`, `je`, `jz` for control flow
- **Bit Manipulation**: `shr` for bit shifting (dividing exponent by 2)
- **Division**: Uses `div` instruction for modular arithmetic

### 4. **Algorithm Steps**
1. Load message, exponent, and modulus
2. Initialize result to 1
3. Use binary exponentiation:
   - If exponent is odd, multiply result by current base
   - Square the base and halve the exponent
   - Continue until exponent becomes 0
4. Return final result (ciphertext)

## Note:
This is a simplified educational example. Real RSA implementations require:
- Large number arithmetic (bignum libraries)
- Proper padding schemes (OAEP, PKCS#1)
- Secure random number generation
- Proper handling of multi-precision integers
- Memory management and error handling

The actual implementation would be much more complex due to the need for handling large integers beyond 64-bit registers.

