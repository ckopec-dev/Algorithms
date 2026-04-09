# Sieve of Eratosthenes in Assembly (x86-64 NASM)

```assembly
section .data
    max_number equ 100
    sieve_array db max_number dup(1)  ; Array to mark primes (1 = prime, 0 = not prime)
    primes_list db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  ; Buffer for storing primes

section .text
    global _start

; Function to implement Sieve of Eratosthenes
sieve_of_eratosthenes:
    ; Mark 0 and 1 as not prime
    mov byte [sieve_array], 0
    mov byte [sieve_array + 1], 0
    
    ; Start with 2 (first prime)
    mov rcx, 2          ; i = 2
    mov r8, 2           ; j = i * i
    
outer_loop:
    ; Check if i * i > max_number
    mov rax, rcx
    mul rcx
    cmp rax, max_number
    jg sieve_done
    
    ; Check if i is still marked as prime
    mov al, [sieve_array + rcx]
    cmp al, 1
    jne skip_marking
    
    ; Mark multiples of i as not prime
    mov r9, rcx         ; r9 = i
    mov r10, rcx        ; r10 = j = i * i
    mov r11, 0          ; counter for prime counting
    
inner_loop:
    ; Check if j >= max_number
    cmp r10, max_number
    jge inner_loop_done
    
    ; Mark j as not prime
    mov byte [sieve_array + r10], 0
    
    ; Increment j by i
    add r10, r9
    jmp inner_loop
    
inner_loop_done:
    ; Move to next number
    inc rcx
    jmp outer_loop
    
skip_marking:
    inc rcx
    jmp outer_loop

sieve_done:
    ; Collect all prime numbers
    mov rcx, 2
    mov r10, 0          ; index for primes_list
    
collect_primes:
    ; Check if we've processed all numbers
    cmp rcx, max_number
    jge primes_done
    
    ; Check if current number is prime
    mov al, [sieve_array + rcx]
    cmp al, 1
    jne next_number
    
    ; Store prime number
    mov [primes_list + r10], cl
    inc r10
    
next_number:
    inc rcx
    jmp collect_primes

primes_done:
    ; Return number of primes found (in r10)
    ret

; Main program
_start:
    ; Call the sieve function
    call sieve_of_eratosthenes
    
    ; Exit program
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall
```

## Explanation

This assembly implementation of the Sieve of Eratosthenes algorithm:

### Key Features:
1. **Data Section**: 
   - `sieve_array`: Array to mark prime numbers (1 = prime, 0 = not prime)
   - `primes_list`: Buffer to store found prime numbers

2. **Algorithm Steps**:
   - Initialize all numbers as potentially prime
   - Start with 2 (first prime)
   - Mark multiples of each prime as not prime
   - Continue until √n
   - Collect remaining primes

3. **Key Registers**:
   - `rcx`: Current number being checked
   - `r8`: Square of current number
   - `r9`: Prime number being used to mark multiples
   - `r10`: Index for tracking positions

4. **Main Loops**:
   - `outer_loop`: Iterates through potential primes
   - `inner_loop`: Marks multiples of each prime as composite

### Time Complexity:
- **Time**: O(n log log n)
- **Space**: O(n)

### Usage:
The algorithm marks composite numbers in the sieve_array and can be extended to collect and display the actual prime numbers found.

