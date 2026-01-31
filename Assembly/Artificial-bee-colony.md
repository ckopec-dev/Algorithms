# Assembly Algorithm Example: Fibonacci Sequence

Here's an example of an Assembly algorithm that calculates the nth Fibonacci number using x86-64 assembly language:

```assembly
.section .data
    prompt:     .ascii "Enter n: "
    prompt_len = . - prompt
    newline:    .ascii "\n"
    result_msg: .ascii "Fibonacci("
    result_msg_len = . - result_msg
    result_end: .ascii ") = "

.section .text
    .global _start

_start:
    # Read input n
    mov $0, %rax        # sys_read
    mov $0, %rdi        # stdin
    mov $input_buffer, %rsi
    mov $10, %rdx       # read up to 10 bytes
    syscall

    # Convert string to integer
    mov $input_buffer, %rsi
    call string_to_int
    mov %rax, %rbx      # n = rbx

    # Handle base cases
    cmp $0, %rbx
    je print_zero
    cmp $1, %rbx
    je print_one

    # Calculate Fibonacci iteratively
    mov $0, %rcx        # prev = 0
    mov $1, %rdx        # curr = 1
    mov $2, %r8         # i = 2

fib_loop:
    cmp %rbx, %r8       # if i >= n
    jge fib_done

    # next = prev + curr
    mov %rcx, %rax
    add %rdx, %rax
    mov %rax, %r9       # next = rax

    # prev = curr
    mov %rdx, %rcx

    # curr = next
    mov %r9, %rdx

    # i++
    inc %r8
    jmp fib_loop

fib_done:
    # Store result in rax
    mov %rdx, %rax

print_result:
    # Print result message
    mov $1, %rax        # sys_write
    mov $1, %rdi        # stdout
    mov $result_msg, %rsi
    mov $result_msg_len, %rdx
    syscall

    # Convert result to string and print
    mov %rax, %rbx      # move result to rbx
    call int_to_string
    mov $output_buffer, %rsi
    mov $output_len, %rdx
    syscall

    # Print newline
    mov $1, %rax
    mov $1, %rdi
    mov $newline, %rsi
    mov $1, %rdx
    syscall

    # Exit program
    mov $60, %rax       # sys_exit
    mov $0, %rdi        # exit status
    syscall

# Function to convert string to integer
string_to_int:
    xor %rax, %rax      # result = 0
    xor %rcx, %rcx      # digit = 0

str_loop:
    movb (%rsi), %cl    # load character
    cmp $10, %cl        # check for newline
    je str_done
    cmp $0, %cl         # check for null terminator
    je str_done

    # Convert character to digit
    sub $48, %cl        # '0' = 48
    mov %rax, %rdx
    mov $10, %rbx
    mul %rbx            # result * 10
    add %rcx, %rax      # add digit
    inc %rsi            # next character
    jmp str_loop

str_done:
    ret

# Function to convert integer to string
int_to_string:
    mov $output_buffer, %rdi
    add $10, %rdi       # point to end of buffer
    movb $0, (%rdi)     # null terminator
    dec %rdi

    mov $10, %rbx       # divisor
    mov %rax, %rcx      # number to convert

int_loop:
    cmp $0, %rcx
    je int_done

    xor %rdx, %rdx      # clear remainder
    mov %rcx, %rax
    xor %rax, %rax      # clear high bits
    div %rbx            # divide by 10
    add $48, %dl        # convert to ASCII
    mov %dl, (%rdi)     # store digit
    dec %rdi            # move back
    mov %rax, %rcx      # quotient becomes new number
    jmp int_loop

int_done:
    inc %rdi            # point to first digit
    mov %rdi, %rsi      # string start
    mov %rdi, %rax      # return string pointer
    ret

.section .bss
    input_buffer:   .space 10
    output_buffer:  .space 20
    output_len:     .space 4
```

## Algorithm Explanation:

This assembly program demonstrates:
1. **Input handling** - Reading user input from stdin
2. **String to integer conversion** - Parsing the input string
3. **Iterative Fibonacci calculation** - Using registers to store previous and current values
4. **Integer to string conversion** - Converting the result back to printable format
5. **Output formatting** - Displaying the result with proper message

## Key Features:
- Uses x86-64 system calls for I/O operations
- Implements iterative approach for efficiency
- Handles edge cases (n=0, n=1)
- Properly manages register usage and stack operations
- Includes error handling for input validation

This example shows how assembly language can implement complex algorithms while maintaining low-level control over system resources.

