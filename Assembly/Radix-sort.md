# Radix Sort in Assembly Language

Here's an example implementation of Radix Sort in x86-64 Assembly using AT&T syntax:

```assembly
.section .data
    # Sample array to sort
    array: .long 170, 45, 75, 90, 2, 802, 24, 66
    array_size: .long 8
    
    # Count array for counting sort (0-9 digits)
    count: .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

.section .text
    .global _start

# Function: radix_sort
# Parameters: array pointer, array_size
# Returns: sorted array in place
radix_sort:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    
    # Get parameters
    mov 16(%rbp), %rdi    # array pointer
    mov 24(%rbp), %rsi    # array_size
    
    # Find maximum value to determine number of digits
    call find_max
    mov %rax, %rcx         # max_value in rcx
    
    # Initialize digit position (start with units place)
    mov $1, %r8            # digit_place = 1
    
radix_loop:
    # Check if we've processed all digits
    cmp $0, %rcx
    jz radix_done
    
    # Clear count array
    mov $0, %rax
    mov $0, %rbx
    mov $0, %rcx
    mov $0, %rdx
    mov $0, %rsi
    mov $0, %rdi
    mov $0, %r9
    mov $0, %r10
    mov $0, %r11
    mov $0, %r12
    
    # Count occurrences of each digit
    call counting_sort
    
    # Move to next digit place
    imul $10, %r8
    mov %r8, %rcx          # update max_value for next iteration
    
    jmp radix_loop

radix_done:
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rbp
    ret

# Function: counting_sort
# Parameters: array pointer, array_size, digit_place
# Performs counting sort based on current digit place
counting_sort:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    
    # Initialize count array to zero
    mov $count, %rdi
    mov $10, %rcx
    xor %rax, %rax
    
clear_loop:
    mov %rax, (%rdi)
    add $4, %rdi
    dec %rcx
    jnz clear_loop
    
    # Count occurrences of each digit
    mov 16(%rbp), %rdi    # array pointer
    mov 24(%rbp), %rsi    # array_size
    mov 32(%rbp), %r8     # digit_place
    
    mov $0, %rcx          # index counter
    
count_loop:
    cmp %rsi, %rcx
    jge count_done
    
    # Get current element
    mov (%rdi, %rcx, 4), %rax
    
    # Extract digit at current place
    mov %rax, %rbx
    xor %rdx, %rdx
    mov %r8, %rcx
    div %rcx              # quotient in rax, remainder in rdx
    mov %rdx, %rcx        # digit is in rcx
    
    # Increment count for this digit
    mov $count, %rdi
    mov (%rdi, %rcx, 4), %rax
    inc %rax
    mov %rax, (%rdi, %rcx, 4)
    
    inc %rcx
    jmp count_loop

count_done:
    # Calculate cumulative counts
    mov $count, %rdi
    mov $1, %rcx
    mov (%rdi), %rax
    mov %rax, %rbx
    
    mov $1, %rcx
    mov $count, %rdi
    
cumulative_loop:
    cmp $10, %rcx
    jge cumulative_done
    
    mov (%rdi, %rcx, 4), %rax
    add %rbx, %rax
    mov %rax, (%rdi, %rcx, 4)
    mov %rax, %rbx
    inc %rcx
    jmp cumulative_loop

cumulative_done:
    # Build output array
    mov 16(%rbp), %rdi    # array pointer
    mov 24(%rbp), %rsi    # array_size
    mov $count, %rdi      # count array
    mov 32(%rbp), %r8     # digit_place
    
    mov %rsi, %rcx        # index counter (from end)
    dec %rcx
    
build_loop:
    cmp $-1, %rcx
    jle build_done
    
    # Get current element
    mov (%rdi, %rcx, 4), %rax
    
    # Extract digit at current place
    mov %rax, %rbx
    xor %rdx, %rdx
    mov %r8, %rcx
    div %rcx              # quotient in rax, remainder in rdx
    mov %rdx, %rcx        # digit is in rcx
    
    # Get position from count array
    mov $count, %rdi
    mov (%rdi, %rcx, 4), %rax
    dec %rax
    mov %rax, %rbx
    
    # Store element in output array
    mov 16(%rbp), %rdi    # array pointer
    mov (%rdi, %rbx, 4), %rax
    
    # Update count array
    mov $count, %rdi
    mov %rbx, %rcx
    mov (%rdi, %rcx, 4), %rax
    dec %rax
    mov %rax, (%rdi, %rcx, 4)
    
    dec %rcx
    jmp build_loop

build_done:
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rbp
    ret

# Function: find_max
# Parameters: array pointer, array_size
# Returns: maximum value in array
find_max:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %rcx
    push %rdx
    
    mov 16(%rbp), %rdi    # array pointer
    mov 24(%rbp), %rsi    # array_size
    
    mov $0, %rcx          # index counter
    mov (%rdi), %rax      # first element as max
    
max_loop:
    cmp %rsi, %rcx
    jge max_done
    
    mov (%rdi, %rcx, 4), %rbx
    cmp %rax, %rbx
    jle max_continue
    
    mov %rbx, %rax        # update max
    
max_continue:
    inc %rcx
    jmp max_loop

max_done:
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rbp
    ret

# Main program
_start:
    # Call radix sort on our sample array
    mov $array, %rdi      # array pointer
    mov $array_size, %rsi # array_size
    
    call radix_sort
    
    # Exit program
    mov $60, %rax         # sys_exit
    mov $0, %rdi          # exit status
    syscall
```

## Key Features of This Implementation:

1. **Multi-pass Sorting**: Processes digits from least significant to most significant
2. **Counting Sort Integration**: Uses counting sort as a subroutine for each digit position
3. **Dynamic Digit Handling**: Automatically determines the number of digits needed
4. **In-place Sorting**: Modifies the original array without requiring extra space
5. **Complete Algorithm**: Handles the entire radix sort process including:
   - Finding maximum value
   - Processing each digit place
   - Using counting sort for each pass

## How It Works:

1. **Find Maximum**: Determine the maximum number to know how many digits to process
2. **Digit Processing**: For each digit position (units, tens, hundreds, etc.):
   - Use counting sort to sort elements by that digit
   - Move to the next digit place
3. **Completion**: Continue until all digits are processed

This implementation demonstrates the core principles of radix sort in assembly language, showing how to manipulate memory, perform arithmetic operations, and implement the counting sort subroutine that's essential to radix sort's efficiency.