# Viterbi Algorithm Implementation in Assembly

Here's an example implementation of the Viterbi algorithm in x86-64 assembly language for a simple Hidden Markov Model (HMM) with 2 states and 2 observations.

```assembly
.section .data
    # Transition probabilities (2x2 matrix)
    trans_prob: .double 0.7, 0.3   # State 0 -> State 0, State 0 -> State 1
                .double 0.4, 0.6   # State 1 -> State 0, State 1 -> State 1
    
    # Emission probabilities (2x2 matrix)
    emit_prob:  .double 0.5, 0.5   # State 0 emits observation 0, 1
                .double 0.6, 0.4   # State 1 emits observation 0, 1
    
    # Observation sequence (example: [0, 1, 1, 0])
    observations: .byte 0, 1, 1, 0
    obs_len: .long 4
    
    # Viterbi table for dynamic programming
    viterbi_table: .space 80        # 4 observations × 2 states × 8 bytes (double)
    path_table: .space 4            # 4 observations × 1 byte (path)

.section .text
.global _start

viterbi_algorithm:
    # Function to compute Viterbi path
    # Input: observations array, length
    # Output: most likely state sequence in path_table
    
    push %rbp
    mov %rsp, %rbp
    
    # Parameters
    mov %rdi, %r8      # observations pointer
    mov %rsi, %r9      # observation length
    
    # Initialize Viterbi table with zeros
    xor %rax, %rax     # counter
    mov $10, %rcx      # 5 states * 2 observations = 10 doubles (80 bytes)
init_loop:
    movsd $0.0, (%r8, %rax, 8)  # Initialize with zeros
    inc %rax
    dec %rcx
    jnz init_loop
    
    # Step 1: Initialize first observation
    # For state 0: prob = emit_prob[0][obs[0]] * initial_prob[0]
    # For state 1: prob = emit_prob[1][obs[0]] * initial_prob[1]
    
    mov $0, %rax       # current observation index
    
    # Calculate P(0) for first observation
    movb (%r8), %dl    # get first observation (0 or 1)
    movslq %edx, %rdx  # extend to 64-bit
    movsd emit_prob(,%rdx, 8), %xmm0   # load emission prob for state 0
    
    # Store in Viterbi table [0][0] = P(0) for first observation
    movsd %xmm0, viterbi_table(%rax)
    
    # Calculate P(1) for first observation  
    movsd emit_prob+8(,%rdx, 8), %xmm0   # load emission prob for state 1
    movsd %xmm0, viterbi_table+8(%rax)   # store in [0][1]
    
    # Step 2: Forward recursion
    mov $1, %rax       # start from second observation (index 1)
outer_loop:
    cmp %r9, %rax      # compare with length
    jge end_viterbi
    
    # Get current observation
    movb (%r8,%rax), %dl
    movslq %edx, %rdx
    
    # For each state in current observation
    mov $0, %rcx       # state counter (0 or 1)
inner_loop:
    cmp $2, %rcx       # max 2 states
    jge inner_loop_end
    
    # Calculate probability for this state at current time step
    movsd emit_prob(,%rdx, 8), %xmm0   # emission prob for this state
    
    # Find maximum transition probability from previous states
    movsd viterbi_table-8(%rax, 8), %xmm1  # previous state 0
    movsd viterbi_table(%rax, 8), %xmm2    # previous state 1
    
    # Get transition probabilities
    movsd trans_prob(,%rcx, 8), %xmm3      # trans_prob[0][state]
    movsd trans_prob+8(,%rcx, 8), %xmm4    # trans_prob[1][state]
    
    # Calculate max transition probability
    mulsd %xmm3, %xmm1   # P(state_0) * trans_prob[0][state]
    mulsd %xmm4, %xmm2   # P(state_1) * trans_prob[1][state]
    
    # Find maximum of the two
    movsd %xmm1, %xmm5
    cmpsd %xmm2, %xmm5
    cmovs %xmm2, %xmm5
    
    # Multiply by emission probability
    mulsd %xmm0, %xmm5
    
    # Store in Viterbi table
    movsd %xmm5, viterbi_table(%rax, 8)
    
    inc %rcx
    jmp inner_loop
    
inner_loop_end:
    inc %rax
    jmp outer_loop

end_viterbi:
    # Backtrack to find most likely path
    # Find final state with maximum probability
    mov $0, %rax       # state index
    movsd viterbi_table(%rax), %xmm0   # max prob for state 0
    movsd viterbi_table+8(%rax), %xmm1 # max prob for state 1
    
    cmpsd %xmm1, %xmm0
    cmovs %xmm1, %xmm0
    
    # Store the path (simple implementation - just return last state)
    movb %al, path_table(%r9)  # store final state
    
    pop %rbp
    ret

# Main function to demonstrate Viterbi algorithm
main:
    push %rbp
    mov %rsp, %rbp
    
    # Call Viterbi algorithm
    leaq observations, %rdi   # load observations array
    movl obs_len, %esi       # load observation length
    call viterbi_algorithm
    
    # Exit program
    mov $60, %rax      # sys_exit
    mov $0, %rdi       # exit status
    syscall

.section .bss
    # Additional working space for calculations
    temp_storage: .space 1024
```

## Key Components Explained:

### 1. **Data Section**
- `trans_prob`: 2×2 transition probability matrix
- `emit_prob`: 2×2 emission probability matrix  
- `observations`: Input observation sequence
- `viterbi_table`: Dynamic programming table for storing probabilities

### 2. **Algorithm Steps**
1. **Initialization**: Set up first observation probabilities
2. **Forward Recursion**: Calculate probabilities for each state at each time step
3. **Backtracking**: Determine the most likely path

### 3. **Key Assembly Concepts Used**
- **Memory addressing**: Using base + offset calculations for arrays
- **Floating-point operations**: `movsd`, `mulsd`, `cmpsd` for double precision
- **Conditional jumps**: `jge`, `jnz`, `cmovs` for control flow
- **Register usage**: Proper handling of function parameters and return values

### 4. **Important Notes**
- This is a simplified implementation for educational purposes
- Real-world HMMs would require more sophisticated memory management
- The algorithm assumes equal initial probabilities for both states
- Error checking and bounds validation are omitted for clarity

This implementation demonstrates how the Viterbi algorithm can be efficiently implemented at the assembly level, leveraging the processor's capabilities for fast numerical computation.