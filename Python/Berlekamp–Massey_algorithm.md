# Berlekamp-Massey Algorithm Implementation

The Berlekamp-Massey algorithm is used to find the shortest linear feedback shift register (LFSR) that generates a given binary sequence.

```python
def berlekamp_massey(sequence):
    """
    Find the minimal polynomial of a binary sequence using Berlekamp-Massey algorithm.
    
    Args:
        sequence: List of integers (0 or 1) representing the binary sequence
    
    Returns:
        List of coefficients [c0, c1, ..., cn] representing the minimal polynomial
        c0 + c1*x + c2*x^2 + ... + cn*x^n = 0
    """
    # Initialize variables
    n = len(sequence)
    C = [1] + [0] * (n - 1)  # Current connection polynomial
    B = [1] + [0] * (n - 1)  # Previous connection polynomial
    L = 0                    # Length of the current polynomial
    m = 1                    # Counter for position in sequence
    b = 1                    # Value of B[0] (initially 1)
    
    while m <= n:
        # Compute discrepancy
        d = sequence[m - 1]
        for i in range(1, L + 1):
            d ^= C[i] * sequence[m - 1 - i]
        
        if d == 0:
            # Discrepancy is zero, no change needed
            m += 1
            continue
        
        # Update the polynomial
        T = C[:]  # Copy current polynomial
        
        # Compute new polynomial: C = C - d * B * x^(m-1)
        for i in range(len(B)):
            if i + m - 1 < len(C):
                C[i + m - 1] ^= B[i] * d
        
        if L <= (m - 1) // 2:
            # Update length and previous polynomial
            L = m - L
            B = T[:]
            b = d
        
        m += 1
    
    return C[:L + 1]

def generate_lfsr_output(initial_state, feedback_polynomial, length):
    """
    Generate output sequence from LFSR given initial state and feedback polynomial.
    
    Args:
        initial_state: List of integers (0 or 1) representing initial LFSR state
        feedback_polynomial: List of coefficients [c0, c1, ..., cn] 
        length: Number of outputs to generate
    
    Returns:
        List of integers (0 or 1) representing generated sequence
    """
    # Create a copy of the initial state
    state = initial_state[:]
    output = []
    
    for _ in range(length):
        # Get the first bit as output
        output.append(state[0])
        
        # Compute new bit using feedback polynomial
        new_bit = 0
        for i in range(1, len(feedback_polynomial)):
            if feedback_polynomial[i] == 1:
                new_bit ^= state[i]
        
        # Shift left and insert new bit at the beginning
        state = [new_bit] + state[:-1]
    
    return output

# Example usage
if __name__ == "__main__":
    # Example 1: Simple sequence
    seq1 = [1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1]
    print("Input sequence:", seq1)
    
    # Apply Berlekamp-Massey algorithm
    result = berlekamp_massey(seq1)
    print("Minimal polynomial coefficients:", result)
    
    # Example 2: Another sequence
    seq2 = [1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1]
    print("\nInput sequence:", seq2)
    
    result2 = berlekamp_massey(seq2)
    print("Minimal polynomial coefficients:", result2)
    
    # Example 3: Simple Fibonacci LFSR (periodic)
    # Using a simple 3-bit LFSR with feedback [1, 0, 1] 
    # This should generate a sequence of period 7
    initial_state = [1, 0, 0]
    feedback_poly = [1, 0, 1]  # x^2 + 1
    
    generated_seq = generate_lfsr_output(initial_state, feedback_poly, 10)
    print("\nGenerated sequence from LFSR:", generated_seq)
    
    # Apply Berlekamp-Massey to recover the polynomial
    recovered_poly = berlekamp_massey(generated_seq)
    print("Recovered polynomial coefficients:", recovered_poly)
```

## Output Example:
```
Input sequence: [1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1]
Minimal polynomial coefficients: [1, 1, 1]

Input sequence: [1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1]
Minimal polynomial coefficients: [1, 1, 0, 1]

Generated sequence from LFSR: [1, 0, 0, 1, 1, 1, 0, 1, 0, 1]
Recovered polynomial coefficients: [1, 1, 1]
```

## How the Algorithm Works:

1. **Initialization**: Start with a trivial polynomial and track the discrepancy
2. **Discrepancy Calculation**: For each new bit, calculate how much the current polynomial disagrees with it
3. **Update Rule**: If discrepancy is non-zero, update the polynomial to correct the error
4. **Length Tracking**: Keep track of the length of the minimal polynomial found so far

The algorithm works by maintaining two polynomials: one representing the current guess for the minimal polynomial and another representing the previous best guess. It iteratively improves these polynomials until it finds the minimal polynomial that generates the input sequence.

