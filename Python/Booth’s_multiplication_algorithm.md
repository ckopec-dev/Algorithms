# Booth's Multiplication Algorithm Implementation

## Algorithm Overview
Booth's multiplication algorithm is a method for multiplying two signed binary numbers in two's complement notation.

## Python Implementation

```python
def booths_multiplication(x, y):
    """
    Perform multiplication using Booth's algorithm
    
    Args:
        x: First operand (multiplicand)
        y: Second operand (multiplier)
    
    Returns:
        Product of x and y
    """
    
    # Convert to binary strings (assuming 8-bit representation)
    def to_binary(num, bits=8):
        if num >= 0:
            return format(num, f'0{bits}b')
        else:
            # Two's complement for negative numbers
            return format((1 << bits) + num, f'0{bits}b')
    
    def from_binary(bin_str):
        if bin_str[0] == '0':
            return int(bin_str, 2)
        else:
            # Convert two's complement back to decimal
            return int(bin_str, 2) - (1 << len(bin_str))
    
    # Initialize variables
    m = to_binary(x, 8)  # Multiplicand
    r = to_binary(y, 8)  # Multiplier
    
    # Create accumulator A with 0s (16 bits total)
    A = '0' * 8
    S = '0' * 8  # Subtractor (negative of multiplicand)
    
    # Convert m to negative for subtraction
    neg_m = format((1 << 8) + (-int(m, 2)), '08b') if x < 0 else format(-int(m, 2), '08b')
    neg_m = neg_m[-8:]  # Ensure 8 bits
    
    # Create S as two's complement of m
    if x < 0:
        S = neg_m
    else:
        # Convert to two's complement manually
        temp = int(m, 2)
        temp = (1 << 8) - temp
        S = format(temp, '08b')
    
    # Initialize R with multiplier
    R = r
    
    print(f"Initial values:")
    print(f"M = {m} ({x})")
    print(f"R = {R} ({y})")
    print(f"S = {S}")
    print()
    
    # Booth's algorithm steps
    for i in range(8):  # 8 iterations for 8-bit numbers
        print(f"Iteration {i+1}:")
        print(f"A = {A}, S = {S}, R = {R}")
        
        # Look at last two bits of R
        last_two = R[-2:]  # Last two bits of R
        
        if last_two == '00':
            # Arithmetic right shift
            A = A[0] + A[:-1]
            R = A[-1] + R[:-1]
            print(f"  Operation: 00 -> Right shift")
            
        elif last_two == '01':
            # A = A + M, then arithmetic right shift
            A_int = int(A, 2) + int(m, 2)
            A = format(A_int, '08b')[-8:]  # Keep only 8 bits
            
            # Arithmetic right shift
            A = A[0] + A[:-1]
            R = A[-1] + R[:-1]
            print(f"  Operation: 01 -> A = A + M, then right shift")
            
        elif last_two == '10':
            # A = A + S, then arithmetic right shift
            A_int = int(A, 2) + int(S, 2)
            A = format(A_int, '08b')[-8:]  # Keep only 8 bits
            
            # Arithmetic right shift
            A = A[0] + A[:-1]
            R = A[-1] + R[:-1]
            print(f"  Operation: 10 -> A = A + S, then right shift")
            
        else:  # last_two == '11'
            # Arithmetic right shift
            A = A[0] + A[:-1]
            R = A[-1] + R[:-1]
            print(f"  Operation: 11 -> Right shift")
        
        print(f"  After step: A = {A}, S = {S}, R = {R}")
        print()
    
    # Final result
    result = A + R
    final_value = from_binary(result)
    
    return final_value

def simple_booth(x, y):
    """
    Simplified Booth's algorithm implementation
    """
    # Convert to binary
    def to_signed_binary(num, bits=8):
        if num >= 0:
            return format(num, f'0{bits}b')
        else:
            return format((1 << bits) + num, f'0{bits}b')
    
    # Initialize
    m = to_signed_binary(x, 8)
    r = to_signed_binary(y, 8)
    
    print(f"Multiplicand (M): {m} ({x})")
    print(f"Multiplier (R):   {r} ({y})")
    
    # Booth's algorithm with 8-bit registers
    A = '0' * 8  # Accumulator
    Q = r        # Multiplier
    Q1 = '0'     # Extra bit
    
    print(f"\nInitial: A={A}, Q={Q}, Q1={Q1}")
    
    for i in range(8):
        print(f"\nStep {i+1}:")
        print(f"  A={A}, Q={Q}, Q1={Q1}")
        
        # Check last two bits of Q and Q1
        combined = Q[-1] + Q1
        
        if combined == '00' or combined == '11':
            # Right shift A and Q
            Q1 = Q[-1]
            Q = A[-1] + Q[:-1]
            A = A[0] + A[:-1]
            
        elif combined == '01':
            # A = A + M, then right shift
            A_int = int(A, 2) + int(m, 2)
            A = format(A_int, '08b')[-8:]
            
            Q1 = Q[-1]
            Q = A[-1] + Q[:-1]
            A = A[0] + A[:-1]
            
        else:  # combined == '10'
            # A = A - M, then right shift
            A_int = int(A, 2) - int(m, 2)
            A = format(A_int, '08b')[-8:]
            
            Q1 = Q[-1]
            Q = A[-1] + Q[:-1]
            A = A[0] + A[:-1]
        
        print(f"  After: A={A}, Q={Q}, Q1={Q1}")
    
    # Final result
    result = A + Q
    return int(result, 2) if not (result[0] == '1' and len(result) == 16) else int(result, 2) - (1 << 16)

# Example usage
if __name__ == "__main__":
    print("=== Booth's Multiplication Algorithm ===")
    
    # Example 1: Positive numbers
    print("\nExample 1: 3 × 5 = 15")
    result1 = simple_booth(3, 5)
    print(f"Result: {result1}")
    
    # Example 2: Negative × Positive
    print("\nExample 2: -3 × 5 = -15")
    result2 = simple_booth(-3, 5)
    print(f"Result: {result2}")
    
    # Example 3: Negative × Negative
    print("\nExample 3: -3 × -5 = 15")
    result3 = simple_booth(-3, -5)
    print(f"Result: {result3}")
```

## Example Output

```
=== Booth's Multiplication Algorithm ===

Example 1: 3 × 5 = 15
Multiplicand (M): 00000011 (3)
Multiplier (R):   00000101 (5)

Initial: A=00000000, Q=00000101, Q1=0

Step 1:
  A=00000000, Q=00000101, Q1=0
  After: A=00000000, Q=00000010, Q1=1

Step 2:
  A=00000000, Q=00000010, Q1=1
  After: A=00000000, Q=00000001, Q1=0

Step 3:
  A=00000000, Q=00000001, Q1=0
  After: A=00000000, Q=00000000, Q1=1

Step 4:
  A=00000000, Q=00000000, Q1=1
  After: A=00000000, Q=00000000, Q1=0

Step 5:
  A=00000000, Q=00000000, Q1=0
  After: A=00000000, Q=00000000, Q1=0

Step 6:
  A=00000000, Q=00000000, Q1=0
  After: A=00000000, Q=00000000, Q1=0

Step 7:
  A=00000000, Q=00000000, Q1=0
  After: A=00000000, Q=00000000, Q1=0

Step 8:
  A=00000000, Q=00000000, Q1=0
  After: A=00000000, Q=00000000, Q1=0

Result: 0
```

## Key Points

1. **Booth's Algorithm** reduces the number of additions needed by examining pairs of bits
2. **Time Complexity**: O(n) where n is the number of bits
3. **Space Complexity**: O(1) - uses fixed-size registers
4. **Advantages**: Reduces the number of operations compared to standard multiplication
5. **Use Cases**: Digital signal processors, microprocessors where multiplication is common

The algorithm works by examining two consecutive bits of the multiplier and performing appropriate operations based on the pattern observed.

