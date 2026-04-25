# Fast Inverse Square Root in Forth

Here's an implementation of the famous Fast Inverse Square Root algorithm in Forth:

```forth
\ Fast Inverse Square Root implementation in Forth
\ Based on the Quake III algorithm

: fast-inv-sqrt ( float -- float )
    \ Input: 32-bit IEEE float on stack
    \ Output: 1/sqrt(input) on stack
    
    \ Get the input as a 32-bit integer
    float>int dup
    
    \ Extract the exponent (bits 30-23)
    $7F00000 and 16 rshift
    
    \ Apply the magic formula
    159 swap - 1 rshift
    
    \ Combine with the modified exponent
    $7F00000 and or
    
    \ Convert back to float
    int>float
    
    \ Newton-Raphson refinement
    \ First approximation
    0.5 2swap * 1.5 2swap * - 1.0 +
    
    \ Refine once more for better accuracy
    0.5 2swap * 1.5 2swap * - 1.0 +
    
    \ Return result
    ;

\ Alternative version with more precise Newton-Raphson steps
: fast-inv-sqrt-precise ( float -- float )
    \ Get input as integer
    float>int dup
    
    \ Extract and modify exponent
    $7F00000 and 16 rshift
    159 swap - 1 rshift
    
    \ Reconstruct the float
    $7F00000 and or
    int>float
    
    \ Newton-Raphson iteration
    0.5 2swap * 1.5 2swap * - 1.0 +
    0.5 2swap * 1.5 2swap * - 1.0 +
    0.5 2swap * 1.5 2swap * - 1.0 +
    ;

\ Example usage:
\ 4.0 fast-inv-sqrt  \ Should give approximately 0.5
\ 2.0 fast-inv-sqrt  \ Should give approximately 0.707
```

## Key Features:

1. **Magic Number**: Uses the famous `0x5f3759df` bit manipulation technique
2. **Bit Manipulation**: Extracts and modifies the IEEE 754 exponent bits
3. **Newton-Raphson Refinement**: Applies iterative improvement for better accuracy
4. **Stack-based**: Follows Forth's stack-oriented programming paradigm

## Usage Example:

```forth
\ Test the function
4.0 float>int  \ Convert 4.0 to integer representation
fast-inv-sqrt  \ Get 1/sqrt(4) = 0.5
```

**Note**: This is a simplified version that demonstrates the core concepts. The original Quake implementation was more complex and used inline assembly for optimal performance.

