# Egyptian Multiplication Algorithm in C

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, uses repeated doubling and addition to multiply two numbers.

## Algorithm Explanation

The algorithm works by:
1. Repeatedly halving one number (ignoring remainders)
2. Repeatedly doubling the other number
3. Adding the doubled numbers when the halved number is odd

## C Implementation

```c
#include <stdio.h>

int egyptian_multiply(int a, int b) {
    int result = 0;
    
    // Continue until a becomes 0
    while (a > 0) {
        // If a is odd, add b to result
        if (a % 2 == 1) {
            result += b;
        }
        
        // Halve a (integer division)
        a = a / 2;
        
        // Double b
        b = b * 2;
    }
    
    return result;
}

int main() {
    int num1 = 13;
    int num2 = 9;
    
    printf("Egyptian multiplication of %d and %d:\n", num1, num2);
    
    int product = egyptian_multiply(num1, num2);
    
    printf("Result: %d\n", product);
    
    // Verify with normal multiplication
    printf("Verification (normal multiplication): %d\n", num1 * num2);
    
    return 0;
}
```

## Step-by-Step Example

For `13 × 9`:

| a | b | a is odd? | Action |
|---|---|-----------|--------|
| 13 | 9 | Yes | Add 9 to result |
| 6 | 18 | No | Skip |
| 3 | 36 | Yes | Add 36 to result |
| 1 | 72 | Yes | Add 72 to result |
| 0 | 144 | - | Stop |

Result: 9 + 36 + 72 = 117

## Output
```
Egyptian multiplication of 13 and 9:
Result: 117
Verification (normal multiplication): 117
```

## Time Complexity
- **Time**: O(log a) where a is the smaller number
- **Space**: O(1)

This algorithm is efficient and demonstrates how ancient Egyptians performed multiplication using only addition, subtraction, and doubling operations.

