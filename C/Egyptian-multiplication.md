# Egyptian Multiplication Algorithm in C

Egyptian multiplication (also known as binary multiplication or Russian peasant multiplication) is an ancient algorithm for multiplying two numbers using repeated doubling and addition.

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
    
    // Continue until the first number becomes 0
    while (a > 0) {
        // If 'a' is odd, add 'b' to the result
        if (a % 2 == 1) {
            result += b;
        }
        
        // Halve 'a' (integer division)
        a = a / 2;
        
        // Double 'b'
        b = b * 2;
    }
    
    return result;
}

int main() {
    int num1, num2;
    
    printf("Enter two numbers to multiply: ");
    scanf("%d %d", &num1, &num2);
    
    int product = egyptian_multiply(num1, num2);
    
    printf("Result: %d × %d = %d\n", num1, num2, product);
    
    // Verify with normal multiplication
    printf("Verification: %d × %d = %d\n", num1, num2, num1 * num2);
    
    return 0;
}
```

## Example Run

```
Enter two numbers to multiply: 13 9
Result: 13 × 9 = 117
Verification: 13 × 9 = 117
```

## Step-by-step trace for 13 × 9:

| a | b | a%2 | Action |
|---|---|-----|--------|
| 13 | 9 | 1 | result = 0 + 9 = 9 |
| 6  | 18 | 0 | (no addition) |
| 3  | 36 | 1 | result = 9 + 36 = 45 |
| 1  | 72 | 1 | result = 45 + 72 = 117 |
| 0  | 144 | - | stop |

## Alternative Implementation with Detailed Steps

```c
#include <stdio.h>

int egyptian_multiply_verbose(int a, int b) {
    int result = 0;
    int step = 1;
    
    printf("Multiplying %d × %d using Egyptian method:\n", a, b);
    
    while (a > 0) {
        printf("Step %d: a=%d, b=%d", step, a, b);
        
        if (a % 2 == 1) {
            result += b;
            printf(" -> odd a, add b: result = %d\n", result);
        } else {
            printf(" -> even a, no addition\n");
        }
        
        a = a / 2;
        b = b * 2;
        step++;
    }
    
    return result;
}

int main() {
    int num1 = 13, num2 = 9;
    int product = egyptian_multiply_verbose(num1, num2);
    printf("Final result: %d\n", product);
    return 0;
}
```

## Output of Verbose Version

```
Multiplying 13 × 9 using Egyptian method:
Step 1: a=13, b=9 -> odd a, add b: result = 9
Step 2: a=6, b=18 -> even a, no addition
Step 3: a=3, b=36 -> odd a, add b: result = 45
Step 4: a=1, b=72 -> odd a, add b: result = 117
Step 5: a=0, b=144 -> even a, no addition
Final result: 117
```

This implementation demonstrates the classic Egyptian multiplication algorithm that efficiently multiplies two numbers using only addition, subtraction, and bit shifting operations.

