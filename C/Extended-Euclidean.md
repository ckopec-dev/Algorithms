# Extended Euclidean Algorithm in C

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two integers, but also finds the coefficients (x and y) such that ax + by = gcd(a, b).

## Code Implementation

```c
#include <stdio.h>

// Function to implement Extended Euclidean Algorithm
// Returns GCD and stores coefficients in x and y
int extended_gcd(int a, int b, int *x, int *y) {
    // Base case
    if (b == 0) {
        *x = 1;
        *y = 0;
        return a;
    }
    
    int x1, y1;
    int gcd = extended_gcd(b, a % b, &x1, &y1);
    
    // Update x and y using results of recursive call
    *x = y1;
    *y = x1 - (a / b) * y1;
    
    return gcd;
}

int main() {
    int a = 35, b = 15;
    int x, y;
    
    int gcd = extended_gcd(a, b, &x, &y);
    
    printf("GCD of %d and %d is %d\n", a, b, gcd);
    printf("Coefficients: x = %d, y = %d\n", x, y);
    printf("Verification: %d * %d + %d * %d = %d\n", 
           a, x, b, y, a * x + b * y);
    
    return 0;
}
```

## Sample Output

```
GCD of 35 and 15 is 5
Coefficients: x = 1, y = -2
Verification: 35 * 1 + 15 * -2 = 5
```

## How it Works

1. **Base Case**: When b = 0, GCD(a, 0) = a, so x = 1, y = 0
2. **Recursive Case**: 
   - Recursively find GCD(b, a % b) with coefficients x₁, y₁
   - Update coefficients: x = y₁, y = x₁ - (a/b) * y₁
3. **Result**: Returns the GCD and stores the coefficients in x and y

## Mathematical Verification

For a = 35, b = 15:
- GCD(35, 15) = 5
- 35 × 1 + 15 × (-2) = 35 - 30 = 5 ✓

This algorithm is essential for finding modular inverses and solving linear Diophantine equations.

