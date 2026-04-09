# Euclidean Algorithm in C

Here's an example implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in C:

```c
#include <stdio.h>

// Iterative implementation of Euclidean algorithm
int gcd_iterative(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Recursive implementation of Euclidean algorithm
int gcd_recursive(int a, int b) {
    if (b == 0) {
        return a;
    }
    return gcd_recursive(b, a % b);
}

int main() {
    int num1 = 48;
    int num2 = 18;
    
    printf("Finding GCD of %d and %d\n", num1, num2);
    
    // Using iterative approach
    int result1 = gcd_iterative(num1, num2);
    printf("GCD (iterative): %d\n", result1);
    
    // Using recursive approach
    int result2 = gcd_recursive(num1, num2);
    printf("GCD (recursive): %d\n", result2);
    
    // Test with another pair
    num1 = 1071;
    num2 = 462;
    printf("\nFinding GCD of %d and %d\n", num1, num2);
    printf("GCD: %d\n", gcd_iterative(num1, num2));
    
    return 0;
}
```

## Output:
```
Finding GCD of 48 and 18
GCD (iterative): 6
GCD (recursive): 6

Finding GCD of 1071 and 462
GCD: 21
```

## How it works:

The Euclidean algorithm is based on the principle that:
**gcd(a, b) = gcd(b, a mod b)**

**Step-by-step example with 48 and 18:**
1. gcd(48, 18) → 48 % 18 = 12 → gcd(18, 12)
2. gcd(18, 12) → 18 % 12 = 6 → gcd(12, 6)
3. gcd(12, 6) → 12 % 6 = 0 → gcd(6, 0)
4. Since b = 0, return a = 6

Both iterative and recursive implementations work correctly, with the iterative version being more memory-efficient as it doesn't use the call stack.

