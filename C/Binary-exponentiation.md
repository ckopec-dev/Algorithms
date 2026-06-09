# Binary Exponentiation Algorithm in C

Binary exponentiation is an efficient algorithm to compute large powers of a number. It uses the binary representation of the exponent to reduce the time complexity from O(n) to O(log n).

## Code Implementation

```c
#include <stdio.h>

// Binary exponentiation function
long long binary_exponentiation(long long base, long long exp) {
    long long result = 1;
    
    while (exp > 0) {
        // If exp is odd, multiply base with result
        if (exp % 2 == 1) {
            result = result * base;
        }
        
        // Square the base and halve the exponent
        base = base * base;
        exp = exp / 2;
    }
    
    return result;
}

// Alternative implementation using bit manipulation
long long binary_exponentiation_bitwise(long long base, long long exp) {
    long long result = 1;
    
    while (exp > 0) {
        // If exp is odd (last bit is 1)
        if (exp & 1) {
            result = result * base;
        }
        
        // Square the base and shift exponent right by 1 bit
        base = base * base;
        exp = exp >> 1;
    }
    
    return result;
}

int main() {
    long long base, exp;
    
    printf("Enter base: ");
    scanf("%lld", &base);
    printf("Enter exponent: ");
    scanf("%lld", &exp);
    
    long long result1 = binary_exponentiation(base, exp);
    long long result2 = binary_exponentiation_bitwise(base, exp);
    
    printf("\nResult using normal method: %lld\n", result1);
    printf("Result using bitwise method: %lld\n", result2);
    
    // Example with specific values
    printf("\nExamples:\n");
    printf("2^10 = %lld\n", binary_exponentiation(2, 10));
    printf("3^5 = %lld\n", binary_exponentiation(3, 5));
    printf("5^4 = %lld\n", binary_exponentiation(5, 4));
    
    return 0;
}
```

## How it works:

**Algorithm Steps:**
1. Initialize result = 1
2. While exponent > 0:
   - If exponent is odd, multiply result by base
   - Square the base and halve the exponent
3. Return result

**Example trace for 2^10:**
```
exp = 10 (even): result = 1, base = 4, exp = 5
exp = 5 (odd): result = 1 × 4 = 4, base = 16, exp = 2
exp = 2 (even): result = 4, base = 256, exp = 1
exp = 1 (odd): result = 4 × 256 = 1024, base = 65536, exp = 0
Return 1024
```

## Time Complexity:
- **Time:** O(log n) where n is the exponent
- **Space:** O(1)

## Output Example:
```
Enter base: 2
Enter exponent: 10

Result using normal method: 1024
Result using bitwise method: 1024

Examples:
2^10 = 1024
3^5 = 243
5^4 = 625
```