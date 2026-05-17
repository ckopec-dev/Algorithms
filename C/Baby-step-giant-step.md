# Baby-step Giant-step Algorithm in C

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding x such that g^x ≡ h (mod p), where g, h, and p are given.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to compute (base^exp) % mod using modular exponentiation
long long mod_pow(long long base, long long exp, long long mod) {
    long long result = 1;
    base = base % mod;
    
    while (exp > 0) {
        if (exp % 2 == 1) {
            result = (result * base) % mod;
        }
        exp = exp >> 1;
        base = (base * base) % mod;
    }
    
    return result;
}

// Function to compute discrete logarithm using baby-step giant-step
long long baby_step_giant_step(long long g, long long h, long long p) {
    long long m = (long long)ceil(sqrt(p - 1));
    
    // Baby steps: compute g^j mod p for j = 0, 1, 2, ..., m-1
    // Store in hash table (using array for simplicity)
    long long *baby_steps = (long long*)malloc(p * sizeof(long long));
    for (long long i = 0; i < p; i++) {
        baby_steps[i] = -1;
    }
    
    long long g_m = mod_pow(g, m, p);
    long long current = 1;
    
    // Store baby steps
    for (long long j = 0; j < m; j++) {
        baby_steps[current] = j;
        current = (current * g) % p;
    }
    
    // Giant steps: compute h * (g^(-m))^i mod p for i = 0, 1, 2, ..., m-1
    long long g_inv_m = mod_pow(g_m, p - 2, p); // Using Fermat's little theorem
    long long target = h;
    
    for (long long i = 0; i < m; i++) {
        if (baby_steps[target] != -1) {
            // Found solution: x = i * m + baby_steps[target]
            long long result = i * m + baby_steps[target];
            free(baby_steps);
            return result;
        }
        target = (target * g_inv_m) % p;
    }
    
    free(baby_steps);
    return -1; // No solution found
}

int main() {
    // Example: Find x such that 3^x ≡ 13 (mod 17)
    long long g = 3;
    long long h = 13;
    long long p = 17;
    
    printf("Solving discrete logarithm: %lld^x ≡ %lld (mod %lld)\n", g, h, p);
    
    long long result = baby_step_giant_step(g, h, p);
    
    if (result != -1) {
        printf("Solution found: x = %lld\n", result);
        
        // Verify the result
        long long verification = mod_pow(g, result, p);
        printf("Verification: %lld^%lld mod %lld = %lld\n", g, result, p, verification);
        printf("Expected: %lld\n", h);
        
        if (verification == h) {
            printf("✓ Verification successful!\n");
        } else {
            printf("✗ Verification failed!\n");
        }
    } else {
        printf("No solution found\n");
    }
    
    // Another example: Find x such that 2^x ≡ 11 (mod 13)
    printf("\n--- Another example ---\n");
    g = 2;
    h = 11;
    p = 13;
    
    printf("Solving discrete logarithm: %lld^x ≡ %lld (mod %lld)\n", g, h, p);
    
    result = baby_step_giant_step(g, h, p);
    
    if (result != -1) {
        printf("Solution found: x = %lld\n", result);
        
        // Verify the result
        long long verification = mod_pow(g, result, p);
        printf("Verification: %lld^%lld mod %lld = %lld\n", g, result, p, verification);
        printf("Expected: %lld\n", h);
        
        if (verification == h) {
            printf("✓ Verification successful!\n");
        } else {
            printf("✗ Verification failed!\n");
        }
    } else {
        printf("No solution found\n");
    }
    
    return 0;
}
```

## How it works:

1. **Baby Steps**: Compute g^j mod p for j = 0, 1, 2, ..., m-1 and store in a hash table
2. **Giant Steps**: Compute h * (g^(-m))^i mod p for i = 0, 1, 2, ..., m-1
3. **Search**: Check if any giant step value exists in the baby steps table
4. **Solution**: If found, x = i * m + j where j is the index from baby steps

## Time Complexity:
- Time: O(√p)
- Space: O(√p)

## Sample Output:
```
Solving discrete logarithm: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 mod 17 = 13
Expected: 13
✓ Verification successful!

--- Another example ---
Solving discrete logarithm: 2^x ≡ 11 (mod 13)
Solution found: x = 7
Verification: 2^7 mod 13 = 11
Expected: 11
✓ Verification successful!
```

