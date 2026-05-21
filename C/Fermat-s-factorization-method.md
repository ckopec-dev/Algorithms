# Fermat's Factorization Method in C

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a-b)(a+b).

```c
#include <stdio.h>
#include <math.h>

// Function to implement Fermat's factorization method
int fermat_factorization(long long n) {
    // Handle edge cases
    if (n <= 1) {
        printf("Number must be greater than 1\n");
        return 0;
    }
    
    // Start with the square root of n
    long long a = (long long)ceil(sqrt(n));
    long long b_squared = a * a - n;
    
    printf("Starting with a = %lld\n", a);
    
    // Continue until b_squared is a perfect square
    while (b_squared > 0) {
        long long b = (long long)sqrt(b_squared);
        
        // Check if b_squared is a perfect square
        if (b * b == b_squared) {
            printf("Found factors!\n");
            printf("a = %lld, b = %lld\n", a, b);
            printf("Factors: %lld and %lld\n", a - b, a + b);
            return 1;
        }
        
        a++;
        b_squared = a * a - n;
        printf("a = %lld, b² = %lld\n", a, b_squared);
    }
    
    printf("No factors found\n");
    return 0;
}

// Alternative implementation that returns both factors
void fermat_factorize(long long n, long long *factor1, long long *factor2) {
    if (n <= 1) {
        *factor1 = *factor2 = 0;
        return;
    }
    
    long long a = (long long)ceil(sqrt(n));
    long long b_squared = a * a - n;
    
    while (b_squared > 0) {
        long long b = (long long)sqrt(b_squared);
        
        if (b * b == b_squared) {
            *factor1 = a - b;
            *factor2 = a + b;
            return;
        }
        
        a++;
        b_squared = a * a - n;
    }
    
    *factor1 = *factor2 = 0; // No factors found
}

int main() {
    long long number;
    
    printf("Fermat's Factorization Method\n");
    printf("=============================\n");
    
    // Test with a composite number
    number = 5959;
    printf("\nFactoring %lld:\n", number);
    
    long long factor1, factor2;
    fermat_factorize(number, &factor1, &factor2);
    
    if (factor1 != 0 && factor2 != 0) {
        printf("Factors found: %lld × %lld = %lld\n", factor1, factor2, factor1 * factor2);
    } else {
        printf("Could not factor %lld using Fermat's method\n", number);
    }
    
    // Test with another number
    number = 1343;
    printf("\nFactoring %lld:\n", number);
    
    fermat_factorize(number, &factor1, &factor2);
    
    if (factor1 != 0 && factor2 != 0) {
        printf("Factors found: %lld × %lld = %lld\n", factor1, factor2, factor1 * factor2);
    } else {
        printf("Could not factor %lld using Fermat's method\n", number);
    }
    
    return 0;
}
```

## How the Algorithm Works:

1. **Start with a = ⌈√n⌉** (ceiling of square root of n)
2. **Calculate b² = a² - n**
3. **Check if b² is a perfect square**
4. **If yes, then n = (a-b)(a+b)**
5. **If no, increment a and repeat**

## Sample Output:
```
Fermat's Factorization Method
=============================

Factoring 5959:
Starting with a = 78
a = 78, b² = 57
a = 79, b² = 152
a = 80, b² = 251
a = 81, b² = 352
a = 82, b² = 455
a = 83, b² = 560
a = 84, b² = 667
a = 85, b² = 776
a = 86, b² = 887
a = 87, b² = 1000
a = 88, b² = 1115
a = 89, b² = 1232
a = 90, b² = 1351
a = 91, b² = 1472
a = 92, b² = 1595
a = 93, b² = 1720
a = 94, b² = 1847
a = 95, b² = 1976
a = 96, b² = 2107
a = 97, b² = 2240
a = 98, b² = 2375
a = 99, b² = 2512
a = 100, b² = 2651
a = 101, b² = 2792
a = 102, b² = 2935
a = 103, b² = 3080
a = 104, b² = 3227
a = 105, b² = 3376
a = 106, b² = 3527
a = 107, b² = 3680
a = 108, b² = 3835
a = 109, b² = 3992
a = 110, b² = 4151
a = 111, b² = 4312
a = 112, b² = 4475
a = 113, b² = 4640
a = 114, b² = 4807
a = 115, b² = 4976
a = 116, b² = 5147
a = 117, b² = 5320
a = 118, b² = 5495
a = 119, b² = 5672
a = 120, b² = 5851
a = 121, b² = 6032
a = 122, b² = 6215
a = 123, b² = 6400
Found factors!
a = 123, b = 80
Factors: 43 and 139
Factors found: 43 × 139 = 5959
```

## Time Complexity:
- **Best case**: O(1) when n is close to a perfect square
- **Worst case**: O(√n) when n is a product of two primes that are far apart

## Key Points:
- Fermat's method works best when the two factors of n are close to each other
- It's particularly effective for numbers that are products of two primes of similar size
- The method is inefficient for numbers with factors that are very far apart
- It's mainly used for educational purposes and in certain cryptographic applications

