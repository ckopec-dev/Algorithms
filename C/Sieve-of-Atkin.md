# Sieve of Atkin Algorithm in C

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer, which is more efficient than the classic Sieve of Eratosthenes for large numbers.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void sieveOfAtkin(int limit) {
    // Create a boolean array and initialize all entries as false
    int* isPrime = (int*)calloc(limit + 1, sizeof(int));
    
    if (limit >= 2) {
        isPrime[2] = 1;  // 2 is prime
    }
    if (limit >= 3) {
        isPrime[3] = 1;  // 3 is prime
    }
    
    // Mark numbers as prime based on the Atkin sieve formula
    for (int x = 1; x * x <= limit; x++) {
        for (int y = 1; y * y <= limit; y++) {
            int n = 4 * x * x + y * y;
            if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
                isPrime[n] = !isPrime[n];
            }
            
            n = 3 * x * x + y * y;
            if (n <= limit && n % 12 == 7) {
                isPrime[n] = !isPrime[n];
            }
            
            n = 3 * x * x - y * y;
            if (x > y && n <= limit && n % 12 == 11) {
                isPrime[n] = !isPrime[n];
            }
        }
    }
    
    // Mark multiples of squares as non-prime
    for (int i = 5; i * i <= limit; i++) {
        if (isPrime[i]) {
            for (int j = i * i; j <= limit; j += i * i) {
                isPrime[j] = 0;
            }
        }
    }
    
    // Print all prime numbers
    printf("Prime numbers up to %d:\n", limit);
    for (int i = 2; i <= limit; i++) {
        if (isPrime[i]) {
            printf("%d ", i);
        }
    }
    printf("\n");
    
    free(isPrime);
}

int main() {
    int limit;
    
    printf("Enter the limit to find primes: ");
    scanf("%d", &limit);
    
    if (limit < 2) {
        printf("No primes to display.\n");
        return 0;
    }
    
    sieveOfAtkin(limit);
    
    return 0;
}
```

## How it works:

1. **Initialization**: Create a boolean array `isPrime` where `isPrime[i]` represents whether `i` is prime
2. **Mark potential primes**: Use three quadratic forms to mark numbers that might be prime:
   - `4x² + y² = n` where `n % 12 = 1 or 5`
   - `3x² + y² = n` where `n % 12 = 7`
   - `3x² - y² = n` where `x > y` and `n % 12 = 11`
3. **Toggle flags**: For each marked number, toggle its flag (using XOR)
4. **Remove multiples**: Mark all multiples of squares as non-prime
5. **Output**: Print all numbers that remain marked as prime

## Example Output:
```
Enter the limit to find primes: 30
Prime numbers up to 30:
2 3 5 7 11 13 17 19 23 29 
```

## Time Complexity:
- **Time**: O(n / log log n)
- **Space**: O(n)

The Sieve of Atkin is particularly efficient for finding large prime numbers and has better theoretical complexity than the Sieve of Eratosthenes for sufficiently large ranges.

