# Sieve of Eratosthenes in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void sieveOfEratosthenes(int n) {
    // Create a boolean array and initialize all entries as true
    bool *prime = (bool*)malloc((n + 1) * sizeof(bool));
    
    // Initialize all numbers as prime (true)
    for (int i = 0; i <= n; i++) {
        prime[i] = true;
    }
    
    // 0 and 1 are not prime numbers
    prime[0] = prime[1] = false;
    
    // Start with the first prime number, 2
    for (int p = 2; p * p <= n; p++) {
        if (prime[p] == true) {
            // Mark all multiples of p as not prime
            for (int i = p * p; i <= n; i += p) {
                prime[i] = false;
            }
        }
    }
    
    // Print all prime numbers
    printf("Prime numbers up to %d:\n", n);
    for (int i = 2; i <= n; i++) {
        if (prime[i] == true) {
            printf("%d ", i);
        }
    }
    printf("\n");
    
    // Free allocated memory
    free(prime);
}

int main() {
    int n;
    
    printf("Enter the number up to which you want to find primes: ");
    scanf("%d", &n);
    
    sieveOfEratosthenes(n);
    
    return 0;
}
```

## Example Output:
```
Enter the number up to which you want to find primes: 30
Prime numbers up to 30:
2 3 5 7 11 13 17 19 23 29 
```

## How it works:

1. **Initialization**: Create a boolean array of size n+1 and initialize all entries as true
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as false
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Output**: Print all numbers that remain marked as true

## Time Complexity: O(n log log n)
## Space Complexity: O(n)