# Pollard's Rho Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to compute greatest common divisor
long long gcd(long long a, long long b) {
    if (b == 0)
        return a;
    return gcd(b, a % b);
}

// Function to perform Pollard's Rho algorithm
long long pollard_rho(long long n) {
    if (n % 2 == 0)
        return 2;
    
    long long x = 2;
    long long y = 2;
    long long c = 1;
    long long d = 1;
    
    // Function f(x) = (x^2 + c) mod n
    long long f(long long x) {
        return ((x * x) % n + c) % n;
    }
    
    while (d == 1) {
        x = f(x);
        y = f(f(y));
        d = gcd(abs(x - y), n);
    }
    
    return d;
}

// Function to perform trial division for small factors
long long trial_division(long long n) {
    if (n <= 1) return n;
    
    // Check for factor 2
    if (n % 2 == 0) return 2;
    
    // Check for odd factors from 3 onwards
    for (long long i = 3; i * i <= n; i += 2) {
        if (n % i == 0) {
            return i;
        }
    }
    
    return n;
}

// Complete factorization function
void factorize(long long n) {
    if (n <= 1) {
        printf("No factors for %lld\n", n);
        return;
    }
    
    printf("Factors of %lld: ", n);
    
    while (n > 1) {
        long long factor = pollard_rho(n);
        
        // If factor is not prime, we need to factorize it further
        if (factor == n) {
            printf("%lld ", factor);
            break;
        } else {
            printf("%lld ", factor);
            n /= factor;
        }
    }
    
    printf("\n");
}

int main() {
    // Test cases
    long long test_numbers[] = {13407807929, 152415787501905210000000000000000000000000000000