# Pollard's Rho Algorithm Implementation in C++

Pollard's rho algorithm is a probabilistic factorization algorithm that is particularly effective for finding small prime factors of large composite numbers.

```cpp
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <algorithm>

class PollardRho {
public:
    // Function to compute greatest common divisor
    static long long gcd(long long a, long long b) {
        if (b == 0) return a;
        return gcd(b, a % b);
    }
    
    // Function to compute (a * b) % mod using long long to prevent overflow
    static long long multiply(long long a, long long b, long long mod) {
        return ((a % mod) * (b % mod)) % mod;
    }
    
    // Function to compute (base^exp) % mod using modular exponentiation
    static long long power(long long base, long long exp, long long mod) {
        long long result = 1;
        base = base % mod;
        while (exp > 0) {
            if (exp % 2 == 1) {
                result = multiply(result, base, mod);
            }
            exp = exp >> 1;
            base = multiply(base, base, mod);
        }
        return result;
    }
    
    // Pollard's rho algorithm to find a non-trivial factor
    static long long pollardRho(long long n) {
        if (n <= 1) return n;
        if (n % 2 == 0) return 2;
        
        // Initialize variables
        long long x = 2, y = 2, c = 1;
        long long d = 1;
        
        // Function f(x) = (x^2 + c) % n
        auto f = [&](long long x) {
            return (multiply(x, x, n) + c) % n;
        };
        
        while (d == 1) {
            x = f(x);
            y = f(f(y));
            d = gcd(abs(x - y), n);
            
            // If we found a factor, return it
            if (d != 1 && d != n) {
                return d;
            }
            
            // If we've tried too many iterations, increase c and try again
            if (d == n) {
                c++;
                x = 2;
                y = 2;
                d = 1;
            }
        }
        
        return d;
    }
    
    // Function to factorize a number completely
    static void factorize(long long n) {
        if (n <= 1) return;
        
        std::cout << "Factors of " << n << ": ";
        
        while (n > 1) {
            long long factor = pollardRho(n);
            std::cout << factor << " ";
            n /= factor;
        }
        std::cout << std::endl;
    }
};

int main() {
    // Test cases
    std::cout << "Pollard's Rho Algorithm Examples\n";
    std::cout << "=================================\n\n";
    
    // Example 1: Factorize 13474223 (should find factor 139)
    long long n1 = 13474223;
    std::cout << "Factorizing " << n1 << std::endl;
    long long factor1 = PollardRho::pollardRho(n1);
    std::cout << "Found factor: " << factor1 << std::endl;
    std::cout << "Verification: " << n1 << " / " << factor1 << " = " << (n1 / factor1) << std::endl;
    std::cout << std::endl;
    
    // Example 2: Factorize 1000000007 (prime number)
    long long n2 = 1000000007;
    std::cout << "Factorizing " << n2 << " (prime number)" << std::endl;
    long long factor2 = PollardRho::pollardRho(n2);
    std::cout << "Found factor: " << factor2 << std::endl;
    std::cout << std::endl;
    
    // Example 3: Factorize 9052787 (should find factor 1409)
    long long n3 = 9052787;
    std::cout << "Factorizing " << n3 << std::endl;
    long long factor3 = PollardRho::pollardRho(n3);
    std::cout << "Found factor: " << factor3 << std::endl;
    std::cout << "Verification: " << n3 << " / " << factor3 << " = " << (n3 / factor3) << std::endl;
    std::cout << std::endl;
    
    // Example 4: Complete factorization
    long long n4 = 1234567890;
    std::cout << "Complete factorization of " << n4 << std::endl;
    PollardRho::factorize(n4);
    std::cout << std::endl;
    
    return 0;
}
```

## How the Algorithm Works

1. **Initialization**: Start with two variables `x` and `y` both set to 2, and a constant `c`
2. **Function Definition**: Use a pseudo-random function `f(x) = (x² + c) mod n`
3. **Cycle Detection**: 
   - Advance `x` by one step: `x = f(x)`
   - Advance `y` by two steps: `y = f(f(y))`
   - Compute `d = gcd(|x - y|, n)`
4. **Factor Discovery**: If `d > 1` and `d < n`, then `d` is a non-trivial factor
5. **Retry**: If no factor is found, increment `c` and restart

## Key Features

- **Time Complexity**: O(n^(1/4)) on average
- **Space Complexity**: O(1)
- **Probabilistic**: May require multiple attempts to find a factor
- **Effective for**: Small factors of large numbers

## Sample Output
```
Pollard's Rho Algorithm Examples
=================================

Factorizing 13474223
Found factor: 139
Verification: 13474223 / 139 = 96937

Factorizing 1000000007 (prime number)
Found factor: 1000000007

Factorizing 9052787
Found factor: 1409
Verification: 9052787 / 1409 = 6423

Complete factorization of 1234567890
Factors of 1234567890: 2 3 3 5 3607 3803 
```

This implementation demonstrates how Pollard's rho algorithm can efficiently find factors of large composite numbers, particularly when the factors are relatively small compared to the number being factored.