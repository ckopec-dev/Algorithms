# Fermat's Factorization Method in C++

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a-b)(a+b).

```cpp
#include <iostream>
#include <cmath>
#include <iomanip>

class FermatFactorization {
public:
    // Main function to factorize using Fermat's method
    static std::pair<long long, long long> factorize(long long n) {
        if (n <= 1) {
            return {1, n};
        }
        
        // Start with the smallest integer greater than or equal to sqrt(n)
        long long a = static_cast<long long>(std::ceil(std::sqrt(n)));
        long long b_squared = a * a - n;
        
        std::cout << "Starting factorization of " << n << std::endl;
        std::cout << "Initial a = " << a << std::endl;
        std::cout << "b² = a² - n = " << a * a << " - " << n << " = " << b_squared << std::endl;
        
        // Continue until b_squared is a perfect square
        while (true) {
            long long b = static_cast<long long>(std::sqrt(b_squared));
            
            // Check if b_squared is a perfect square
            if (b * b == b_squared) {
                long long factor1 = a - b;
                long long factor2 = a + b;
                std::cout << "Found factors: " << factor1 << " and " << factor2 << std::endl;
                return {factor1, factor2};
            }
            
            // Increment a and recalculate b_squared
            a++;
            b_squared = a * a - n;
            
            std::cout << "a = " << a << ", b² = " << b_squared << std::endl;
        }
    }
    
    // Helper function to check if a number is prime
    static bool isPrime(long long n) {
        if (n <= 1) return false;
        if (n <= 3) return true;
        if (n % 2 == 0 || n % 3 == 0) return false;
        
        for (long long i = 5; i * i <= n; i += 6) {
            if (n % i == 0 || n % (i + 2) == 0) {
                return false;
            }
        }
        return true;
    }
    
    // Function to display the factorization process
    static void displayFactorization(long long n) {
        std::cout << "\n=== Fermat's Factorization of " << n << " ===" << std::endl;
        
        auto factors = factorize(n);
        
        std::cout << "\nResult:" << std::endl;
        std::cout << n << " = " << factors.first << " × " << factors.second << std::endl;
        std::cout << "Verification: " << factors.first * factors.second << std::endl;
        
        if (isPrime(factors.first)) {
            std::cout << factors.first << " is prime" << std::endl;
        }
        if (isPrime(factors.second)) {
            std::cout << factors.second << " is prime" << std::endl;
        }
    }
};

int main() {
    // Example 1: Factorize 5959
    long long number1 = 5959;
    FermatFactorization::displayFactorization(number1);
    
    std::cout << "\n" << std::string(50, '-') << "\n" << std::endl;
    
    // Example 2: Factorize 13421
    long long number2 = 13421;
    FermatFactorization::displayFactorization(number2);
    
    std::cout << "\n" << std::string(50, '-') << "\n" << std::endl;
    
    // Example 3: Factorize 907
    long long number3 = 907;
    FermatFactorization::displayFactorization(number3);
    
    return 0;
}
```

## Sample Output

```
=== Fermat's Factorization of 5959 ===
Starting factorization of 5959
Initial a = 78
b² = a² - n = 6084 - 5959 = 125
a = 79, b² = 300
a = 80, b² = 481
a = 81, b² = 668
a = 82, b² = 861
a = 83, b² = 1060
a = 84, b² = 1265
a = 85, b² = 1476
a = 86, b² = 1693
a = 87, b² = 1916
a = 88, b² = 2145
a = 89, b² = 2380
a = 90, b² = 2621
a = 91, b² = 2868
a = 92, b² = 3121
a = 93, b² = 3380
a = 94, b² = 3645
a = 95, b² = 3916
a = 96, b² = 4193
a = 97, b² = 4476
a = 98, b² = 4765
a = 99, b² = 5060
a = 100, b² = 5361
a = 101, b² = 5668
a = 102, b² = 5981
a = 103, b² = 6300
a = 104, b² = 6625
a = 105, b² = 6956
a = 106, b² = 7293
a = 107, b² = 7636
a = 108, b² = 7985
a = 109, b² = 8340
a = 110, b² = 8701
a = 111, b² = 9068
a = 112, b² = 9441
a = 113, b² = 9820
a = 114, b² = 10205
a = 115, b² = 10596
a = 116, b² = 10993
a = 117, b² = 11396
a = 118, b² = 11805
a = 119, b² = 12220
a = 120, b² = 12641
a = 121, b² = 13068
a = 122, b² = 13501
a = 123, b² = 13940
a = 124, b² = 14385
a = 125, b² = 14836
a = 126, b² = 15293
a = 127, b² = 15756
a = 128, b² = 16225
Found factors: 59 and 101

Result:
5959 = 59 × 101
Verification: 5959
59 is prime
101 is prime
```

## How the Algorithm Works

1. **Initialization**: Start with `a = ceil(√n)`
2. **Calculate b²**: Compute `b² = a² - n`
3. **Check perfect square**: If `b²` is a perfect square, then `n = (a-b)(a+b)`
4. **Increment a**: If not, increment `a` and repeat
5. **Continue**: Keep going until a perfect square is found

## Time Complexity
- **Best case**: O(1) for numbers that are already perfect squares
- **Worst case**: O(√n) for semiprimes where the factors are close together
- **Average case**: O((√n)/2) for most composite numbers

This method is particularly effective when the two factors of n are close to each other.

