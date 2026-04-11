# Sieve of Atkin Algorithm in C++

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer, which is more efficient than the traditional Sieve of Eratosthenes for large numbers.

```cpp
#include <iostream>
#include <vector>
#include <cmath>

class SieveOfAtkin {
private:
    std::vector<bool> isPrime;
    int limit;

public:
    SieveOfAtkin(int n) : limit(n) {
        isPrime.assign(n + 1, false);
    }

    void generatePrimes() {
        if (limit < 2) return;
        
        // Initialize base primes
        isPrime[2] = isPrime[3] = true;
        
        // Mark squares of primes as non-prime
        for (int i = 5; i * i <= limit; i++) {
            if (isPrime[i]) {
                for (int j = i * i; j <= limit; j += i) {
                    isPrime[j] = false;
                }
            }
        }
        
        // Apply Sieve of Atkin algorithm
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
        
        // Remove multiples of squares
        for (int i = 5; i * i <= limit; i++) {
            if (isPrime[i]) {
                for (int j = i * i; j <= limit; j += i * i) {
                    isPrime[j] = false;
                }
            }
        }
    }

    void printPrimes() {
        std::cout << "Prime numbers up to " << limit << ":\n";
        for (int i = 2; i <= limit; i++) {
            if (isPrime[i]) {
                std::cout << i << " ";
            }
        }
        std::cout << std::endl;
    }

    std::vector<int> getPrimes() {
        std::vector<int> primes;
        for (int i = 2; i <= limit; i++) {
            if (isPrime[i]) {
                primes.push_back(i);
            }
        }
        return primes;
    }
};

int main() {
    int n = 30;
    
    std::cout << "Sieve of Atkin Algorithm Example\n";
    std::cout << "================================\n\n";
    
    SieveOfAtkin sieve(n);
    sieve.generatePrimes();
    sieve.printPrimes();
    
    // Get primes as vector
    std::vector<int> primes = sieve.getPrimes();
    std::cout << "\nNumber of primes found: " << primes.size() << std::endl;
    
    return 0;
}
```

## Output:
```
Sieve of Atkin Algorithm Example
================================

Prime numbers up to 30:
2 3 5 7 11 13 17 19 23 29 

Number of primes found: 10
```

## How it works:

1. **Initialization**: Create a boolean array to track prime numbers
2. **Base cases**: Mark 2 and 3 as primes
3. **Main algorithm**:
   - For each pair of numbers (x,y), calculate n = 4x² + y²
   - If n ≤ limit and n % 12 is 1 or 5, toggle the prime flag
   - Similarly check n = 3x² + y² (n % 12 = 7) and n = 3x² - y² (x > y, n % 12 = 11)
4. **Remove squares**: Eliminate multiples of square numbers
5. **Output**: Collect all marked primes

## Time Complexity: O(n / log log n)
## Space Complexity: O(n)

The Sieve of Atkin is particularly efficient for finding large prime numbers and has better asymptotic complexity than the Sieve of Eratosthenes for large ranges.

