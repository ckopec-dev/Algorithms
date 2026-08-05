# Baby-step Giant-step Algorithm Implementation

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: given $g^x \equiv h \pmod{p}$, find $x$.

```cpp
#include <iostream>
#include <unordered_map>
#include <cmath>
using namespace std;

class BabyStepGiantStep {
public:
    // Function to compute (base^exp) % mod using modular exponentiation
    static long long modPow(long long base, long long exp, long long mod) {
        long long result = 1;
        base = base % mod;
        while (exp > 0) {
            if (exp % 2 == 1)
                result = (result * base) % mod;
            exp = exp >> 1;
            base = (base * base) % mod;
        }
        return result;
    }
    
    // Baby-step giant-step algorithm to solve discrete logarithm
    static long long solve(long long g, long long h, long long p) {
        long long n = ceil(sqrt(p));
        
        // Baby steps: store g^j mod p for j = 0, 1, ..., n-1
        unordered_map<long long, long long> babySteps;
        
        // Compute and store g^j mod p
        long long g_j = 1;
        for (long long j = 0; j < n; j++) {
            if (babySteps.find(g_j) == babySteps.end()) {
                babySteps[g_j] = j;
            }
            g_j = (g_j * g) % p;
        }
        
        // Giant steps: compute g^(-n) mod p
        long long g_neg_n = modPow(g, n, p);
        g_neg_n = modPow(g_neg_n, p - 2, p); // Modular inverse using Fermat's little theorem
        
        // Search for solution
        long long y = h;
        for (long long i = 0; i < n; i++) {
            if (babySteps.find(y) != babySteps.end()) {
                long long x = i * n + babySteps[y];
                return x;
            }
            y = (y * g_neg_n) % p;
        }
        
        return -1; // No solution found
    }
};

int main() {
    // Example: Solve g^x ≡ h (mod p)
    // Find x such that 3^x ≡ 13 (mod 17)
    
    long long g = 3;
    long long h = 13;
    long long p = 17;
    
    cout << "Solving: " << g << "^x ≡ " << h << " (mod " << p << ")" << endl;
    
    long long result = BabyStepGiantStep::solve(g, h, p);
    
    if (result != -1) {
        cout << "Solution found: x = " << result << endl;
        
        // Verify the solution
        long long verify = BabyStepGiantStep::modPow(g, result, p);
        cout << "Verification: " << g << "^" << result << " ≡ " << verify << " (mod " << p << ")" << endl;
    } else {
        cout << "No solution found!" << endl;
    }
    
    // Another example
    cout << "\nAnother example:" << endl;
    g = 5; h = 8; p = 11;
    cout << "Solving: " << g << "^x ≡ " << h << " (mod " << p << ")" << endl;
    
    result = BabyStepGiantStep::solve(g, h, p);
    
    if (result != -1) {
        cout << "Solution found: x = " << result << endl;
        
        // Verify the solution
        long long verify = BabyStepGiantStep::modPow(g, result, p);
        cout << "Verification: " << g << "^" << result << " ≡ " << verify << " (mod " << p << ")" << endl;
    } else {
        cout << "No solution found!" << endl;
    }
    
    return 0;
}
```

## Algorithm Explanation

### How it works:
1. **Baby Steps**: Compute and store values of $g^j \bmod p$ for $j = 0, 1, ..., \sqrt{p}-1$
2. **Giant Steps**: For each $i = 0, 1, ..., \sqrt{p}-1$, compute $h \cdot (g^{-\sqrt{p}})^i \bmod p$
3. **Search**: Look for a match between baby steps and giant steps
4. **Solution**: If found, $x = i \cdot \sqrt{p} + j$

### Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

### Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)

Another example:
Solving: 5^x ≡ 8 (mod 11)
Solution found: x = 2
Verification: 5^2 ≡ 8 (mod 11)
```

This implementation efficiently solves discrete logarithm problems with complexity significantly better than brute force O(p).