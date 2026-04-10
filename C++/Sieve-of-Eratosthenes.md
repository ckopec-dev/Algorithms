# Sieve of Eratosthenes in C++

```cpp
#include <iostream>
#include <vector>
using namespace std;

void sieveOfEratosthenes(int n) {
    // Create a boolean array and initialize all entries as true
    vector<bool> prime(n + 1, true);
    
    // 0 and 1 are not prime numbers
    prime[0] = prime[1] = false;
    
    // Start with the first prime number, 2
    for (int p = 2; p * p <= n; p++) {
        if (prime[p]) {
            // Mark all multiples of p as not prime
            for (int i = p * p; i <= n; i += p) {
                prime[i] = false;
            }
        }
    }
    
    // Print all prime numbers
    cout << "Prime numbers up to " << n << " are: ";
    for (int i = 2; i <= n; i++) {
        if (prime[i]) {
            cout << i << " ";
        }
    }
    cout << endl;
}

int main() {
    int n = 30;
    sieveOfEratosthenes(n);
    return 0;
}
```

## Output
```
Prime numbers up to 30 are: 2 3 5 7 11 13 17 19 23 29
```

## How it works:

1. **Initialize**: Create a boolean array of size n+1, initialize all values to `true`
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as `false`
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Result**: All numbers still marked as `true` are prime

## Time Complexity: O(n log log n)
## Space Complexity: O(n)

