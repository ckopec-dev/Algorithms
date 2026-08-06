# Binary Exponentiation Algorithm in C++

Binary exponentiation is an efficient algorithm to compute large powers of a number with time complexity O(log n).

## Code Implementation

```cpp
#include <iostream>
using namespace std;

// Iterative binary exponentiation
long long binaryExponentiation(long long base, long long exp) {
    long long result = 1;
    
    while (exp > 0) {
        // If exp is odd, multiply base with result
        if (exp % 2 == 1) {
            result = result * base;
        }
        // Square the base and halve the exponent
        base = base * base;
        exp = exp / 2;
    }
    
    return result;
}

// Recursive binary exponentiation
long long binaryExpRecursive(long long base, long long exp) {
    if (exp == 0) {
        return 1;
    }
    
    long long half = binaryExpRecursive(base, exp / 2);
    
    if (exp % 2 == 0) {
        return half * half;
    } else {
        return base * half * half;
    }
}

int main() {
    long long base, exponent;
    
    cout << "Enter base and exponent: ";
    cin >> base >> exponent;
    
    long long result1 = binaryExponentiation(base, exponent);
    long long result2 = binaryExpRecursive(base, exponent);
    
    cout << base << "^" << exponent << " = " << result1 << endl;
    cout << base << "^" << exponent << " = " << result2 << endl;
    
    return 0;
}
```

## How it works:

**Algorithm Steps:**
1. Initialize `result = 1`
2. While `exp > 0`:
   - If `exp` is odd, multiply `result` by `base`
   - Square the `base` and halve the `exp`
3. Return `result`

**Example:** Calculate 3^10
```
3^10 = (3^5)² = ((3^2)² × 3)² = (((3×3)² × 3)²)
```

## Time Complexity:
- **Time:** O(log n)
- **Space:** O(1) for iterative, O(log n) for recursive

## Sample Output:
```
Enter base and exponent: 3 10
3^10 = 59049
3^10 = 59049
```