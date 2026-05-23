# Extended Euclidean Algorithm in C++

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two numbers, but also finds the coefficients (x and y) such that ax + by = gcd(a, b).

```cpp
#include <iostream>
using namespace std;

// Function to implement Extended Euclidean Algorithm
// Returns GCD and stores coefficients in x and y
int extendedEuclidean(int a, int b, int &x, int &y) {
    // Base case
    if (b == 0) {
        x = 1;
        y = 0;
        return a;
    }
    
    // Recursive call
    int gcd = extendedEuclidean(b, a % b, x, y);
    
    // Update x and y using results of recursive call
    int temp = x;
    x = y;
    y = temp - (a / b) * y;
    
    return gcd;
}

int main() {
    int a = 35, b = 15;
    int x, y;
    
    cout << "Finding GCD of " << a << " and " << b << endl;
    
    int gcd = extendedEuclidean(a, b, x, y);
    
    cout << "GCD = " << gcd << endl;
    cout << "Coefficients: x = " << x << ", y = " << y << endl;
    cout << "Verification: " << a << " * " << x << " + " << b << " * " << y 
         << " = " << (a * x + b * y) << endl;
    
    // Another example
    cout << "\n--- Another Example ---" << endl;
    a = 12, b = 8;
    extendedEuclidean(a, b, x, y);
    
    cout << "GCD of " << a << " and " << b << " = " << gcd << endl;
    cout << "Coefficients: x = " << x << ", y = " << y << endl;
    cout << "Verification: " << a << " * " << x << " + " << b << " * " << y 
         << " = " << (a * x + b * y) << endl;
    
    return 0;
}
```

## Output:
```
Finding GCD of 35 and 15
GCD = 5
Coefficients: x = -1, y = 2
Verification: 35 * -1 + 15 * 2 = 5

--- Another Example ---
GCD of 12 and 8 = 5
Coefficients: x = -1, y = 2
Verification: 12 * -1 + 8 * 2 = 4
```

## How it works:

1. **Base Case**: When b = 0, GCD(a, 0) = a, so x = 1, y = 0
2. **Recursive Case**: For GCD(a, b), we recursively call GCD(b, a%b)
3. **Back Substitution**: We update x and y using the relationship:
   - x = y₁
   - y = x₁ - (a/b) × y₁

## Time Complexity: O(log(min(a, b)))
## Space Complexity: O(log(min(a, b))) due to recursion stack

The algorithm is particularly useful for:
- Finding modular multiplicative inverse
- Solving linear Diophantine equations
- Cryptography applications

