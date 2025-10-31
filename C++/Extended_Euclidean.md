# Extended Euclidean Algorithm in C++

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two numbers, but also finds the coefficients (x and y) such that ax + by = gcd(a,b).

## Code Implementation

```cpp
#include <iostream>
using namespace std;

// Function to implement Extended Euclidean Algorithm
// Returns GCD and stores x, y such that ax + by = gcd(a,b)
int extendedGCD(int a, int b, int &x, int &y) {
    // Base case
    if (b == 0) {
        x = 1;
        y = 0;
        return a;
    }
    
    // Recursive call
    int gcd = extendedGCD(b, a % b, x, y);
    
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
    
    int gcd = extendedGCD(a, b, x, y);
    
    cout << "GCD: " << gcd << endl;
    cout << "Coefficients: x = " << x << ", y = " << y << endl;
    cout << "Verification: " << a << "*(" << x << ") + " << b << "*(" << y << ") = " 
         << a*x + b*y << endl;
    
    return 0;
}
```

## Sample Output

```
Finding GCD of 35 and 15
GCD: 5
Coefficients: x = 1, y = -2
Verification: 35*(1) + 15*(-2) = 5
```

## How it Works

1. **Base Case**: When b = 0, gcd(a, 0) = a, so x = 1, y = 0
2. **Recursive Step**: For gcd(a, b), we recursively find gcd(b, a%b)
3. **Back Substitution**: Using the relationship: if gcd(b, a%b) = bx' + (a%b)y', then:
   - x = y'
   - y = x' - (a/b) * y'

## Key Points

- Time Complexity: O(log(min(a,b)))
- Space Complexity: O(log(min(a,b))) due to recursion stack
- The algorithm is useful for solving linear Diophantine equations and finding modular inverses
- The coefficients x and y satisfy the equation ax + by = gcd(a,b)

This implementation correctly finds both the GCD and the Bézout coefficients that satisfy the extended Euclidean equation.

