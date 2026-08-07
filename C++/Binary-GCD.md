# Binary GCD Algorithm in C++

The Binary GCD algorithm (also known as Stein's algorithm) is an efficient method for computing the greatest common divisor (GCD) of two numbers using bitwise operations instead of division.

## Implementation

```cpp
#include <iostream>
using namespace std;

// Binary GCD algorithm implementation
int binary_gcd(int a, int b) {
    // Handle edge cases
    if (a == 0) return b;
    if (b == 0) return a;
    
    // Make sure both numbers are positive
    a = abs(a);
    b = abs(b);
    
    // Count common factors of 2
    int shift = 0;
    while (((a | b) & 1) == 0) {
        a >>= 1;
        b >>= 1;
        shift++;
    }
    
    // Remove remaining factors of 2 from a
    while ((a & 1) == 0) {
        a >>= 1;
    }
    
    while (b != 0) {
        // Remove factors of 2 from b
        while ((b & 1) == 0) {
            b >>= 1;
        }
        
        // Ensure a <= b
        if (a > b) {
            swap(a, b);
        }
        
        // Subtract smaller from larger
        b = b - a;
    }
    
    // Restore common factors of 2
    return a << shift;
}

// Alternative recursive implementation
int binary_gcd_recursive(int a, int b) {
    if (a == 0) return b;
    if (b == 0) return a;
    
    a = abs(a);
    b = abs(b);
    
    // If both are even
    if ((a & 1) == 0 && (b & 1) == 0) {
        return binary_gcd_recursive(a >> 1, b >> 1) << 1;
    }
    // If a is even and b is odd
    else if ((a & 1) == 0) {
        return binary_gcd_recursive(a >> 1, b);
    }
    // If a is odd and b is even
    else if ((b & 1) == 0) {
        return binary_gcd_recursive(a, b >> 1);
    }
    // If both are odd
    else {
        if (a > b) {
            return binary_gcd_recursive(a - b, b);
        } else {
            return binary_gcd_recursive(a, b - a);
        }
    }
}

int main() {
    int num1, num2;
    
    cout << "Enter two numbers: ";
    cin >> num1 >> num2;
    
    cout << "GCD of " << num1 << " and " << num2 << " is: " 
         << binary_gcd(num1, num2) << endl;
    
    cout << "Using recursive version: " 
         << binary_gcd_recursive(num1, num2) << endl;
    
    // Test cases
    cout << "\nTest cases:" << endl;
    cout << "GCD(48, 18) = " << binary_gcd(48, 18) << endl;   // Expected: 6
    cout << "GCD(100, 25) = " << binary_gcd(100, 25) << endl; // Expected: 25
    cout << "GCD(17, 13) = " << binary_gcd(17, 13) << endl;   // Expected: 1
    
    return 0;
}
```

## How it works:

1. **Handle edge cases**: If either number is 0, return the other number
2. **Remove common factors of 2**: Count how many times both numbers can be divided by 2
3. **Remove remaining factors of 2 from one number**: Make sure at least one number becomes odd
4. **Subtract and shift**: Repeatedly subtract the smaller number from the larger and remove factors of 2
5. **Restore result**: Multiply back the common factors of 2 that were removed

## Time Complexity:
- **Time**: O(log(min(a,b)))
- **Space**: O(1) for iterative, O(log(min(a,b))) for recursive

## Example Output:
```
Enter two numbers: 48 18
GCD of 48 and 18 is: 6
Using recursive version: 6

Test cases:
GCD(48, 18) = 6
GCD(100, 25) = 25
GCD(17, 13) = 1
```