# Euclidean Algorithm in C++

The Euclidean algorithm is an efficient method for computing the greatest common divisor (GCD) of two integers.

## Iterative Implementation

```cpp
#include <iostream>
using namespace std;

// Iterative implementation of Euclidean algorithm
int gcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

int main() {
    int num1 = 48, num2 = 18;
    
    cout << "GCD of " << num1 << " and " << num2 << " is: " 
         << gcd(num1, num2) << endl;
    
    return 0;
}
```

## Recursive Implementation

```cpp
#include <iostream>
using namespace std;

// Recursive implementation of Euclidean algorithm
int gcd_recursive(int a, int b) {
    if (b == 0)
        return a;
    return gcd_recursive(b, a % b);
}

int main() {
    int num1 = 48, num2 = 18;
    
    cout << "GCD of " << num1 << " and " << num2 << " is: " 
         << gcd_recursive(num1, num2) << endl;
    
    return 0;
}
```

## Complete Example with User Input

```cpp
#include <iostream>
using namespace std;

int gcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

int main() {
    int num1, num2;
    
    cout << "Enter two numbers: ";
    cin >> num1 >> num2;
    
    // Handle negative numbers
    num1 = abs(num1);
    num2 = abs(num2);
    
    cout << "GCD of " << num1 << " and " << num2 << " is: " 
         << gcd(num1, num2) << endl;
    
    return 0;
}
```

## Sample Output
```
Enter two numbers: 48 18
GCD of 48 and 18 is: 6
```

## How it works:
1. **Step 1**: Divide 48 by 18 → remainder = 12
2. **Step 2**: Divide 18 by 12 → remainder = 6  
3. **Step 3**: Divide 12 by 6 → remainder = 0
4. **Result**: GCD = 6

The algorithm works because GCD(a,b) = GCD(b, a mod b) until one number becomes 0.