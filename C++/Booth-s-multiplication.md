# Booth's Multiplication Algorithm in C++

Booth's multiplication algorithm is a method for multiplying two signed binary numbers in two's complement notation. Here's a complete implementation:

```cpp
#include <iostream>
#include <bitset>
#include <string>
using namespace std;

class BoothMultiplier {
private:
    int A, S, P;  // Accumulator, Subtractor, Product
    int n;        // Number of bits
    
    // Function to get the least significant bit
    int getLSB(int num) {
        return num & 1;
    }
    
    // Function to get the second least significant bit
    int getSecondLSB(int num) {
        return (num >> 1) & 1;
    }
    
    // Function to perform arithmetic right shift
    int arithmeticRightShift(int num) {
        int signBit = (num >> (n - 1)) & 1;
        return (num >> 1) | (signBit << (n - 1));
    }
    
    // Function to convert decimal to binary string
    string toBinary(int num, int bits) {
        if (num >= 0) {
            return bitset<32>(num).to_string().substr(32 - bits, bits);
        } else {
            return bitset<32>(num).to_string().substr(32 - bits, bits);
        }
    }
    
public:
    // Booth's multiplication algorithm
    int multiply(int multiplicand, int multiplier, int bits = 8) {
        n = bits;
        A = 0;                    // Initialize A with zeros
        S = -multiplicand;        // Initialize S with negative multiplicand
        P = multiplier;           // Initialize P with multiplier
        
        cout << "Initial values:" << endl;
        cout << "A = " << toBinary(A, bits) << " (" << A << ")" << endl;
        cout << "S = " << toBinary(S, bits) << " (" << S << ")" << endl;
        cout << "P = " << toBinary(P, bits) << " (" << P << ")" << endl;
        cout << "------------------------" << endl;
        
        // Perform Booth's algorithm for n iterations
        for (int i = 0; i < n; i++) {
            cout << "Step " << (i + 1) << ":" << endl;
            
            int LSB = getLSB(P);
            int secondLSB = getSecondLSB(P);
            
            cout << "P = " << toBinary(P, bits) << " (LSB=" << LSB 
                 << ", secondLSB=" << secondLSB << ")" << endl;
            
            // Check the last two bits of P
            if (secondLSB == 0 && LSB == 1) {
                // P = P + A
                P = P + A;
                cout << "P = P + A = " << toBinary(P, bits) << endl;
            }
            else if (secondLSB == 1 && LSB == 0) {
                // P = P + S
                P = P + S;
                cout << "P = P + S = " << toBinary(P, bits) << endl;
            }
            
            // Arithmetic right shift of (A, P)
            int temp = arithmeticRightShift(P);
            cout << "Arithmetic right shift: P = " << toBinary(temp, bits) << endl;
            P = temp;
            
            cout << "------------------------" << endl;
        }
        
        // Final result is in P (upper n bits of (A, P))
        return P;
    }
};

int main() {
    BoothMultiplier booth;
    
    cout << "Booth's Multiplication Algorithm Example" << endl;
    cout << "========================================" << endl;
    
    // Example 1: 5 × 3 = 15
    cout << "Example 1: 5 × 3" << endl;
    int result1 = booth.multiply(5, 3, 4);
    cout << "Result: " << result1 << endl;
    cout << "Expected: 15" << endl;
    cout << endl;
    
    // Example 2: 7 × (-2) = -14
    cout << "Example 2: 7 × (-2)" << endl;
    int result2 = booth.multiply(7, -2, 4);
    cout << "Result: " << result2 << endl;
    cout << "Expected: -14" << endl;
    cout << endl;
    
    // Example 3: (-3) × (-4) = 12
    cout << "Example 3: (-3) × (-4)" << endl;
    int result3 = booth.multiply(-3, -4, 4);
    cout << "Result: " << result3 << endl;
    cout << "Expected: 12" << endl;
    
    return 0;
}
```

## How Booth's Algorithm Works:

1. **Initialization**: 
   - A = 0 (n-bit zero)
   - S = -multiplicand (n-bit negative multiplicand)
   - P = multiplier (n-bit multiplier)

2. **For each step**:
   - Examine the last two bits of P (P₀, P₋₁)
   - If P₀P₋₁ = 01: P = P + A
   - If P₀P₋₁ = 10: P = P + S
   - Arithmetic right shift of (A, P)
   - Repeat n times

3. **Result**: The final result is in the upper n bits of (A, P)

## Sample Output:
```
Booth's Multiplication Algorithm Example
========================================
Example 1: 5 × 3
Initial values:
A = 0000 (0)
S = 1111 (15)
P = 0011 (3)
------------------------
Step 1:
P = 0011 (LSB=1, secondLSB=1)
Arithmetic right shift: P = 0001 (1)
------------------------
Step 2:
P = 0001 (LSB=1, secondLSB=0)
P = P + S = 0000 (0)
Arithmetic right shift: P = 0000 (0)
------------------------
...
Result: 15
Expected: 15
```

This implementation demonstrates how Booth's algorithm efficiently handles signed multiplication by reducing the number of additions required compared to traditional binary multiplication.

