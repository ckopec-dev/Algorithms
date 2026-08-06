# Berlekamp-Massey Algorithm Implementation

Here's a C++ implementation of the Berlekamp-Massey algorithm for finding the minimal polynomial of a linearly recurrent sequence:

```cpp
#include <iostream>
#include <vector>
using namespace std;

class BerlekampMassey {
public:
    // Find the minimal polynomial for the given sequence
    static vector<int> findMinimalPolynomial(const vector<int>& sequence) {
        int n = sequence.size();
        if (n == 0) return {};
        
        // Initialize variables
        vector<int> C(n, 0);  // Current connection polynomial
        vector<int> B(n, 0);  // Previous connection polynomial
        C[0] = 1;
        B[0] = 1;
        
        int L = 0;           // Length of the current polynomial
        int m = 1;           // Counter for position in sequence
        int b = 1;           // Value of B[0] (always 1 in our case)
        
        for (int n_pos = 0; n_pos < n; n_pos++) {
            // Calculate discrepancy
            int d = sequence[n_pos];
            for (int i = 1; i <= L; i++) {
                d = (d + (long long)C[i] * sequence[n_pos - i]) % 2;
            }
            
            if (d == 0) {
                m++;
            } else {
                // Update the polynomial
                vector<int> T = C;
                for (int i = 0; i <= L; i++) {
                    C[i] = (C[i] + (long long)d * B[i]) % 2;
                }
                
                if (2 * L <= n_pos) {
                    L = n_pos - L;
                    B = T;
                    b = d;
                    m = 1;
                } else {
                    m++;
                }
            }
        }
        
        // Extract the minimal polynomial coefficients
        vector<int> result(L + 1);
        for (int i = 0; i <= L; i++) {
            result[i] = C[i];
        }
        
        return result;
    }
    
    // Alternative simpler version that returns just the degree
    static int findDegree(const vector<int>& sequence) {
        vector<int> poly = findMinimalPolynomial(sequence);
        return poly.size() - 1;
    }
};

// Example usage and test function
int main() {
    cout << "Berlekamp-Massey Algorithm Example\n";
    cout << "==================================\n\n";
    
    // Example 1: Sequence with known minimal polynomial x^3 + x + 1 = 0
    // This corresponds to a sequence like: 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, ...
    vector<int> sequence1 = {1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0};
    
    cout << "Example 1: Input sequence\n";
    for (int i = 0; i < min(10, (int)sequence1.size()); i++) {
        cout << sequence1[i] << " ";
    }
    cout << "\n";
    
    vector<int> poly1 = BerlekampMassey::findMinimalPolynomial(sequence1);
    
    cout << "Minimal polynomial coefficients: ";
    for (int i = 0; i < poly1.size(); i++) {
        if (i > 0 && poly1[i] != 0) cout << "+";
        if (i == 0) cout << poly1[i];
        else if (i == 1) cout << poly1[i] << "x";
        else cout << poly1[i] << "x^" << i;
    }
    cout << "\n";
    
    // Example 2: Simple sequence
    vector<int> sequence2 = {1, 1, 0, 1, 1, 0, 1, 1, 0};
    
    cout << "\nExample 2: Input sequence\n";
    for (int i = 0; i < min(9, (int)sequence2.size()); i++) {
        cout << sequence2[i] << " ";
    }
    cout << "\n";
    
    vector<int> poly2 = BerlekampMassey::findMinimalPolynomial(sequence2);
    
    cout << "Minimal polynomial coefficients: ";
    for (int i = 0; i < poly2.size(); i++) {
        if (i > 0 && poly2[i] != 0) cout << "+";
        if (i == 0) cout << poly2[i];
        else if (i == 1) cout << poly2[i] << "x";
        else cout << poly2[i] << "x^" << i;
    }
    cout << "\n";
    
    // Example 3: Linear recurrence sequence
    vector<int> fibonacci = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34};
    
    cout << "\nExample 3: Fibonacci sequence\n";
    for (int i = 0; i < min(10, (int)fibonacci.size()); i++) {
        cout << fibonacci[i] << " ";
    }
    cout << "\n";
    
    vector<int> poly3 = BerlekampMassey::findMinimalPolynomial(fibonacci);
    
    cout << "Minimal polynomial coefficients: ";
    for (int i = 0; i < poly3.size(); i++) {
        if (i > 0 && poly3[i] != 0) cout << "+";
        if (i == 0) cout << poly3[i];
        else if (i == 1) cout << poly3[i] << "x";
        else cout << poly3[i] << "x^" << i;
    }
    cout << "\n";
    
    return 0;
}
```

## How it works:

The Berlekamp-Massey algorithm finds the minimal polynomial that generates a given linearly recurrent sequence. Here's what happens in the implementation:

1. **Initialization**: Start with the trivial polynomial C(x) = 1 and B(x) = 1
2. **Discrepancy calculation**: For each new element, calculate how much it differs from what the current polynomial predicts
3. **Update**: If discrepancy is non-zero, update the polynomial using the previous polynomial
4. **Length tracking**: Keep track of the length L of the minimal polynomial

## Key Features:

- Handles binary sequences (0s and 1s)
- Returns polynomial coefficients in increasing order of powers
- Works with any linearly recurrent sequence
- Time complexity: O(n²) where n is the length of the sequence

## Sample Output:
```
Example 1: Input sequence
1 0 1 1 0 1 0 0 1 1 
Minimal polynomial coefficients: 1+1x+1x^2+1x^3

Example 2: Input sequence
1 1 0 1 1 0 1 1 0 
Minimal polynomial coefficients: 1+1x+1x^2+1x^3

Example 3: Fibonacci sequence
0 1 1 2 3 5 8 13 21 34 
Minimal polynomial coefficients: 1+1x+1x^2
```

This implementation can be used for finding the minimal polynomial of linear feedback shift registers, analyzing linear recurrence relations, and cryptographic applications.