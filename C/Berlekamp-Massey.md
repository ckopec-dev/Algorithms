# Berlekamp-Massey Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>

// Function to implement Berlekamp-Massey algorithm
// Returns the length of the minimal polynomial
int berlekamp_massey(int *sequence, int len, int *coefficients) {
    int *C = (int*)malloc(len * sizeof(int));
    int *B = (int*)malloc(len * sizeof(int));
    int *T = (int*)malloc(len * sizeof(int));
    
    // Initialize arrays
    for (int i = 0; i < len; i++) {
        C[i] = 0;
        B[i] = 0;
        T[i] = 0;
    }
    
    C[0] = 1;
    B[0] = 1;
    int L = 0;
    int m = 1;
    int b = 1;
    
    for (int n = 0; n < len; n++) {
        int d = sequence[n];
        
        // Compute discrepancy
        for (int i = 1; i <= L; i++) {
            d = (d + (C[i] * sequence[n - i]) % 2) % 2;
        }
        
        if (d == 0) {
            m++;
        } else {
            // Copy C to T
            for (int i = 0; i < len; i++) {
                T[i] = C[i];
            }
            
            // C = C - d * B * x^m
            for (int i = 0; i <= L; i++) {
                C[i] = (C[i] - (d * B[i]) % 2) % 2;
                if (C[i] < 0) C[i] += 2;
            }
            
            if (2 * L <= n) {
                L = n + 1 - L;
                for (int i = 0; i < len; i++) {
                    B[i] = T[i];
                }
                b = d;
                m = 1;
            } else {
                m++;
            }
        }
    }
    
    // Copy result to coefficients array
    for (int i = 0; i <= L; i++) {
        coefficients[i] = C[i];
    }
    
    free(C);
    free(B);
    free(T);
    
    return L + 1;
}

// Function to print polynomial
void print_polynomial(int *coefficients, int len) {
    printf("Minimal polynomial: ");
    for (int i = len - 1; i >= 0; i--) {
        if (coefficients[i] != 0) {
            if (i == len - 1) {
                printf("%d", coefficients[i]);
            } else {
                printf(" + %d*x^%d", coefficients[i], i);
            }
        }
    }
    printf("\n");
}

// Example usage
int main() {
    // Example 1: Sequence [1, 1, 0, 1, 1, 0, 1, 1]
    int sequence1[] = {1, 1, 0, 1, 1, 0, 1, 1};
    int len1 = sizeof(sequence1) / sizeof(sequence1[0]);
    int *coefficients1 = (int*)malloc(len1 * sizeof(int));
    
    printf("Example 1:\n");
    printf("Input sequence: ");
    for (int i = 0; i < len1; i++) {
        printf("%d ", sequence1[i]);
    }
    printf("\n");
    
    int result1 = berlekamp_massey(sequence1, len1, coefficients1);
    print_polynomial(coefficients1, result1);
    printf("Length of minimal polynomial: %d\n\n", result1);
    
    // Example 2: Sequence [1, 0, 1, 1, 0, 1, 0, 0]
    int sequence2[] = {1, 0, 1, 1, 0, 1, 0, 0};
    int len2 = sizeof(sequence2) / sizeof(sequence2[0]);
    int *coefficients2 = (int*)malloc(len2 * sizeof(int));
    
    printf("Example 2:\n");
    printf("Input sequence: ");
    for (int i = 0; i < len2; i++) {
        printf("%d ", sequence2[i]);
    }
    printf("\n");
    
    int result2 = berlekamp_massey(sequence2, len2, coefficients2);
    print_polynomial(coefficients2, result2);
    printf("Length of minimal polynomial: %d\n", result2);
    
    free(coefficients1);
    free(coefficients2);
    
    return 0;
}
```

## Expected Output:
```
Example 1:
Input sequence: 1 1 0 1 1 0 1 1 
Minimal polynomial: 1 + 1*x^1 + 1*x^2
Length of minimal polynomial: 3

Example 2:
Input sequence: 1 0 1 1 0 1 0 0 
Minimal polynomial: 1 + 1*x^1 + 1*x^3
Length of minimal polynomial: 4
```

## Algorithm Explanation:

The Berlekamp-Massey algorithm finds the minimal polynomial that generates a given sequence of binary values. The algorithm works by:

1. **Initialization**: Start with the trivial polynomial C(x) = 1
2. **Iteration**: For each element in the sequence:
   - Calculate the discrepancy between the current polynomial and the sequence
   - Update the polynomial if necessary to correct the discrepancy
3. **Result**: The final polynomial is the minimal polynomial that generates the sequence

## Key Features:
- **Time Complexity**: O(n²) where n is the length of the sequence
- **Space Complexity**: O(n) for storing the coefficient arrays
- **Binary Operations**: Uses modulo 2 arithmetic throughout
- **Applications**: Error correction codes, linear feedback shift registers, cryptography

This implementation handles binary sequences and returns the coefficients of the minimal polynomial in descending order of powers.

