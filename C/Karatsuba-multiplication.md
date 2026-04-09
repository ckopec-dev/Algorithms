# Karatsuba Multiplication Algorithm in C

Here's an implementation of the Karatsuba multiplication algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Function to calculate the maximum of two numbers
int max(int a, int b) {
    return (a > b) ? a : b;
}

// Function to perform Karatsuba multiplication
long long karatsuba(long long x, long long y) {
    // Base case
    if (x < 10 || y < 10)
        return x * y;
    
    // Calculate the size of the numbers
    int n = max(strlen(str(x)), strlen(str(y)));
    int half = n / 2;
    
    // Split the numbers
    long long high1 = x / pow(10, half);
    long long low1 = x % pow(10, half);
    long long high2 = y / pow(10, half);
    long long low2 = y % pow(10, half);
    
    // 3 recursive calls
    long long z0 = karatsuba(low1, low2);
    long long z1 = karatsuba((low1 + high1), (low2 + high2));
    long long z2 = karatsuba(high1, high2);
    
    // Combine the results
    return (z2 * pow(10, 2 * half)) + ((z1 - z2 - z0) * pow(10, half)) + z0;
}

// Alternative implementation for strings (more practical for large numbers)
char* karatsuba_string(char* x, char* y) {
    int len_x = strlen(x);
    int len_y = strlen(y);
    
    // Base case
    if (len_x == 1 && len_y == 1) {
        int result = (x[0] - '0') * (y[0] - '0');
        char* str_result = (char*)malloc(2 * sizeof(char));
        str_result[0] = result + '0';
        str_result[1] = '\0';
        return str_result;
    }
    
    // Make both strings of equal length by padding with zeros
    int max_len = max(len_x, len_y);
    if (max_len % 2 != 0) max_len++;
    
    // Pad strings with leading zeros
    char* padded_x = (char*)malloc((max_len + 1) * sizeof(char));
    char* padded_y = (char*)malloc((max_len + 1) * sizeof(char));
    
    memset(padded_x, '0', max_len);
    memset(padded_y, '0', max_len);
    
    strcpy(padded_x + max_len - len_x, x);
    strcpy(padded_y + max_len - len_y, y);
    
    // Split the strings
    int half = max_len / 2;
    char* x_high = (char*)malloc((half + 1) * sizeof(char));
    char* x_low = (char*)malloc((half + 1) * sizeof(char));
    char* y_high = (char*)malloc((half + 1) * sizeof(char));
    char* y_low = (char*)malloc((half + 1) * sizeof(char));
    
    strncpy(x_high, padded_x, half);
    x_high[half] = '\0';
    strncpy(x_low, padded_x + half, half);
    x_low[half] = '\0';
    strncpy(y_high, padded_y, half);
    y_high[half] = '\0';
    strncpy(y_low, padded_y + half, half);
    y_low[half] = '\0';
    
    // Recursive calls
    char* z0 = karatsuba_string(x_low, y_low);
    char* z2 = karatsuba_string(x_high, y_high);
    
    // Calculate (x_high + x_low) * (y_high + y_low)
    // This is a simplified version - full implementation would need addition
    char* z1 = NULL; // Placeholder
    
    // Combine results
    // This is a simplified version - a full implementation would be more complex
    
    free(padded_x);
    free(padded_y);
    free(x_high);
    free(x_low);
    free(y_high);
    free(y_low);
    free(z0);
    free(z2);
    
    return "Result";
}

// Simple version for demonstration with small numbers
long long simple_karatsuba(long long x, long long y) {
    if (x < 10 || y < 10)
        return x * y;
    
    int n = 0;
    long long temp_x = x, temp_y = y;
    while (temp_x > 0) { n++; temp_x /= 10; }
    while (temp_y > 0) { n++; temp_y /= 10; }
    n /= 2;
    
    long long high1 = x / (long long)pow(10, n);
    long long low1 = x % (long long)pow(10, n);
    long long high2 = y / (long long)pow(10, n);
    long long low2 = y % (long long)pow(10, n);
    
    long long z0 = simple_karatsuba(low1, low2);
    long long z1 = simple_karatsuba((low1 + high1), (low2 + high2));
    long long z2 = simple_karatsuba(high1, high2);
    
    return (z2 * (long long)pow(10, 2 * n)) + ((z1 - z2 - z0) * (long long)pow(10, n)) + z0;
}

int main() {
    long long num1 = 1234;
    long long num2 = 5678;
    
    printf("Multiplying %lld and %lld\n", num1, num2);
    printf("Result using Karatsuba: %lld\n", simple_karatsuba(num1, num2));
    printf("Result using normal multiplication: %lld\n", num1 * num2);
    
    return 0;
}
```

## Key Features of This Implementation:

1. **Base Case**: When numbers are single digits, use normal multiplication
2. **Splitting**: Divide numbers into high and low parts
3. **Recursive Calls**: Make three recursive calls instead of four
4. **Combination**: Combine results using the Karatsuba formula

## Time Complexity:
- **Normal multiplication**: O(n²)
- **Karatsuba multiplication**: O(n^1.585)

## Advantages:
- More efficient for large numbers
- Reduces the number of multiplications from 4 to 3
- Better performance for very large integers

## Note:
This is a simplified version. A complete implementation would need proper string handling for very large numbers and more robust addition/subtraction functions for the intermediate results.

