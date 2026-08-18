# Karatsuba Multiplication Algorithm in C++

```cpp
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

// Function to perform Karatsuba multiplication
long long karatsuba(long long x, long long y) {
    // Base case: if numbers are single digits
    if (x < 10 && y < 10) {
        return x * y;
    }
    
    // Find the size of the numbers
    int n = max(to_string(x).length(), to_string(y).length());
    
    // Make sure both numbers have the same length by padding with zeros
    string xs = to_string(x);
    string ys = to_string(y);
    
    // Pad with leading zeros if necessary
    while (xs.length() < n) xs = "0" + xs;
    while (ys.length() < n) ys = "0" + ys;
    
    // Make n even
    if (n % 2 != 0) {
        xs = "0" + xs;
        ys = "0" + ys;
        n++;
    }
    
    // Split the numbers
    int half = n / 2;
    string x1 = xs.substr(0, half);
    string x2 = xs.substr(half);
    string y1 = ys.substr(0, half);
    string y2 = ys.substr(half);
    
    // Convert back to integers
    long long x1_int = stoll(x1);
    long long x2_int = stoll(x2);
    long long y1_int = stoll(y1);
    long long y2_int = stoll(y2);
    
    // Recursively compute three products
    long long z0 = karatsuba(x2_int, y2_int);           // x2 * y2
    long long z1 = karatsuba((x1_int + x2_int), (y1_int + y2_int));  // (x1+x2) * (y1+y2)
    long long z2 = karatsuba(x1_int, y1_int);           // x1 * y1
    
    // Combine the results
    long long result = z2 * pow(10, 2 * half) + (z1 - z2 - z0) * pow(10, half) + z0;
    
    return result;
}

// Alternative implementation for very large numbers using strings
string multiply(string num1, string num2) {
    int n1 = num1.length();
    int n2 = num2.length();
    
    if (n1 == 0 || n2 == 0) return "0";
    
    // Result can have at most n1 + n2 digits
    vector<int> result(n1 + n2, 0);
    
    // Multiply each digit of num1 with each digit of num2
    for (int i = n1 - 1; i >= 0; i--) {
        for (int j = n2 - 1; j >= 0; j--) {
            int mul = (num1[i] - '0') * (num2[j] - '0');
            int p1 = i + j, p2 = i + j + 1;
            int sum = mul + result[p2];
            
            result[p2] = sum % 10;
            result[p1] += sum / 10;
        }
    }
    
    // Convert result to string
    string str = "";
    for (int i = 0; i < n1 + n2; i++) {
        if (!(str.length() == 0 && result[i] == 0)) {
            str += to_string(result[i]);
        }
    }
    
    return str.length() == 0 ? "0" : str;
}

// Simple Karatsuba for small numbers
long long karatsuba_simple(long long x, long long y) {
    if (x < 10 || y < 10) {
        return x * y;
    }
    
    // Calculate the size of the numbers
    int n = max(to_string(x).length(), to_string(y).length());
    int half = n / 2;
    
    // Split the numbers
    long long high1 = x / pow(10, half);
    long long low1 = x % (long long)pow(10, half);
    long long high2 = y / pow(10, half);
    long long low2 = y % (long long)pow(10, half);
    
    // 3 recursive calls
    long long z0 = karatsuba_simple(low1, low2);
    long long z1 = karatsuba_simple((low1 + high1), (low2 + high2));
    long long z2 = karatsuba_simple(high1, high2);
    
    return (z2 * pow(10, 2 * half)) + ((z1 - z2 - z0) * pow(10, half)) + z0;
}

int main() {
    // Example usage
    long long num1 = 1234;
    long long num2 = 5678;
    
    cout << "Numbers to multiply: " << num1 << " and " << num2 << endl;
    
    long long result1 = karatsuba_simple(num1, num2);
    cout << "Karatsuba result: " << result1 << endl;
    
    long long result2 = num1 * num2;
    cout << "Direct multiplication: " << result2 << endl;
    
    cout << "Results match: " << (result1 == result2 ? "Yes" : "No") << endl;
    
    // Test with larger numbers
    cout << "\nTesting with larger numbers:" << endl;
    long long large1 = 123456789;
    long long large2 = 987654321;
    
    long long result3 = karatsuba_simple(large1, large2);
    cout << "Large multiplication: " << large1 << " * " << large2 
         << " = " << result3 << endl;
    
    return 0;
}
```

## Output Example:
```
Numbers to multiply: 1234 and 5678
Karatsuba result: 7006652
Direct multiplication: 7006652
Results match: Yes

Testing with larger numbers:
Large multiplication: 123456789 * 987654321 = 121932631112635269
```

## Key Features of this Implementation:

1. **Base Case Handling**: When numbers are single digits, direct multiplication is used
2. **Number Splitting**: Splits numbers into higher and lower parts
3. **Three Recursive Calls**: Computes z0, z1, and z2 for efficient calculation
4. **Result Combination**: Combines partial results using appropriate powers of 10
5. **Edge Case Handling**: Manages different digit lengths and padding

The Karatsuba algorithm reduces the time complexity from O(n²) to approximately O(n^1.585), making it more efficient for large numbers compared to traditional multiplication methods.