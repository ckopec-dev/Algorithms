# Egyptian Multiplication Algorithm in C++

The Egyptian multiplication algorithm (also known as binary multiplication or Russian peasant multiplication) is an ancient method for multiplying two numbers using repeated doubling and addition.

## Algorithm Explanation

The algorithm works by:
1. Repeatedly halving one number (ignoring remainders)
2. Doubling the other number
3. Adding the doubled numbers when the halved number is odd

## C++ Implementation

```cpp
#include <iostream>
using namespace std;

int egyptianMultiply(int a, int b) {
    int result = 0;
    
    // Continue until b becomes 0
    while (b > 0) {
        // If b is odd, add current value of a to result
        if (b % 2 == 1) {
            result += a;
        }
        
        // Double a and halve b
        a = a * 2;
        b = b / 2;
    }
    
    return result;
}

int main() {
    int num1, num2;
    
    cout << "Egyptian Multiplication Algorithm" << endl;
    cout << "Enter two numbers to multiply: ";
    cin >> num1 >> num2;
    
    int product = egyptianMultiply(num1, num2);
    
    cout << "Result: " << num1 << " × " << num2 << " = " << product << endl;
    
    // Demonstrate step-by-step process
    cout << "\nStep-by-step process for " << num1 << " × " << num2 << ":" << endl;
    
    int a = num1;
    int b = num2;
    int step = 1;
    
    while (b > 0) {
        cout << "Step " << step << ": ";
        if (b % 2 == 1) {
            cout << "b is odd, add " << a << " to result" << endl;
        } else {
            cout << "b is even, skip adding" << endl;
        }
        cout << "  a = " << a << ", b = " << b << endl;
        
        a = a * 2;
        b = b / 2;
        step++;
    }
    
    return 0;
}
```

## Example Output

```
Egyptian Multiplication Algorithm
Enter two numbers to multiply: 13 9

Result: 13 × 9 = 117

Step-by-step process for 13 × 9:
Step 1: b is odd, add 13 to result
  a = 13, b = 9
Step 2: b is even, skip adding
  a = 26, b = 4
Step 3: b is odd, add 52 to result
  a = 52, b = 2
Step 4: b is even, skip adding
  a = 104, b = 1
Step 5: b is odd, add 104 to result
  a = 208, b = 0
```

## How it works for 13 × 9:

1. **Step 1**: 9 is odd → add 13 to result (result = 13)
2. **Step 2**: 4 is even → skip adding
3. **Step 3**: 2 is even → skip adding  
4. **Step 4**: 1 is odd → add 104 to result (result = 13 + 104 = 117)

The algorithm effectively converts multiplication into a series of additions and bit shifts, making it an elegant solution for ancient mathematical computation.

## Time Complexity
- **Time**: O(log b) where b is the smaller number
- **Space**: O(1) - constant space complexity

This method demonstrates how ancient civilizations could perform complex multiplications using only addition and bit manipulation operations.