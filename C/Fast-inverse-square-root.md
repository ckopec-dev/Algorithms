```c
#include <stdio.h>
#include <stdint.h>

float fast_inverse_square_root(float number) {
    long i;
    float x2, y;
    const float threehalfs = 1.5F;

    x2 = number * 0.5F;
    y = number;
    
    // Convert float to integer representation
    i = *(long*)&y;
    
    // Fast inverse square root algorithm
    i = 0x5f3759df - (i >> 1);
    
    // Convert back to float
    y = *(float*)&i;
    
    // Newton-Raphson iteration for better accuracy
    y = y * (threehalfs - (x2 * y * y));
    y = y * (threehalfs - (x2 * y * y));
    
    return y;
}

int main() {
    float test_number = 16.0f;
    float result = fast_inverse_square_root(test_number);
    
    printf("Input: %f\n", test_number);
    printf("Fast inverse square root: %f\n", result);
    printf("Actual inverse square root: %f\n", 1.0f / sqrt(test_number));
    
    return 0;
}
```

This implementation demonstrates the famous Fast Inverse Square Root algorithm used in the game Quake III Arena. The key steps are:

1. **Bit manipulation**: Uses the IEEE 754 float representation to perform bit-level operations
2. **Magic number**: `0x5f3759df` is the magic constant that provides an initial approximation
3. **Newton-Raphson iteration**: Two iterations improve the accuracy of the result
4. **Optimization**: Avoids expensive division and square root operations by using bit manipulation and iterative refinement

The algorithm is particularly useful in real-time graphics and game programming where performance is critical.

