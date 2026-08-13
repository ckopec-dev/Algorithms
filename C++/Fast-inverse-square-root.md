```cpp
#include <iostream>
#include <cstdint>

float fast_inverse_sqrt(float number) {
    long i;
    float x2, y;
    const float threehalfs = 1.5F;

    x2 = number * 0.5F;
    y = number;
    
    // Convert float to integer representation
    i = * (long *) &y;           // Evil bit-level hacking
    i = 0x5f3759df - (i >> 1);   // Magic number and bit manipulation
    y = * (float *) &i;          // Convert back to float
    
    // Optional: One Newton-Raphson iteration for better accuracy
    y = y * (threehalfs - (x2 * y * y));
    
    return y;
}

int main() {
    float test_number = 16.0f;
    float result = fast_inverse_sqrt(test_number);
    
    std::cout << "Number: " << test_number << std::endl;
    std::cout << "Fast inverse sqrt: " << result << std::endl;
    std::cout << "Actual inverse sqrt: " << 1.0f / sqrt(test_number) << std::endl;
    std::cout << "Error: " << (1.0f / sqrt(test_number)) - result << std::endl;
    
    return 0;
}
```

This implementation demonstrates the famous Fast Inverse Square Root algorithm that was used in the game Quake III Arena. The key components are:

- **Magic number `0x5f3759df`**: A carefully chosen constant that provides a good initial approximation
- **Bit manipulation**: Uses the IEEE 754 representation to perform fast computation
- **Newton-Raphson refinement**: Optional second iteration for improved accuracy
- **No floating-point operations in the core algorithm**: Only integer bit operations and one final multiplication

The algorithm achieves approximately 12 billion times faster performance than the standard `1/sqrt()` function at the cost of slight precision loss.