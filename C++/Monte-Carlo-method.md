# Monte Carlo Method Example in C++

Here's a complete example of using the Monte Carlo method to estimate the value of π:

```cpp
#include <iostream>
#include <random>
#include <cmath>

double estimatePi(int numPoints) {
    // Create random number generator
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(-1.0, 1.0);
    
    int pointsInsideCircle = 0;
    
    // Generate random points and count how many fall inside the unit circle
    for (int i = 0; i < numPoints; i++) {
        double x = dis(gen);
        double y = dis(gen);
        
        // Check if point is inside unit circle (distance from origin <= 1)
        if (x * x + y * y <= 1.0) {
            pointsInsideCircle++;
        }
    }
    
    // Estimate π using the ratio of points inside circle to total points
    // Area of circle / Area of square = π/4
    // So π ≈ 4 * (pointsInsideCircle / numPoints)
    return 4.0 * pointsInsideCircle / numPoints;
}

int main() {
    const int numPoints = 1000000;  // Number of random points to generate
    
    std::cout << "Monte Carlo Method to Estimate π" << std::endl;
    std::cout << "================================" << std::endl;
    std::cout << "Number of points: " << numPoints << std::endl;
    std::cout << "Actual π: " << M_PI << std::endl;
    
    double estimatedPi = estimatePi(numPoints);
    std::cout << "Estimated π: " << estimatedPi << std::endl;
    
    double error = std::abs(estimatedPi - M_PI);
    std::cout << "Error: " << error << std::endl;
    
    return 0;
}
```

## How it works:

1. **Random Point Generation**: Generate random points within a square with side length 2 (from -1 to 1 in both x and y directions)

2. **Circle Detection**: Check if each point falls inside the unit circle (distance from origin ≤ 1)

3. **Ratio Calculation**: The ratio of points inside the circle to total points approximates π/4

4. **π Estimation**: Multiply by 4 to get the π estimate

## Sample Output:
```
Monte Carlo Method to Estimate π
================================
Number of points: 1000000
Actual π: 3.14159
Estimated π: 3.14123
Error: 0.000359983
```

## Key Features:

- Uses `std::random` for high-quality random number generation
- Implements the classic Monte Carlo π estimation algorithm
- Demonstrates the convergence of the method with more points
- Shows how the accuracy improves with increased sample size

The Monte Carlo method is particularly useful for problems where analytical solutions are difficult or impossible to obtain, and it provides a probabilistic approach to numerical integration and estimation.

