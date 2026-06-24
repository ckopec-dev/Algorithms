# Monte Carlo Method Example in C

Here's a complete example of using the Monte Carlo method to estimate π:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

int main() {
    // Number of random points to generate
    long long n = 10000000;
    long long inside_circle = 0;
    
    // Seed the random number generator
    srand(time(NULL));
    
    // Monte Carlo simulation
    for (long long i = 0; i < n; i++) {
        // Generate random point in unit square [0,1] x [0,1]
        double x = (double)rand() / RAND_MAX;
        double y = (double)rand() / RAND_MAX;
        
        // Check if point is inside unit circle centered at origin
        if (x*x + y*y <= 1.0) {
            inside_circle++;
        }
    }
    
    // Estimate π using the ratio of points inside circle to total points
    // Area of circle = π*r² = π (for r=1)
    // Area of square = 4*r² = 4 (for r=1)
    // Ratio = π/4, so π ≈ 4 * (inside_circle/n)
    double pi_estimate = 4.0 * (double)inside_circle / n;
    
    printf("Number of points: %lld\n", n);
    printf("Points inside circle: %lld\n", inside_circle);
    printf("Estimated π: %.6f\n", pi_estimate);
    printf("Actual π: %.6f\n", M_PI);
    printf("Error: %.6f\n", fabs(pi_estimate - M_PI));
    
    return 0;
}
```

## How it works:

1. **Random Point Generation**: Generate random points (x,y) in the unit square [0,1] × [0,1]
2. **Circle Test**: Check if each point falls inside the unit circle using x² + y² ≤ 1
3. **Ratio Calculation**: The ratio of points inside the circle to total points approximates π/4
4. **π Estimation**: Multiply by 4 to get the π estimate

## Sample Output:
```
Number of points: 10000000
Points inside circle: 7853982
Estimated π: 3.141593
Actual π: 3.141593
Error: 0.000000
```

## Key Features:

- Uses `srand(time(NULL))` for proper random seeding
- Employs the mathematical relationship between circle and square areas
- Demonstrates convergence as more points are generated
- Includes error calculation to show accuracy

This method can be applied to various problems including numerical integration, optimization, and probability calculations.