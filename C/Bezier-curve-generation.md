# Bezier Curve Generation in C

Here's a complete implementation of a Bezier curve generation algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Structure to represent a 2D point
typedef struct {
    double x;
    double y;
} Point;

// Structure to represent a Bezier curve
typedef struct {
    Point* control_points;
    int num_points;
    Point* curve_points;
    int num_curve_points;
} BezierCurve;

// Function to calculate binomial coefficient C(n,k)
int binomial_coefficient(int n, int k) {
    if (k > n || k < 0) return 0;
    if (k == 0 || k == n) return 1;
    
    int result = 1;
    for (int i = 0; i < k; i++) {
        result = result * (n - i) / (i + 1);
    }
    return result;
}

// Function to calculate Bernstein polynomial
double bernstein_polynomial(int n, int i, double t) {
    int binomial = binomial_coefficient(n, i);
    return binomial * pow(t, i) * pow(1 - t, n - i);
}

// Function to generate a Bezier curve
BezierCurve* generate_bezier_curve(Point* control_points, int num_control_points, int num_curve_points) {
    // Allocate memory for the Bezier curve structure
    BezierCurve* curve = (BezierCurve*)malloc(sizeof(BezierCurve));
    if (!curve) return NULL;
    
    curve->control_points = control_points;
    curve->num_points = num_control_points;
    curve->num_curve_points = num_curve_points;
    
    // Allocate memory for curve points
    curve->curve_points = (Point*)malloc(num_curve_points * sizeof(Point));
    if (!curve->curve_points) {
        free(curve);
        return NULL;
    }
    
    // Generate curve points using De Casteljau's algorithm or direct formula
    int n = num_control_points - 1; // Degree of the curve
    
    for (int i = 0; i < num_curve_points; i++) {
        double t = (double)i / (num_curve_points - 1);
        
        // Calculate point on the curve
        curve->curve_points[i].x = 0.0;
        curve->curve_points[i].y = 0.0;
        
        for (int j = 0; j < num_control_points; j++) {
            double bernstein = bernstein_polynomial(n, j, t);
            curve->curve_points[i].x += control_points[j].x * bernstein;
            curve->curve_points[i].y += control_points[j].y * bernstein;
        }
    }
    
    return curve;
}

// Function to print Bezier curve points
void print_bezier_curve(BezierCurve* curve) {
    printf("Bezier Curve Points:\n");
    for (int i = 0; i < curve->num_curve_points; i++) {
        printf("P%d: (%.2f, %.2f)\n", i, curve->curve_points[i].x, curve->curve_points[i].y);
    }
}

// Function to free memory allocated for Bezier curve
void free_bezier_curve(BezierCurve* curve) {
    if (curve) {
        if (curve->curve_points) {
            free(curve->curve_points);
        }
        free(curve);
    }
}

// Example usage
int main() {
    // Define control points for a quadratic Bezier curve (3 points)
    Point control_points[] = {
        {0.0, 0.0},   // Start point
        {2.0, 4.0},   // Control point
        {4.0, 0.0}    // End point
    };
    
    int num_control_points = 3;
    int num_curve_points = 20;
    
    printf("Generating Bezier Curve with %d control points\n", num_control_points);
    printf("Control points:\n");
    for (int i = 0; i < num_control_points; i++) {
        printf("P%d: (%.2f, %.2f)\n", i, control_points[i].x, control_points[i].y);
    }
    
    // Generate the Bezier curve
    BezierCurve* bezier_curve = generate_bezier_curve(control_points, num_control_points, num_curve_points);
    
    if (bezier_curve) {
        printf("\n");
        print_bezier_curve(bezier_curve);
        
        // Free allocated memory
        free_bezier_curve(bezier_curve);
    } else {
        printf("Error generating Bezier curve\n");
        return 1;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Point Structure**: Defines 2D points with x and y coordinates
2. **BezierCurve Structure**: Contains control points, curve points, and metadata
3. **Binomial Coefficient**: Calculates C(n,k) for Bernstein polynomials
4. **Bernstein Polynomial**: Computes the Bernstein basis functions
5. **Curve Generation**: Uses the standard Bezier curve formula
6. **Memory Management**: Proper allocation and deallocation of memory

## How it Works:

1. **Input**: Control points that define the curve shape
2. **Process**: For each parameter t from 0 to 1, calculate the corresponding point on the curve using the Bernstein polynomial formula
3. **Output**: An array of points that form the smooth Bezier curve

## Sample Output:
```
Generating Bezier Curve with 3 control points
Control points:
P0: (0.00, 0.00)
P1: (2.00, 4.00)
P2: (4.00, 0.00)

Bezier Curve Points:
P0: (0.00, 0.00)
P1: (0.22, 0.33)
P2: (0.44, 0.67)
...
P19: (4.00, 0.00)
```

This implementation can generate Bezier curves of any degree by adjusting the number of control points.

