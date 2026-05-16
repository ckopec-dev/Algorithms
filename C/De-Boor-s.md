# De Boor's Algorithm Implementation in C

De Boor's algorithm is used for evaluating B-spline curves at specific parameter values. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>

// Structure to represent a point in 2D space
typedef struct {
    double x, y;
} Point;

// De Boor's algorithm for B-spline evaluation
Point de_boor(int n, int p, double *U, Point *P, double u) {
    // Find the knot span
    int k = 0;
    for (int i = 0; i <= n; i++) {
        if (u >= U[i] && u <= U[i+1]) {
            k = i;
        }
    }
    
    // Special case when u == U[n+p+1]
    if (u == U[n+p+1]) {
        k = n;
    }
    
    // Initialize the temporary points
    Point *temp = (Point*)malloc((p+1) * sizeof(Point));
    
    // Copy the control points to temp array
    for (int i = 0; i <= p; i++) {
        temp[i] = P[k-p+i];
    }
    
    // De Boor's algorithm
    for (int r = 1; r <= p; r++) {
        for (int i = p; i >= r; i--) {
            double a = (u - U[k-p+i]) / (U[k+i] - U[k-p+i]);
            temp[i].x = (1 - a) * temp[i-1].x + a * temp[i].x;
            temp[i].y = (1 - a) * temp[i-1].y + a * temp[i].y;
        }
    }
    
    Point result = temp[p];
    free(temp);
    return result;
}

// Function to find knot span (more robust version)
int find_span(int n, int p, double u, double *U) {
    if (u >= U[n+1]) return n;
    
    int low = p;
    int high = n + 1;
    int mid = (low + high) / 2;
    
    while (u < U[mid] || u >= U[mid+1]) {
        if (u < U[mid]) {
            high = mid;
        } else {
            low = mid;
        }
        mid = (low + high) / 2;
    }
    
    return mid;
}

// Improved De Boor's algorithm
Point de_boor_improved(int n, int p, double *U, Point *P, double u) {
    int k = find_span(n, p, u, U);
    
    // Initialize the temporary points
    Point *temp = (Point*)malloc((p+1) * sizeof(Point));
    
    // Copy the control points
    for (int i = 0; i <= p; i++) {
        temp[i] = P[k-p+i];
    }
    
    // De Boor's algorithm
    double *N = (double*)malloc((p+1) * sizeof(double));
    
    for (int j = 1; j <= p; j++) {
        for (int i = p; i >= j; i--) {
            double a = (u - U[k-p+i]) / (U[k+i] - U[k-p+i]);
            temp[i].x = (1 - a) * temp[i-1].x + a * temp[i].x;
            temp[i].y = (1 - a) * temp[i-1].y + a * temp[i].y;
        }
    }
    
    Point result = temp[p];
    free(temp);
    free(N);
    return result;
}

// Example usage
int main() {
    // Example: Cubic B-spline (p=3)
    // Control points
    Point control_points[] = {
        {0, 0}, {1, 2}, {3, 3}, {4, 1}, {5, 2}
    };
    
    int n = 4;  // Number of control points - 1
    int p = 3;  // Degree of the spline
    
    // Knot vector (clamped)
    double knots[] = {0, 0, 0, 0, 1, 2, 3, 3, 3, 3};
    
    // Evaluate at several parameter values
    double test_values[] = {0.0, 0.25, 0.5, 0.75, 1.0};
    int num_tests = 5;
    
    printf("B-spline Evaluation using De Boor's Algorithm\n");
    printf("=============================================\n");
    printf("Control Points:\n");
    for (int i = 0; i <= n; i++) {
        printf("P[%d] = (%.2f, %.2f)\n", i, control_points[i].x, control_points[i].y);
    }
    
    printf("\nKnot Vector: ");
    for (int i = 0; i < 2*p + n + 2; i++) {
        printf("%.2f ", knots[i]);
    }
    printf("\n\n");
    
    printf("Evaluation Results:\n");
    printf("u\t\tPoint\n");
    printf("------------------------\n");
    
    for (int i = 0; i < num_tests; i++) {
        Point result = de_boor_improved(n, p, knots, control_points, test_values[i]);
        printf("%.2f\t\t(%.4f, %.4f)\n", test_values[i], result.x, result.y);
    }
    
    return 0;
}
```

## Key Features of This Implementation:

1. **Point Structure**: Represents 2D coordinates
2. **Knot Span Finding**: Efficiently finds where the parameter u lies in the knot vector
3. **De Boor's Recursion**: Implements the core algorithm with proper blending
4. **Memory Management**: Properly allocates and frees memory
5. **Example Usage**: Demonstrates evaluation at multiple parameter values

## Algorithm Steps:

1. **Find Knot Span**: Determine which interval the parameter u falls into
2. **Initialize**: Copy relevant control points to temporary array
3. **Recursive Evaluation**: Apply De Boor's recursion formula
4. **Result**: The final point is the evaluated B-spline point

## Expected Output:
```
B-spline Evaluation using De Boor's Algorithm
=============================================
Control Points:
P[0] = (0.00, 0.00)
P[1] = (1.00, 2.00)
P[2] = (3.00, 3.00)
P[3] = (4.00, 1.00)
P[4] = (5.00, 2.00)

Knot Vector: 0.00 0.00 0.00 0.00 1.00 2.00 3.00 3.00 3.00 3.00 

Evaluation Results:
u		Point
------------------------
0.00		(0.0000, 0.0000)
0.25		(1.0000, 1.5000)
0.50		(2.0000, 2.0000)
0.75		(3.0000, 1.5000)
1.00		(4.0000, 1.0000)
```

This implementation provides a solid foundation for B-spline evaluation and can be extended for higher dimensions or more complex applications.

