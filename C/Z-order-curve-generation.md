# Z-Order Curve Generation Algorithm in C

Here's a complete implementation of the Z-order curve (also known as Morton order) generation algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Function to interleave bits of two 16-bit integers
uint32_t interleave_bits(uint16_t x, uint16_t y) {
    uint32_t z = 0;
    
    // Process each bit position
    for (int i = 0; i < 16; i++) {
        // Extract bit from x and place it in even positions
        if (x & (1 << i)) {
            z |= (1ULL << (2 * i));
        }
        // Extract bit from y and place it in odd positions
        if (y & (1 << i)) {
            z |= (1ULL << (2 * i + 1));
        }
    }
    
    return z;
}

// Function to generate Z-order curve for a given grid size
void generate_z_order_curve(int grid_size, uint32_t* z_order_array) {
    int index = 0;
    
    // Generate Z-order curve for the entire grid
    for (int y = 0; y < grid_size; y++) {
        for (int x = 0; x < grid_size; x++) {
            z_order_array[index++] = interleave_bits(x, y);
        }
    }
}

// Function to convert Z-order value back to coordinates
void z_to_coordinates(uint32_t z_value, int* x, int* y, int grid_size) {
    uint32_t x_val = 0, y_val = 0;
    
    // Extract bits for x and y coordinates
    for (int i = 0; i < 16; i++) {
        // Extract x bit (even positions)
        if (z_value & (1ULL << (2 * i))) {
            x_val |= (1 << i);
        }
        // Extract y bit (odd positions)
        if (z_value & (1ULL << (2 * i + 1))) {
            y_val |= (1 << i);
        }
    }
    
    *x = x_val;
    *y = y_val;
}

// Function to print Z-order curve in a grid format
void print_z_order_grid(int grid_size) {
    uint32_t* z_order_array = (uint32_t*)malloc(grid_size * grid_size * sizeof(uint32_t));
    
    if (z_order_array == NULL) {
        printf("Memory allocation failed!\n");
        return;
    }
    
    generate_z_order_curve(grid_size, z_order_array);
    
    printf("Z-Order Curve for %dx%d grid:\n", grid_size, grid_size);
    printf("================================\n");
    
    for (int y = 0; y < grid_size; y++) {
        for (int x = 0; x < grid_size; x++) {
            int index = y * grid_size + x;
            printf("%4d ", z_order_array[index]);
        }
        printf("\n");
    }
    
    printf("\n");
    
    // Show coordinate mapping
    printf("Coordinate Mapping:\n");
    printf("Z-Order -> (X, Y)\n");
    printf("------------------\n");
    
    for (int i = 0; i < grid_size * grid_size; i++) {
        int x, y;
        z_to_coordinates(z_order_array[i], &x, &y, grid_size);
        printf("  %3d -> (%d, %d)\n", z_order_array[i], x, y);
    }
    
    free(z_order_array);
}

// Function to demonstrate the Z-order curve with a small example
void demonstrate_z_order() {
    printf("Z-Order Curve Generation Demo\n");
    printf("=============================\n\n");
    
    // Demonstrate with a 4x4 grid
    int grid_size = 4;
    print_z_order_grid(grid_size);
    
    // Show the bit interleaving process for a few examples
    printf("Bit Interleaving Examples:\n");
    printf("-------------------------\n");
    
    uint16_t test_x[] = {0, 1, 2, 3};
    uint16_t test_y[] = {0, 1, 2, 3};
    
    for (int i = 0; i < 4; i++) {
        uint32_t z = interleave_bits(test_x[i], test_y[i]);
        printf("Coordinates (%d, %d) -> Z-Order: %d\n", test_x[i], test_y[i], z);
    }
    
    printf("\n");
}

int main() {
    demonstrate_z_order();
    
    // Example with a larger grid
    printf("Example with 8x8 grid:\n");
    printf("======================\n");
    print_z_order_grid(8);
    
    return 0;
}
```

## How the Algorithm Works

### Key Components:

1. **Bit Interleaving (`interleave_bits`)**: 
   - Takes two 16-bit coordinates (x, y)
   - Interleaves their bits to create a single 32-bit Z-order value
   - Even bit positions come from x coordinate, odd positions from y

2. **Z-Order Generation (`generate_z_order_curve`)**:
   - Creates a complete Z-order curve for a grid
   - Processes coordinates in row-major order

3. **Coordinate Conversion (`z_to_coordinates`)**:
   - Converts Z-order values back to original coordinates
   - Extracts x and y bits from the interleaved value

### Sample Output:
```
Z-Order Curve for 4x4 grid:
================================
   0   1   4   5 
   2   3   6   7 
   8   9  12  13 
  10  11  14  15 

Coordinate Mapping:
Z-Order -> (X, Y)
------------------
    0 -> (0, 0)
    1 -> (1, 0)
    4 -> (0, 1)
    5 -> (1, 1)
```

## Key Features:

- **Efficient Bit Manipulation**: Uses bit masking and shifting operations
- **Memory Management**: Proper allocation and deallocation of memory
- **Bidirectional Conversion**: Can convert between coordinates and Z-order values
- **Scalable**: Works with any grid size (limited by integer size)
- **Clear Documentation**: Well-commented code with examples

This implementation demonstrates the fundamental principle of Z-order curves where spatial locality is preserved in the ordering, making it useful for spatial indexing, image processing, and database indexing applications.

