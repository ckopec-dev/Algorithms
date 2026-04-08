# Z-Order Curve Generation Algorithm

The Z-order curve (also known as Morton order) is a space-filling curve that maps multi-dimensional data to one dimension while preserving locality. Here's a Python implementation:

```python
def z_order_curve_generation(width, height):
    """
    Generate Z-order curve for a given 2D grid
    
    Args:
        width (int): Width of the grid
        height (int): Height of the grid
    
    Returns:
        list: List of (x, y) coordinates in Z-order sequence
    """
    points = []
    
    # Generate all points in the grid
    for y in range(height):
        for x in range(width):
            points.append((x, y))
    
    # Sort points by Z-order value
    points.sort(key=lambda point: z_order_value(point[0], point[1]))
    
    return points

def z_order_value(x, y):
    """
    Calculate Z-order value for a given (x, y) coordinate
    
    Args:
        x (int): X coordinate
        y (int): Y coordinate
    
    Returns:
        int: Z-order value
    """
    # Interleave bits of x and y
    result = 0
    bit_position = 0
    
    while x > 0 or y > 0:
        # Extract least significant bit of x and y
        x_bit = x & 1
        y_bit = y & 1
        
        # Set the bit in result at position bit_position * 2
        result |= (x_bit << (bit_position * 2))
        # Set the bit in result at position bit_position * 2 + 1
        result |= (y_bit << (bit_position * 2 + 1))
        
        # Move to next bit
        x >>= 1
        y >>= 1
        bit_position += 1
    
    return result

def z_order_curve_bit_interleaving(x, y):
    """
    Alternative implementation using bit interleaving
    
    Args:
        x (int): X coordinate
        y (int): Y coordinate
    
    Returns:
        int: Z-order value
    """
    # Convert to binary and interleave bits
    def interleave_bits(x, y):
        result = 0
        for i in range(32):  # Assuming 32-bit integers
            if i < 32:  # Check if we have bits left
                if (x & (1 << i)):
                    result |= (1 << (2 * i))
                if (y & (1 << i)):
                    result |= (1 << (2 * i + 1))
        return result
    
    return interleave_bits(x, y)

# Example usage
def print_z_order_curve(width, height):
    """
    Print Z-order curve for a given grid size
    """
    print(f"Z-Order Curve for {width}x{height} grid:")
    print("-" * 30)
    
    points = z_order_curve_generation(width, height)
    
    # Print points with their Z-order values
    for i, (x, y) in enumerate(points):
        z_value = z_order_value(x, y)
        print(f"Point {i+1:2d}: ({x:2d}, {y:2d}) -> Z-value: {z_value:3d}")
    
    return points

# Demonstration
if __name__ == "__main__":
    # Example 1: 4x4 grid
    print("Example 1: 4x4 Grid")
    points_4x4 = print_z_order_curve(4, 4)
    print()
    
    # Example 2: 3x3 grid
    print("Example 2: 3x3 Grid")
    points_3x3 = print_z_order_curve(3, 3)
    print()
    
    # Example 3: Manual verification of Z-order values
    print("Manual verification of Z-order values:")
    test_points = [(0,0), (1,0), (0,1), (1,1), (2,0), (0,2)]
    for x, y in test_points:
        z_val = z_order_value(x, y)
        print(f"({x}, {y}) -> Z-value: {z_val}")
```

## Output Example:
```
Example 1: 4x4 Grid
------------------------------
Point  1: ( 0,  0) -> Z-value:   0
Point  2: ( 1,  0) -> Z-value:   1
Point  3: ( 0,  1) -> Z-value:   2
Point  4: ( 1,  1) -> Z-value:   3
Point  5: ( 2,  0) -> Z-value:   4
Point  6: ( 3,  0) -> Z-value:   5
Point  7: ( 2,  1) -> Z-value:   6
Point  8: ( 3,  1) -> Z-value:   7
Point  9: ( 0,  2) -> Z-value:   8
Point 10: ( 1,  2) -> Z-value:   9
Point 11: ( 0,  3) -> Z-value:  10
Point 12: ( 1,  3) -> Z-value:  11
Point 13: ( 2,  2) -> Z-value:  12
Point 14: ( 3,  2) -> Z-value:  13
Point 15: ( 2,  3) -> Z-value:  14
Point 16: ( 3,  3) -> Z-value:  15

Manual verification of Z-order values:
(0, 0) -> Z-value: 0
(1, 0) -> Z-value: 1
(0, 1) -> Z-value: 2
(1, 1) -> Z-value: 3
(2, 0) -> Z-value: 4
(0, 2) -> Z-value: 8
```

## Key Features:

1. **Bit Interleaving**: The core algorithm interleaves the bits of x and y coordinates
2. **Locality Preservation**: Points that are close in 2D space tend to have similar Z-order values
3. **Space-Filling**: Maps 2D coordinates to 1D sequence while maintaining spatial relationships
4. **Efficient**: O(n log n) time complexity due to sorting

The Z-order curve is commonly used in spatial indexing, database systems, and image processing applications where maintaining spatial locality is important.

