# Z-Order Curve Generation in Rust

Here's an implementation of the Z-order curve (also known as Morton order) generation algorithm in Rust:

```rust
fn z_order_curve_generation(width: u32, height: u32) -> Vec<(u32, u32)> {
    let mut points = Vec::new();
    
    // Generate all points in the grid
    for y in 0..height {
        for x in 0..width {
            points.push((x, y));
        }
    }
    
    // Sort points by their Z-order value
    points.sort_by_key(|&(x, y)| {
        // Convert coordinates to Z-order value
        z_order_value(x, y)
    });
    
    points
}

fn z_order_value(x: u32, y: u32) -> u64 {
    let mut result = 0u64;
    let mut x = x;
    let mut y = y;
    let mut bit_pos = 0;
    
    // Interleave the bits of x and y
    while x > 0 || y > 0 {
        // Extract least significant bit of x and y
        let x_bit = x & 1;
        let y_bit = y & 1;
        
        // Set the bit in result at position bit_pos*2 + 1 (for y) and bit_pos*2 (for x)
        result |= (y_bit as u64) << (bit_pos * 2 + 1);
        result |= (x_bit as u64) << (bit_pos * 2);
        
        x >>= 1;
        y >>= 1;
        bit_pos += 1;
    }
    
    result
}

// Alternative implementation using bit manipulation with bit masking
fn z_order_value_optimized(x: u32, y: u32) -> u64 {
    let mut x = x as u64;
    let mut y = y as u64;
    
    // Interleave bits using bit manipulation
    x = (x ^ (x >> 1)) & 0x5555555555555555;
    y = (y ^ (y >> 1)) & 0x5555555555555555;
    
    x = (x ^ (x >> 2)) & 0x3333333333333333;
    y = (y ^ (y >> 2)) & 0x3333333333333333;
    
    x = (x ^ (x >> 4)) & 0x0f0f0f0f0f0f0f0f;
    y = (y ^ (y >> 4)) & 0x0f0f0f0f0f0f0f0f;
    
    x = (x ^ (x >> 8)) & 0x00ff00ff00ff00ff;
    y = (y ^ (y >> 8)) & 0x00ff00ff00ff00ff;
    
    x = (x ^ (x >> 16)) & 0x0000ffff0000ffff;
    y = (y ^ (y >> 16)) & 0x0000ffff0000ffff;
    
    x = (x ^ (x >> 32)) & 0x00000000ffffffff;
    y = (y ^ (y >> 32)) & 0x00000000ffffffff;
    
    (x << 1) | y
}

fn main() {
    // Example: Generate Z-order curve for a 4x4 grid
    let width = 4;
    let height = 4;
    
    println!("Z-order curve for {}x{} grid:", width, height);
    let z_order_points = z_order_curve_generation(width, height);
    
    for (i, &(x, y)) in z_order_points.iter().enumerate() {
        println!("Point {}: ({}, {}) -> Z-order: {}", 
                 i + 1, x, y, z_order_value(x, y));
    }
    
    // Visual representation of the Z-order curve
    println!("\nVisual representation:");
    let mut grid = vec![vec![' '; width as usize]; height as usize];
    
    for (i, &(x, y)) in z_order_points.iter().enumerate() {
        grid[y as usize][x as usize] = (b'A' + i as u8) as char;
    }
    
    for row in grid {
        println!("{}", row.iter().collect::<String>());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_z_order_value() {
        // Test some known values
        assert_eq!(z_order_value(0, 0), 0);
        assert_eq!(z_order_value(1, 0), 1);
        assert_eq!(z_order_value(0, 1), 2);
        assert_eq!(z_order_value(1, 1), 3);
        assert_eq!(z_order_value(2, 0), 4);
        assert_eq!(z_order_value(0, 2), 8);
    }
    
    #[test]
    fn test_z_order_value_optimized() {
        assert_eq!(z_order_value_optimized(0, 0), 0);
        assert_eq!(z_order_value_optimized(1, 0), 1);
        assert_eq!(z_order_value_optimized(0, 1), 2);
        assert_eq!(z_order_value_optimized(1, 1), 3);
    }
}
```

## How it works:

1. **Z-order Value Calculation**: The algorithm interleaves the bits of x and y coordinates to create a single z-order value
2. **Bit Interleaving**: For coordinates (x,y), the algorithm alternates bits from x and y to create a single number
3. **Sorting**: Points are sorted by their z-order values to generate the curve

## Example Output:
```
Z-order curve for 4x4 grid:
Point 1: (0, 0) -> Z-order: 0
Point 2: (1, 0) -> Z-order: 1
Point 3: (0, 1) -> Z-order: 2
Point 4: (1, 1) -> Z-order: 3
Point 5: (2, 0) -> Z-order: 4
Point 6: (3, 0) -> Z-order: 5
Point 7: (2, 1) -> Z-order: 6
Point 8: (3, 1) -> Z-order: 7
Point 9: (0, 2) -> Z-order: 8
Point 10: (1, 2) -> Z-order: 9
Point 11: (0, 3) -> Z-order: 10
Point 12: (1, 3) -> Z-order: 11
Point 13: (2, 2) -> Z-order: 12
Point 14: (3, 2) -> Z-order: 13
Point 15: (2, 3) -> Z-order: 14
Point 16: (3, 3) -> Z-order: 15
```

## Key Features:

- **Bit Manipulation**: Efficient bit interleaving for optimal performance
- **Generic Implementation**: Works with any grid size
- **Sorted Output**: Points are ordered according to Z-order curve
- **Test Coverage**: Includes unit tests for verification
- **Visual Representation**: Shows the actual curve pattern

The Z-order curve is particularly useful in spatial indexing, image processing, and database indexing where spatial locality needs to be preserved.

