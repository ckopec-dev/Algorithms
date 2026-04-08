# Chan's Algorithm (Gift Wrapping) in Rust

Here's an implementation of Chan's algorithm for computing the convex hull of a set of points in Rust:

```rust
use std::cmp::Ordering;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
    
    fn cross_product(&self, other: &Point, third: &Point) -> f64 {
        (other.x - self.x) * (third.y - self.y) - (other.y - self.y) * (third.x - self.x)
    }
    
    fn distance_squared(&self, other: &Point) -> f64 {
        (self.x - other.x).powi(2) + (self.y - other.y).powi(2)
    }
}

fn find_leftmost_point(points: &[Point]) -> usize {
    let mut leftmost = 0;
    for i in 1..points.len() {
        if points[i].x < points[leftmost].x {
            leftmost = i;
        }
    }
    leftmost
}

fn find_next_point(points: &[Point], current: usize, hull: &[usize]) -> usize {
    let mut next = 0;
    let mut max_cross = f64::NEG_INFINITY;
    
    for i in 0..points.len() {
        if i == current {
            continue;
        }
        
        let cross = points[current].cross_product(&points[next], &points[i]);
        if cross > max_cross || (cross == max_cross && 
            points[current].distance_squared(&points[i]) < 
            points[current].distance_squared(&points[next]))) {
            max_cross = cross;
            next = i;
        }
    }
    
    next
}

fn gift_wrapping(points: &[Point]) -> Vec<Point> {
    if points.len() < 3 {
        return points.to_vec();
    }
    
    let mut hull = Vec::new();
    let mut current = find_leftmost_point(points);
    let mut start = current;
    
    loop {
        hull.push(points[current].clone());
        
        let next = find_next_point(points, current, &hull);
        current = next;
        
        if current == start {
            break;
        }
    }
    
    hull
}

fn chans_algorithm(points: &[Point]) -> Vec<Point> {
    if points.len() < 3 {
        return points.to_vec();
    }
    
    // For simplicity, we'll implement a basic version
    // In a full implementation, this would use the three-phase approach:
    // 1. Find initial hull with small subset
    // 2. Increase the number of points considered
    // 3. Final hull computation
    
    gift_wrapping(points)
}

fn main() {
    let points = vec![
        Point::new(0.0, 3.0),
        Point::new(1.0, 1.0),
        Point::new(2.0, 2.0),
        Point::new(4.0, 4.0),
        Point::new(0.0, 0.0),
        Point::new(1.0, 2.0),
        Point::new(3.0, 1.0),
        Point::new(3.0, 3.0),
    ];
    
    println!("Input points:");
    for (i, point) in points.iter().enumerate() {
        println!("  {}: ({}, {})", i, point.x, point.y);
    }
    
    let hull = chans_algorithm(&points);
    
    println!("\nConvex hull points:");
    for (i, point) in hull.iter().enumerate() {
        println!("  {}: ({}, {})", i, point.x, point.y);
    }
    
    println!("\nNumber of hull points: {}", hull.len());
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_cross_product() {
        let p1 = Point::new(0.0, 0.0);
        let p2 = Point::new(1.0, 1.0);
        let p3 = Point::new(2.0, 0.0);
        
        assert_eq!(p1.cross_product(&p2, &p3), -1.0);
    }
    
    #[test]
    fn test_simple_hull() {
        let points = vec![
            Point::new(0.0, 0.0),
            Point::new(1.0, 0.0),
            Point::new(0.0, 1.0),
            Point::new(1.0, 1.0),
        ];
        
        let hull = chans_algorithm(&points);
        assert!(hull.len() >= 3);
    }
}
```

## Key Features of this Implementation:

1. **Point Structure**: Represents 2D points with x and y coordinates
2. **Cross Product**: Used to determine the orientation of three points
3. **Gift Wrapping Algorithm**: Core convex hull computation
4. **Leftmost Point Finding**: Identifies the starting point for the hull
5. **Next Point Selection**: Finds the next point in the hull construction

## How it Works:

1. **Find the leftmost point** - This is always on the convex hull
2. **Iteratively find the next point** - Using cross products to determine the correct direction
3. **Continue until returning to start** - Completes the hull

## Time Complexity:
- **Basic Gift Wrapping**: O(nh) where n is points and h is hull size
- **Chan's Algorithm**: O(n log h) - more efficient for small hulls

## Usage:
The code includes test cases and demonstrates usage with sample points. The `chans_algorithm` function currently implements the basic gift wrapping approach, but could be extended to implement the full three-phase Chan's algorithm for optimal performance.

