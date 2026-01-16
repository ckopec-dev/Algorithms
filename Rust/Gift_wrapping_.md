# Gift Wrapping Algorithm (Jarvis March) in Rust

Here's an implementation of the gift wrapping algorithm to find the convex hull of a set of points:

```rust
use std::cmp::Ordering;

#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl Eq for Point {}

// Calculate cross product of three points
fn cross_product(p1: &Point, p2: &Point, p3: &Point) -> i64 {
    (p2.x as i64 - p1.x as i64) * (p3.y as i64 - p2.y as i64) 
    - (p2.y as i64 - p1.y as i64) * (p3.x as i64 - p2.x as i64)
}

// Find the leftmost point (lowest y-coordinate, then lowest x-coordinate)
fn find_leftmost_point(points: &[Point]) -> usize {
    let mut leftmost = 0;
    for i in 1..points.len() {
        if points[i].y < points[leftmost].y || 
           (points[i].y == points[leftmost].y && points[i].x < points[leftmost].x) {
            leftmost = i;
        }
    }
    leftmost
}

// Find the next point in the convex hull
fn find_next_point(points: &[Point], current: &Point, hull: &[Point]) -> Point {
    let mut next = points[0].clone();
    
    for i in 1..points.len() {
        // Skip if it's the current point
        if points[i] == *current {
            continue;
        }
        
        // If next is the first point or if the cross product indicates a left turn
        let cross = cross_product(&current, &next, &points[i]);
        if next == points[0] || cross > 0 || (cross == 0 && 
            (points[i].x - current.x).pow(2) + (points[i].y - current.y).pow(2) >
            (next.x - current.x).pow(2) + (next.y - current.y).pow(2)) {
            next = points[i].clone();
        }
    }
    
    next
}

// Gift wrapping algorithm (Jarvis March) to find convex hull
fn gift_wrapping(points: &[Point]) -> Vec<Point> {
    if points.len() < 3 {
        return points.to_vec();
    }
    
    let mut hull = Vec::new();
    
    // Find the leftmost point
    let leftmost = find_leftmost_point(points);
    let mut current = points[leftmost].clone();
    
    // Start with the leftmost point
    hull.push(current.clone());
    
    // Keep wrapping until we return to the starting point
    loop {
        let next = find_next_point(points, &current, &hull);
        
        // If we've returned to the starting point, we're done
        if next == points[leftmost] {
            break;
        }
        
        current = next.clone();
        hull.push(current.clone());
    }
    
    hull
}

fn main() {
    // Example points
    let points = vec![
        Point::new(0, 3),
        Point::new(1, 1),
        Point::new(2, 2),
        Point::new(4, 4),
        Point::new(0, 0),
        Point::new(1, 2),
        Point::new(3, 1),
        Point::new(3, 3),
    ];
    
    println!("Input points:");
    for point in &points {
        println!("({}, {})", point.x, point.y);
    }
    
    let hull = gift_wrapping(&points);
    
    println!("\nConvex hull points:");
    for point in &hull {
        println!("({}, {})", point.x, point.y);
    }
    
    println!("\nNumber of points in convex hull: {}", hull.len());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cross_product() {
        let p1 = Point::new(0, 0);
        let p2 = Point::new(1, 1);
        let p3 = Point::new(2, 2);
        
        assert_eq!(cross_product(&p1, &p2, &p3), 0); // Collinear
    }
    
    #[test]
    fn test_simple_hull() {
        let points = vec![
            Point::new(0, 0),
            Point::new(1, 0),
            Point::new(0, 1),
            Point::new(1, 1),
        ];
        
        let hull = gift_wrapping(&points);
        assert_eq!(hull.len(), 4); // All points should be on hull
    }
}
```

## How it works:

1. **Find the leftmost point**: The algorithm starts with the point that has the lowest y-coordinate (and lowest x-coordinate if there's a tie).

2. **Find the next point**: For each point in the hull, it finds the next point by checking which point makes the largest left turn when viewed from the current point.

3. **Continue until completion**: The algorithm continues until it returns to the starting point, completing the convex hull.

## Key components:

- **Cross product calculation**: Determines the orientation of three points (clockwise, counterclockwise, or collinear)
- **Point comparison**: Handles the logic for finding the next point in the hull
- **Edge cases**: Handles cases with fewer than 3 points

## Time Complexity:
- **O(nh)** where n is the number of input points and h is the number of points on the hull
- **O(n²)** in the worst case when all points are on the hull

## Space Complexity:
- **O(h)** for storing the convex hull points

The algorithm is particularly useful for understanding computational geometry concepts and is the foundation for more advanced convex hull algorithms.

