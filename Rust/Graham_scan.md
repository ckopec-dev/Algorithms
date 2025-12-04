# Graham Scan Algorithm in Rust

Here's a complete implementation of the Graham scan algorithm for finding the convex hull of a set of points:

```rust
use std::f64;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
}

// Calculate cross product of three points
fn cross_product(o: &Point, a: &Point, b: &Point) -> f64 {
    (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)
}

// Calculate distance between two points
fn distance(p1: &Point, p2: &Point) -> f64 {
    ((p1.x - p2.x).powi(2) + (p1.y - p2.y).powi(2)).sqrt()
}

// Find the point with the lowest y-coordinate (and leftmost if tie)
fn find_lowest_point(points: &[Point]) -> usize {
    let mut lowest_index = 0;
    let mut lowest_y = points[0].y;
    
    for i in 1..points.len() {
        if points[i].y < lowest_y || (points[i].y == lowest_y && points[i].x < points[lowest_index].x) {
            lowest_y = points[i].y;
            lowest_index = i;
        }
    }
    
    lowest_index
}

// Sort points by polar angle with respect to the lowest point
fn sort_by_polar_angle(mut points: Vec<Point>, lowest_point: &Point) -> Vec<Point> {
    points.sort_by(|a, b| {
        let cross = cross_product(lowest_point, a, b);
        if cross == 0.0 {
            // If collinear, sort by distance from lowest point
            distance(lowest_point, a).partial_cmp(&distance(lowest_point, b)).unwrap()
        } else {
            // Sort by polar angle (counter-clockwise)
            cross.partial_cmp(&0.0).unwrap()
        }
    });
    
    points
}

fn graham_scan(mut points: Vec<Point>) -> Vec<Point> {
    if points.len() < 3 {
        return points;
    }
    
    // Find the lowest point
    let lowest_index = find_lowest_point(&points);
    let lowest_point = points.remove(lowest_index);
    
    // Sort points by polar angle
    let sorted_points = sort_by_polar_angle(points, &lowest_point);
    
    // Initialize the hull with the lowest point and first two points
    let mut hull = vec![lowest_point.clone()];
    
    if !sorted_points.is_empty() {
        hull.push(sorted_points[0].clone());
    }
    
    if sorted_points.len() > 1 {
        hull.push(sorted_points[1].clone());
    }
    
    // Process remaining points
    for i in 2..sorted_points.len() {
        let current_point = &sorted_points[i];
        
        // Remove points that make clockwise turns
        while hull.len() > 1 {
            let second_last = &hull[hull.len() - 2];
            let last = &hull[hull.len() - 1];
            
            if cross_product(second_last, last, current_point) > 0.0 {
                // Counter-clockwise turn, keep the last point
                break;
            } else {
                // Clockwise turn, remove the last point
                hull.pop();
            }
        }
        
        hull.push(current_point.clone());
    }
    
    hull
}

fn main() {
    // Example points
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
    
    let hull = graham_scan(points);
    
    println!("\nConvex hull points:");
    for (i, point) in hull.iter().enumerate() {
        println!("  {}: ({}, {})", i, point.x, point.y);
    }
    
    println!("\nNumber of points in convex hull: {}", hull.len());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_case() {
        let points = vec![
            Point::new(0.0, 0.0),
            Point::new(1.0, 0.0),
            Point::new(0.0, 1.0),
            Point::new(1.0, 1.0),
        ];
        
        let hull = graham_scan(points);
        assert_eq!(hull.len(), 4); // All points form the hull
    }
    
    #[test]
    fn test_collinear_points() {
        let points = vec![
            Point::new(0.0, 0.0),
            Point::new(1.0, 1.0),
            Point::new(2.0, 2.0),
            Point::new(3.0, 3.0),
        ];
        
        let hull = graham_scan(points);
        // Should return the first and last points (the extreme points)
        assert_eq!(hull.len(), 2);
    }
}
```

## How it works:

1. **Find the lowest point**: Identify the point with the smallest y-coordinate (and leftmost if tied)
2. **Sort by polar angle**: Sort all other points by their polar angle with respect to the lowest point
3. **Build the hull**: Use a stack-based approach to construct the convex hull by removing points that create clockwise turns

## Key Features:

- **Cross product calculation** to determine turn direction
- **Proper sorting** by polar angle using cross product
- **Handles edge cases** like collinear points
- **Includes unit tests** for verification
- **Memory efficient** implementation

## Time Complexity:
- **O(n log n)** due to sorting
- **O(n)** for the actual hull construction

## Space Complexity:
- **O(n)** for storing the points and hull

The algorithm correctly identifies the convex hull of the given set of points, which is the smallest convex polygon that contains all the points.

