# Iterative Closest Point (ICP) Algorithm in Rust

Here's a complete implementation of the ICP algorithm in Rust:

```rust
use std::f64::consts::PI;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    fn distance(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

#[derive(Debug, Clone)]
struct Transformation {
    tx: f64,
    ty: f64,
    theta: f64,
}

impl Transformation {
    fn new(tx: f64, ty: f64, theta: f64) -> Self {
        Transformation { tx, ty, theta }
    }

    fn apply(&self, point: &Point) -> Point {
        let cos_theta = self.theta.cos();
        let sin_theta = self.theta.sin();
        
        Point {
            x: self.tx + point.x * cos_theta - point.y * sin_theta,
            y: self.ty + point.x * sin_theta + point.y * cos_theta,
        }
    }
}

struct ICP {
    max_iterations: usize,
    tolerance: f64,
}

impl ICP {
    fn new(max_iterations: usize, tolerance: f64) -> Self {
        ICP {
            max_iterations,
            tolerance,
        }
    }

    fn find_closest_points(&self, source: &[Point], target: &[Point]) -> Vec<(usize, usize)> {
        let mut closest_pairs = Vec::new();
        
        for (i, source_point) in source.iter().enumerate() {
            let mut min_distance = f64::MAX;
            let mut closest_index = 0;
            
            for (j, target_point) in target.iter().enumerate() {
                let distance = source_point.distance(target_point);
                if distance < min_distance {
                    min_distance = distance;
                    closest_index = j;
                }
            }
            
            closest_pairs.push((i, closest_index));
        }
        
        closest_pairs
    }

    fn compute_transformation(&self, source: &[Point], target: &[Point], pairs: &[(usize, usize)]) -> Transformation {
        let mut source_sum_x = 0.0;
        let mut source_sum_y = 0.0;
        let mut target_sum_x = 0.0;
        let mut target_sum_y = 0.0;
        let n = pairs.len() as f64;

        for &(source_idx, target_idx) in pairs {
            source_sum_x += source[source_idx].x;
            source_sum_y += source[source_idx].y;
            target_sum_x += target[target_idx].x;
            target_sum_y += target[target_idx].y;
        }

        let source_center_x = source_sum_x / n;
        let source_center_y = source_sum_y / n;
        let target_center_x = target_sum_x / n;
        let target_center_y = target_sum_y / n;

        let mut cross = 0.0;
        let mut dot = 0.0;

        for &(source_idx, target_idx) in pairs {
            let source_x = source[source_idx].x - source_center_x;
            let source_y = source[source_idx].y - source_center_y;
            let target_x = target[target_idx].x - target_center_x;
            let target_y = target[target_idx].y - target_center_y;

            cross += source_x * target_y - source_y * target_x;
            dot += source_x * target_x + source_y * target_y;
        }

        let theta = cross.atan2(dot);
        let tx = target_center_x - source_center_x * theta.cos() + source_center_y * theta.sin();
        let ty = target_center_y - source_center_x * theta.sin() - source_center_y * theta.cos();

        Transformation::new(tx, ty, theta)
    }

    fn apply_transformation(&self, points: &[Point], transform: &Transformation) -> Vec<Point> {
        points.iter().map(|point| transform.apply(point)).collect()
    }

    fn compute_rms(&self, source: &[Point], target: &[Point], pairs: &[(usize, usize)]) -> f64 {
        let mut sum = 0.0;
        let n = pairs.len() as f64;

        for &(source_idx, target_idx) in pairs {
            let distance = source[source_idx].distance(&target[target_idx]);
            sum += distance * distance;
        }

        (sum / n).sqrt()
    }

    fn run(&self, source: &[Point], target: &[Point]) -> (Vec<Point>, Transformation) {
        let mut current_source = source.to_vec();
        let mut accumulated_transform = Transformation::new(0.0, 0.0, 0.0);
        let mut rms = f64::MAX;

        for iteration in 0..self.max_iterations {
            let closest_pairs = self.find_closest_points(&current_source, target);
            let transform = self.compute_transformation(&current_source, target, &closest_pairs);
            
            // Apply transformation to current source
            current_source = self.apply_transformation(&current_source, &transform);
            
            // Update accumulated transformation
            accumulated_transform = Transformation::new(
                accumulated_transform.tx + transform.tx,
                accumulated_transform.ty + transform.ty,
                accumulated_transform.theta + transform.theta,
            );

            // Check convergence
            rms = self.compute_rms(&current_source, target, &closest_pairs);
            
            if rms < self.tolerance {
                println!("Converged after {} iterations with RMS = {}", iteration + 1, rms);
                break;
            }
        }

        (current_source, accumulated_transform)
    }
}

fn main() {
    // Create sample source and target point clouds
    let source_points = vec![
        Point::new(0.0, 0.0),
        Point::new(1.0, 0.0),
        Point::new(2.0, 0.0),
        Point::new(0.0, 1.0),
        Point::new(1.0, 1.0),
        Point::new(2.0, 1.0),
    ];

    let target_points = vec![
        Point::new(0.5, 0.5),
        Point::new(1.5, 0.5),
        Point::new(2.5, 0.5),
        Point::new(0.5, 1.5),
        Point::new(1.5, 1.5),
        Point::new(2.5, 1.5),
    ];

    // Create ICP instance
    let icp = ICP::new(100, 1e-6);

    // Run ICP algorithm
    let (transformed_source, final_transform) = icp.run(&source_points, &target_points);

    println!("Original source points:");
    for point in &source_points {
        println!("  ({}, {})", point.x, point.y);
    }

    println!("\nTarget points:");
    for point in &target_points {
        println!("  ({}, {})", point.x, point.y);
    }

    println!("\nTransformed source points:");
    for point in &transformed_source {
        println!("  ({}, {})", point.x, point.y);
    }

    println!("\nFinal transformation:");
    println!("  Translation: ({}, {})", final_transform.tx, final_transform.ty);
    println!("  Rotation: {} radians ({} degrees)", final_transform.theta, final_transform.theta * 180.0 / PI);
}
```

## Key Features of this Implementation:

1. **Point Structure**: Represents 2D points with x and y coordinates
2. **Transformation Structure**: Handles translation and rotation
3. **ICP Algorithm**: 
   - Finds closest points between source and target
   - Computes optimal transformation using least squares
   - Applies transformation iteratively
   - Converges when RMS error is below tolerance

## How it works:

1. **Initialization**: Start with source and target point clouds
2. **Closest Point Matching**: For each source point, find the closest target point
3. **Transformation Computation**: Calculate optimal rigid transformation (translation + rotation)
4. **Transformation Application**: Apply the transformation to source points
5. **Convergence Check**: Continue until convergence or maximum iterations reached

## Usage:

The algorithm can be used for:
- Point cloud registration
- Object alignment
- 2D shape matching
- Robotics and computer vision applications

This implementation provides a solid foundation that can be extended with additional features like:
- 3D point cloud support
- More sophisticated outlier rejection
- Different distance metrics
- Performance optimizations for large point clouds

