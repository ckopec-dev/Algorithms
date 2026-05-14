# Delaunay Triangulation in Rust

Here's a complete example of a Delaunay triangulation implementation in Rust using the `delaunator` crate:

## Cargo.toml
```toml
[package]
name = "delaunay_example"
version = "0.1.0"
edition = "2021"

[dependencies]
delaunator = "1.0.0"
```

## src/main.rs
```rust
use delaunator::{Point, Triangulation};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Vertex {
    x: f64,
    y: f64,
}

impl Vertex {
    fn new(x: f64, y: f64) -> Self {
        Vertex { x, y }
    }
}

fn main() {
    // Create a set of 2D points
    let points = vec![
        Vertex::new(0.0, 0.0),
        Vertex::new(1.0, 0.0),
        Vertex::new(0.0, 1.0),
        Vertex::new(1.0, 1.0),
        Vertex::new(0.5, 0.5),
        Vertex::new(2.0, 0.0),
        Vertex::new(0.0, 2.0),
        Vertex::new(2.0, 2.0),
    ];

    // Convert to delaunator Point format
    let points_delaunator: Vec<Point> = points
        .iter()
        .map(|v| Point { x: v.x, y: v.y })
        .collect();

    // Perform Delaunay triangulation
    let triangulation = Triangulation::new(&points_delaunator);

    println!("Delaunay Triangulation Results:");
    println!("Number of triangles: {}", triangulation.triangles.len() / 3);
    println!("Number of points: {}", points_delaunator.len());

    // Print triangle indices
    println!("\nTriangle indices:");
    for i in (0..triangulation.triangles.len()).step_by(3) {
        let a = triangulation.triangles[i];
        let b = triangulation.triangles[i + 1];
        let c = triangulation.triangles[i + 2];
        println!("Triangle {}: {}, {}, {}", i/3, a, b, c);
    }

    // Print triangle vertices
    println!("\nTriangle vertices:");
    for i in (0..triangulation.triangles.len()).step_by(3) {
        let a = triangulation.triangles[i];
        let b = triangulation.triangles[i + 1];
        let c = triangulation.triangles[i + 2];
        
        let point_a = &points_delaunator[a];
        let point_b = &points_delaunator[b];
        let point_c = &points_delaunator[c];
        
        println!("Triangle {}: ({:.1}, {:.1}), ({:.1}, {:.1}), ({:.1}, {:.1})",
                 i/3,
                 point_a.x, point_a.y,
                 point_b.x, point_b.y,
                 point_c.x, point_c.y);
    }

    // Print the Voronoi diagram (dual of Delaunay)
    println!("\nVoronoi diagram vertices:");
    for (i, vertex) in triangulation.voronoi.vertices.iter().enumerate() {
        println!("Vertex {}: ({:.1}, {:.1})", i, vertex.x, vertex.y);
    }
}
```

## Alternative Implementation with Manual Approach

If you want to implement the algorithm manually without external dependencies:

```rust
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
}

#[derive(Debug, Clone)]
struct Triangle {
    a: Point,
    b: Point,
    c: Point,
}

impl Triangle {
    fn new(a: Point, b: Point, c: Point) -> Self {
        Triangle { a, b, c }
    }

    // Check if a point is inside the circumcircle of this triangle
    fn circumcircle_contains(&self, p: Point) -> bool {
        // Calculate the circumcircle center and radius
        // This is a simplified version - in practice, you'd use a more robust method
        let (cx, cy, r2) = self.circumcircle();
        
        let dx = p.x - cx;
        let dy = p.y - cy;
        dx * dx + dy * dy < r2
    }

    fn circumcircle(&self) -> (f64, f64, f64) {
        // Calculate circumcircle center and squared radius
        // This is a simplified implementation
        let (x1, y1) = (self.a.x, self.a.y);
        let (x2, y2) = (self.b.x, self.b.y);
        let (x3, y3) = (self.c.x, self.c.y);
        
        let d = 2.0 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2));
        
        if d.abs() < 1e-10 {
            // Collinear points - return a reasonable fallback
            return ((x1 + x2 + x3) / 3.0, (y1 + y2 + y3) / 3.0, 1.0);
        }
        
        let ux = ((x1 * x1 + y1 * y1) * (y2 - y3) + 
                 (x2 * x2 + y2 * y2) * (y3 - y1) + 
                 (x3 * x3 + y3 * y3) * (y1 - y2)) / d;
        let uy = ((x1 * x1 + y1 * y1) * (x3 - x2) + 
                 (x2 * x2 + y2 * y2) * (x1 - x3) + 
                 (x3 * x3 + y3 * y3) * (x2 - x1)) / d;
        
        let r2 = (x1 - ux) * (x1 - ux) + (y1 - uy) * (y1 - uy);
        
        (ux, uy, r2)
    }
}

fn delaunay_triangulation(points: &[Point]) -> Vec<Triangle> {
    // This is a simplified version - a full implementation would be much more complex
    // For demonstration purposes, we'll just create a basic triangulation
    
    let mut triangles = Vec::new();
    
    if points.len() < 3 {
        return triangles;
    }
    
    // Simple triangulation using a basic algorithm
    // In practice, you'd use a proper Delaunay implementation
    for i in 0..points.len() - 2 {
        for j in (i + 1)..points.len() - 1 {
            for k in (j + 1)..points.len() {
                triangles.push(Triangle::new(points[i], points[j], points[k]));
            }
        }
    }
    
    triangles
}

fn main() {
    let points = vec![
        Point::new(0.0, 0.0),
        Point::new(1.0, 0.0),
        Point::new(0.0, 1.0),
        Point::new(1.0, 1.0),
        Point::new(0.5, 0.5),
    ];

    let triangles = delaunay_triangulation(&points);
    
    println!("Generated {} triangles:", triangles.len());
    for (i, triangle) in triangles.iter().enumerate() {
        println!("Triangle {}: ({:.1}, {:.1}), ({:.1}, {:.1}), ({:.1}, {:.1})",
                 i,
                 triangle.a.x, triangle.a.y,
                 triangle.b.x, triangle.b.y,
                 triangle.c.x, triangle.c.y);
    }
}
```

## Key Features

1. **External Crate Usage**: The first example uses the `delaunator` crate which provides a robust, optimized implementation
2. **Point Representation**: Points are represented as 2D coordinates
3. **Triangle Output**: Returns triangle indices and vertex coordinates
4. **Voronoi Diagram**: The library also provides dual Voronoi diagram information
5. **Performance**: The `delaunator` crate is highly optimized and handles edge cases well

## Usage

To run the example:

```bash
cargo run
```

The output will show the triangulation results including triangle indices and vertex coordinates, demonstrating the Delaunay property where no point lies inside the circumcircle of any triangle.

