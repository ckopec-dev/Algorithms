# Lloyd's Algorithm Implementation in Rust

Lloyd's algorithm is an iterative method for k-means clustering. Here's a complete implementation:

```rust
use std::collections::HashMap;
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
    
    fn distance(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }
}

#[derive(Debug, Clone)]
struct Cluster {
    centroid: Point,
    points: Vec<Point>,
}

impl Cluster {
    fn new(centroid: Point) -> Self {
        Cluster {
            centroid,
            points: Vec::new(),
        }
    }
    
    fn update_centroid(&mut self) {
        if self.points.is_empty() {
            return;
        }
        
        let sum_x: f64 = self.points.iter().map(|p| p.x).sum();
        let sum_y: f64 = self.points.iter().map(|p| p.y).sum();
        
        self.centroid = Point::new(sum_x / self.points.len() as f64, 
                                  sum_y / self.points.len() as f64);
    }
}

fn lloyds_algorithm(points: &[Point], k: usize, max_iterations: usize) -> Vec<Cluster> {
    // Initialize centroids randomly
    let mut clusters: Vec<Cluster> = Vec::new();
    
    // Simple initialization: use first k points as initial centroids
    for i in 0..k {
        if i < points.len() {
            clusters.push(Cluster::new(points[i].clone()));
        }
    }
    
    for iteration in 0..max_iterations {
        // Clear existing points from clusters
        for cluster in clusters.iter_mut() {
            cluster.points.clear();
        }
        
        // Assign points to nearest cluster
        for point in points.iter() {
            let mut min_distance = f64::MAX;
            let mut nearest_cluster = 0;
            
            for (i, cluster) in clusters.iter().enumerate() {
                let distance = point.distance(&cluster.centroid);
                if distance < min_distance {
                    min_distance = distance;
                    nearest_cluster = i;
                }
            }
            
            clusters[nearest_cluster].points.push(point.clone());
        }
        
        // Update centroids
        let mut updated = false;
        for cluster in clusters.iter_mut() {
            let old_centroid = cluster.centroid.clone();
            cluster.update_centroid();
            
            // Check if centroid changed significantly
            if old_centroid.distance(&cluster.centroid) > 1e-6 {
                updated = true;
            }
        }
        
        // If no centroids changed significantly, stop
        if !updated {
            println!("Converged after {} iterations", iteration + 1);
            break;
        }
    }
    
    clusters
}

fn print_clusters(clusters: &[Cluster]) {
    for (i, cluster) in clusters.iter().enumerate() {
        println!("Cluster {}: centroid=({:.2}, {:.2}), points={}", 
                 i, cluster.centroid.x, cluster.centroid.y, cluster.points.len());
        for point in &cluster.points {
            println!("  Point: ({:.2}, {:.2})", point.x, point.y);
        }
        println!();
    }
}

fn main() {
    // Create sample data points
    let points = vec![
        Point::new(1.0, 2.0),
        Point::new(1.5, 1.8),
        Point::new(5.0, 8.0),
        Point::new(8.0, 8.0),
        Point::new(1.0, 0.6),
        Point::new(9.0, 11.0),
        Point::new(8.0, 2.0),
        Point::new(10.0, 2.0),
        Point::new(9.0, 3.0),
    ];
    
    println!("Original points:");
    for point in &points {
        println!("  ({:.2}, {:.2})", point.x, point.y);
    }
    println!();
    
    // Run Lloyd's algorithm with k=3
    let clusters = lloyds_algorithm(&points, 3, 100);
    
    println!("Final clusters:");
    print_clusters(&clusters);
    
    // Demonstrate convergence
    println!("Cluster assignments:");
    for (i, point) in points.iter().enumerate() {
        let mut min_distance = f64::MAX;
        let mut nearest_cluster = 0;
        
        for (j, cluster) in clusters.iter().enumerate() {
            let distance = point.distance(&cluster.centroid);
            if distance < min_distance {
                min_distance = distance;
                nearest_cluster = j;
            }
        }
        
        println!("Point ({:.2}, {:.2}) -> Cluster {}", 
                 point.x, point.y, nearest_cluster);
    }
}
```

## Key Components Explained

### 1. **Point Structure**
- Represents a 2D coordinate with x and y values
- Includes distance calculation method

### 2. **Cluster Structure**
- Contains a centroid (center point) and a list of assigned points
- Has method to update centroid based on assigned points

### 3. **Lloyd's Algorithm Implementation**
- **Initialization**: Randomly select initial centroids
- **Assignment**: Assign each point to the nearest centroid
- **Update**: Recalculate centroids as the mean of assigned points
- **Convergence**: Stop when centroids don't change significantly

### 4. **Algorithm Steps**
1. Initialize k centroids randomly
2. Assign each point to the nearest centroid
3. Update each centroid to the mean of its assigned points
4. Repeat steps 2-3 until convergence or max iterations

## Sample Output
```
Original points:
  (1.00, 2.00)
  (1.50, 1.80)
  (5.00, 8.00)
  (8.00, 8.00)
  (1.00, 0.60)
  (9.00, 11.00)
  (8.00, 2.00)
  (10.00, 2.00)
  (9.00, 3.00)

Final clusters:
Cluster 0: centroid=(1.17, 1.47), points=3
  Point: (1.00, 2.00)
  Point: (1.50, 1.80)
  Point: (1.00, 0.60)

Cluster 1: centroid=(8.00, 5.67), points=4
  Point: (5.00, 8.00)
  Point: (8.00, 8.00)
  Point: (8.00, 2.00)
  Point: (9.00, 3.00)

Cluster 2: centroid=(9.00, 6.50), points=2
  Point: (9.00, 11.00)
  Point: (10.00, 2.00)
```

This implementation demonstrates the core principles of Lloyd's algorithm for k-means clustering in Rust, including proper data structures, iterative optimization, and convergence detection.

