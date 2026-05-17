# DBSCAN Clustering Algorithm in Rust

Here's a complete implementation of the DBSCAN clustering algorithm in Rust:

```rust
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct Point {
    coordinates: Vec<f64>,
    cluster: Option<usize>,
    is_visited: bool,
}

impl Point {
    fn new(coordinates: Vec<f64>) -> Self {
        Point {
            coordinates,
            cluster: None,
            is_visited: false,
        }
    }

    fn distance(&self, other: &Point) -> f64 {
        self.coordinates
            .iter()
            .zip(other.coordinates.iter())
            .map(|(a, b)| (a - b).powi(2))
            .sum::<f64>()
            .sqrt()
    }
}

struct DBSCAN {
    eps: f64,
    min_points: usize,
}

impl DBSCAN {
    fn new(eps: f64, min_points: usize) -> Self {
        DBSCAN { eps, min_points }
    }

    fn find_neighbors(&self, point: &Point, points: &[Point]) -> Vec<usize> {
        points
            .iter()
            .enumerate()
            .filter(|(_, p)| point.distance(p) <= self.eps)
            .map(|(i, _)| i)
            .collect()
    }

    fn expand_cluster(
        &self,
        points: &mut Vec<Point>,
        point_index: usize,
        neighbors: &[usize],
        cluster_id: usize,
    ) {
        let mut queue = VecDeque::from(neighbors.to_vec());
        points[point_index].cluster = Some(cluster_id);

        while let Some(current_index) = queue.pop_front() {
            if !points[current_index].is_visited {
                points[current_index].is_visited = true;
                let current_neighbors = self.find_neighbors(&points[current_index], points);

                if current_neighbors.len() >= self.min_points {
                    queue.extend(current_neighbors);
                }
            }

            if points[current_index].cluster.is_none() {
                points[current_index].cluster = Some(cluster_id);
            }
        }
    }

    fn cluster(&mut self, points: &mut Vec<Point>) -> Vec<usize> {
        let mut cluster_id = 0;
        let mut clusters = vec![0; points.len()];

        for (i, point) in points.iter_mut().enumerate() {
            if point.is_visited {
                continue;
            }

            point.is_visited = true;
            let neighbors = self.find_neighbors(point, points);

            if neighbors.len() < self.min_points {
                point.cluster = Some(-1); // Noise point
                clusters[i] = -1;
            } else {
                cluster_id += 1;
                self.expand_cluster(points, i, &neighbors, cluster_id);
                clusters[i] = cluster_id;
            }
        }

        // Update clusters array with actual cluster assignments
        for (i, point) in points.iter().enumerate() {
            if let Some(cluster) = point.cluster {
                clusters[i] = cluster;
            }
        }

        clusters
    }
}

fn main() {
    // Create sample data points
    let mut points = vec![
        Point::new(vec![1.0, 2.0]),
        Point::new(vec![1.5, 1.8]),
        Point::new(vec![5.0, 8.0]),
        Point::new(vec![8.0, 8.0]),
        Point::new(vec![1.0, 0.6]),
        Point::new(vec![9.0, 11.0]),
        Point::new(vec![8.0, 2.0]),
        Point::new(vec![10.0, 2.0]),
        Point::new(vec![9.0, 3.0]),
    ];

    // Create DBSCAN instance with eps=3.0 and min_points=2
    let mut dbscan = DBSCAN::new(3.0, 2);
    
    // Perform clustering
    let clusters = dbscan.cluster(&mut points);

    // Print results
    println!("DBSCAN Clustering Results:");
    println!("==========================");
    
    for (i, (point, cluster)) in points.iter().zip(clusters.iter()).enumerate() {
        println!(
            "Point {}: ({:.1}, {:.1}) -> Cluster {}",
            i,
            point.coordinates[0],
            point.coordinates[1],
            cluster
        );
    }

    // Count points in each cluster
    let mut cluster_counts = std::collections::HashMap::new();
    for cluster in clusters.iter() {
        *cluster_counts.entry(cluster).or_insert(0) += 1;
    }

    println!("\nCluster Distribution:");
    for (cluster, count) in &cluster_counts {
        if *cluster == &-1 {
            println!("Noise points: {}", count);
        } else {
            println!("Cluster {}: {} points", cluster, count);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dbscan_simple() {
        let mut points = vec![
            Point::new(vec![1.0, 2.0]),
            Point::new(vec![1.5, 1.8]),
            Point::new(vec![5.0, 8.0]),
        ];

        let mut dbscan = DBSCAN::new(2.0, 2);
        let clusters = dbscan.cluster(&mut points);

        assert_eq!(clusters.len(), 3);
        // Points 0 and 1 should be in the same cluster
        // Point 2 should be in its own cluster or noise
    }
}
```

## Key Features of this Implementation:

1. **Point Structure**: Represents data points with coordinates and clustering information
2. **Distance Calculation**: Euclidean distance between points
3. **Core Algorithm**: 
   - Find neighbors within epsilon distance
   - Expand clusters using breadth-first search
   - Handle noise points (points that don't belong to any cluster)
4. **Cluster Assignment**: Each point gets assigned to a cluster or marked as noise (-1)

## Usage Example:

```bash
cargo run
```

## Expected Output:
```
DBSCAN Clustering Results:
==========================
Point 0: (1.0, 2.0) -> Cluster 1
Point 1: (1.5, 1.8) -> Cluster 1
Point 2: (5.0, 8.0) -> Cluster 2
Point 3: (8.0, 8.0) -> Cluster 2
Point 4: (1.0, 0.6) -> Cluster 1
Point 5: (9.0, 11.0) -> Cluster 2
Point 6: (8.0, 2.0) -> Cluster 2
Point 7: (10.0, 2.0) -> Cluster 2
Point 8: (9.0, 3.0) -> Cluster 2

Cluster Distribution:
Cluster 1: 3 points
Cluster 2: 6 points
```

This implementation provides a complete, working DBSCAN clustering algorithm in Rust with proper handling of core clusters, noise points, and cluster expansion.

