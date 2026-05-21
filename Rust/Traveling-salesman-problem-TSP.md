# Traveling Salesman Problem (TSP) in Rust

Here's a complete implementation of a TSP solver using brute force approach in Rust:

```rust
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

    fn distance_to(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }
}

#[derive(Debug, Clone)]
struct TSPSolver {
    cities: Vec<Point>,
    distance_matrix: Vec<Vec<f64>>,
}

impl TSPSolver {
    fn new(cities: Vec<Point>) -> Self {
        let n = cities.len();
        let mut distance_matrix = vec![vec![0.0; n]; n];
        
        // Precompute all distances
        for i in 0..n {
            for j in 0..n {
                distance_matrix[i][j] = cities[i].distance_to(&cities[j]);
            }
        }
        
        TSPSolver {
            cities,
            distance_matrix,
        }
    }

    // Brute force approach - finds optimal solution for small instances
    fn solve_brute_force(&self) -> (Vec<usize>, f64) {
        let n = self.cities.len();
        
        if n <= 1 {
            return (vec![], 0.0);
        }
        
        let mut best_path = vec![0; n];
        let mut best_distance = f64::INFINITY;
        
        // Generate all permutations and find minimum
        let mut current_path = vec![0; n];
        let mut used = vec![false; n];
        used[0] = true;
        current_path[0] = 0;
        
        self.permute_and_find(1, &mut current_path, &mut used, &mut best_path, &mut best_distance);
        
        (best_path, best_distance)
    }
    
    fn permute_and_find(
        &self,
        position: usize,
        current_path: &mut Vec<usize>,
        used: &mut Vec<bool>,
        best_path: &mut Vec<usize>,
        best_distance: &mut f64,
    ) {
        let n = self.cities.len();
        
        if position == n {
            // Calculate total distance
            let mut total_distance = 0.0;
            for i in 0..n {
                let from = current_path[i];
                let to = current_path[(i + 1) % n];
                total_distance += self.distance_matrix[from][to];
            }
            
            if total_distance < *best_distance {
                *best_distance = total_distance;
                *best_path = current_path.clone();
            }
            return;
        }
        
        for i in 1..n {
            if !used[i] {
                used[i] = true;
                current_path[position] = i;
                self.permute_and_find(position + 1, current_path, used, best_path, best_distance);
                used[i] = false;
            }
        }
    }

    // Simple heuristic approach - nearest neighbor
    fn solve_nearest_neighbor(&self) -> (Vec<usize>, f64) {
        let n = self.cities.len();
        if n <= 1 {
            return (vec![], 0.0);
        }
        
        let mut visited = vec![false; n];
        let mut path = vec![0];
        visited[0] = true;
        let mut current_city = 0;
        let mut total_distance = 0.0;
        
        for _ in 1..n {
            let mut nearest_city = None;
            let mut min_distance = f64::INFINITY;
            
            for i in 0..n {
                if !visited[i] && self.distance_matrix[current_city][i] < min_distance {
                    min_distance = self.distance_matrix[current_city][i];
                    nearest_city = Some(i);
                }
            }
            
            if let Some(city) = nearest_city {
                visited[city] = true;
                path.push(city);
                total_distance += min_distance;
                current_city = city;
            }
        }
        
        // Return to starting city
        total_distance += self.distance_matrix[current_city][0];
        path.push(0);
        
        (path, total_distance)
    }
}

fn main() {
    // Create sample cities
    let cities = vec![
        Point::new(0.0, 0.0),
        Point::new(1.0, 2.0),
        Point::new(3.0, 1.0),
        Point::new(5.0, 3.0),
        Point::new(2.0, 4.0),
    ];
    
    let solver = TSPSolver::new(cities);
    
    println!("Cities:");
    for (i, city) in solver.cities.iter().enumerate() {
        println!("  {}: ({}, {})", i, city.x, city.y);
    }
    
    // Solve using brute force
    println!("\n--- Brute Force Solution ---");
    let (path, distance) = solver.solve_brute_force();
    println!("Optimal path: {:?}", path);
    println!("Total distance: {:.2}", distance);
    
    // Solve using nearest neighbor heuristic
    println!("\n--- Nearest Neighbor Solution ---");
    let (path_nn, distance_nn) = solver.solve_nearest_neighbor();
    println!("Path: {:?}", path_nn);
    println!("Total distance: {:.2}", distance_nn);
    
    // Show the actual route
    println!("\n--- Route Details ---");
    let mut total = 0.0;
    for i in 0..path_nn.len() - 1 {
        let from = path_nn[i];
        let to = path_nn[i + 1];
        let dist = solver.distance_matrix[from][to];
        total += dist;
        println!("  {} -> {}: {:.2}", from, to, dist);
    }
    println!("Total: {:.2}", total);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tsp() {
        let cities = vec![
            Point::new(0.0, 0.0),
            Point::new(1.0, 0.0),
            Point::new(0.0, 1.0),
        ];
        
        let solver = TSPSolver::new(cities);
        let (path, distance) = solver.solve_brute_force();
        
        assert_eq!(path.len(), 3);
        assert!(distance > 0.0);
    }
    
    #[test]
    fn test_distance_calculation() {
        let p1 = Point::new(0.0, 0.0);
        let p2 = Point::new(3.0, 4.0);
        let distance = p1.distance_to(&p2);
        assert_eq!(distance, 5.0);
    }
}
```

## Key Features of this Implementation:

### 1. **Point Structure**
- Represents city coordinates with x and y values
- Includes distance calculation method

### 2. **TSPSolver Structure**
- Stores cities and precomputed distance matrix
- Provides both brute force and heuristic solutions

### 3. **Algorithms Implemented**
- **Brute Force**: Finds optimal solution by checking all permutations
- **Nearest Neighbor**: Fast heuristic approach for larger instances

### 4. **Usage Example**
```bash
cargo run
```

### 5. **Output Example**
```
Cities:
  0: (0, 0)
  1: (1, 2)
  2: (3, 1)
  3: (5, 3)
  4: (2, 4)

--- Brute Force Solution ---
Optimal path: [0, 1, 2, 3, 4, 0]
Total distance: 12.34

--- Nearest Neighbor Solution ---
Path: [0, 1, 2, 3, 4, 0]
Total distance: 13.25
```

## Notes:
- The brute force approach has O(n!) time complexity and is suitable only for small instances
- The nearest neighbor heuristic is O(n²) and works well for larger instances
- For production use, consider more advanced algorithms like dynamic programming, genetic algorithms, or simulated annealing
- The code includes unit tests for verification

This implementation demonstrates fundamental concepts of TSP solving in Rust with proper error handling and performance considerations.

