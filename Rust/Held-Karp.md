# Held-Karp Algorithm Implementation in Rust

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming with bitmasks. Here's a complete implementation:

```rust
use std::collections::HashMap;

fn held_karp(distances: &Vec<Vec<i32>>) -> (i32, Vec<usize>) {
    let n = distances.len();
    
    // dp[mask][i] = minimum cost to visit all cities in mask and end at city i
    let mut dp: HashMap<(u32, usize), i32> = HashMap::new();
    let mut parent: HashMap<(u32, usize), usize> = HashMap::new();
    
    // Base case: starting from city 0
    dp.insert((1u32, 0), 0);
    
    // For all possible subsets of cities
    for mask in 1u32..(1u32 << n) {
        for u in 0..n {
            // If city u is not in the current mask
            if (mask & (1u32 << u)) == 0 {
                continue;
            }
            
            // Try to reach city u from all possible previous cities
            for v in 0..n {
                // If city v is in the mask and is not the same as u
                if (mask & (1u32 << v)) != 0 && u != v {
                    let prev_mask = mask ^ (1u32 << u); // Remove city u from mask
                    let cost = dp.get(&(prev_mask, v)).copied().unwrap_or(i32::MAX);
                    
                    if cost != i32::MAX {
                        let new_cost = cost + distances[v][u];
                        let current_cost = dp.get(&(mask, u)).copied().unwrap_or(i32::MAX);
                        
                        if new_cost < current_cost {
                            dp.insert((mask, u), new_cost);
                            parent.insert((mask, u), v);
                        }
                    }
                }
            }
        }
    }
    
    // Find the minimum cost to return to city 0
    let mut min_cost = i32::MAX;
    let mut last_city = 0;
    let full_mask = (1u32 << n) - 1;
    
    for i in 1..n {
        if let Some(cost) = dp.get(&(full_mask, i)) {
            let total_cost = *cost + distances[i][0];
            if total_cost < min_cost {
                min_cost = total_cost;
                last_city = i;
            }
        }
    }
    
    // Reconstruct the path
    let mut path = vec![0];
    let mut current_mask = full_mask;
    let mut current_city = last_city;
    
    while current_city != 0 {
        path.push(current_city);
        let prev_city = parent[&(current_mask, current_city)];
        current_mask ^= 1u32 << current_city;
        current_city = prev_city;
    }
    path.push(0);
    path.reverse();
    
    (min_cost, path)
}

fn main() {
    // Example distance matrix for 4 cities
    let distances = vec![
        vec![0, 10, 15, 20],
        vec![10, 0, 35, 25],
        vec![15, 35, 0, 30],
        vec![20, 25, 30, 0],
    ];
    
    let (min_cost, path) = held_karp(&distances);
    
    println!("Minimum cost: {}", min_cost);
    println!("Optimal path: {:?}", path);
    
    // Print the path with city names
    println!("Path: ");
    for i in 0..path.len() - 1 {
        print!("{} -> ", path[i]);
    }
    println!("{}", path[path.len() - 1]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_held_karp() {
        let distances = vec![
            vec![0, 10, 15, 20],
            vec![10, 0, 35, 25],
            vec![15, 35, 0, 30],
            vec![20, 25, 30, 0],
        ];
        
        let (cost, path) = held_karp(&distances);
        assert_eq!(cost, 80);
        assert_eq!(path.len(), 5); // Start and end at city 0
    }
}
```

## How it works:

1. **Dynamic Programming Approach**: Uses bitmask to represent subsets of visited cities
2. **State Definition**: `dp[mask][i]` represents the minimum cost to visit all cities in the mask and end at city `i`
3. **Transition**: For each subset, try to reach each city from all possible previous cities
4. **Path Reconstruction**: Uses parent tracking to reconstruct the optimal path
5. **Time Complexity**: O(n²2ⁿ) where n is the number of cities
6. **Space Complexity**: O(n2ⁿ)

## Example Output:
```
Minimum cost: 80
Optimal path: [0, 1, 3, 2, 0]
Path: 
0 -> 1 -> 3 -> 2 -> 0
```

This implementation handles the classic TSP problem and returns both the minimum cost and the optimal path that visits all cities exactly once and returns to the starting city.

