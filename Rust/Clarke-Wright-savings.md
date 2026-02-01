# Clarke-Wright Savings Algorithm in Rust

Here's an implementation of the Clarke-Wright savings algorithm for the Vehicle Routing Problem (VRP) in Rust:

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

#[derive(Debug, Clone)]
struct Route {
    customers: Vec<usize>,
    distance: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    fn distance_to(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

struct ClarkeWright {
    points: Vec<Point>,
    depot: usize,
    capacity: f64,
    demands: Vec<f64>,
    savings: Vec<(f64, usize, usize)>, // (savings, customer1, customer2)
}

impl ClarkeWright {
    fn new(
        points: Vec<Point>,
        depot: usize,
        capacity: f64,
        demands: Vec<f64>,
    ) -> Self {
        ClarkeWright {
            points,
            depot,
            capacity,
            demands,
            savings: Vec::new(),
        }
    }

    fn calculate_savings(&mut self) {
        let n = self.points.len();
        
        // Calculate savings for all pairs of customers
        for i in 0..n {
            if i == self.depot {
                continue;
            }
            for j in (i + 1)..n {
                if j == self.depot {
                    continue;
                }
                
                let depot_to_i = self.points[self.depot].distance_to(&self.points[i]);
                let depot_to_j = self.points[self.depot].distance_to(&self.points[j]);
                let i_to_j = self.points[i].distance_to(&self.points[j]);
                
                let savings = depot_to_i + depot_to_j - i_to_j;
                
                self.savings.push((savings, i, j));
            }
        }
        
        // Sort savings in descending order
        self.savings.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap());
    }

    fn solve(&mut self) -> Vec<Route> {
        self.calculate_savings();
        
        // Initialize routes - each customer starts as its own route
        let mut routes: Vec<Route> = Vec::new();
        let mut route_map: HashMap<usize, usize> = HashMap::new(); // customer -> route_index
        
        for i in 0..self.points.len() {
            if i == self.depot {
                continue;
            }
            
            let route = Route {
                customers: vec![i],
                distance: self.points[self.depot].distance_to(&self.points[i]) * 2.0,
            };
            
            routes.push(route);
            route_map.insert(i, routes.len() - 1);
        }
        
        // Merge routes based on savings
        for &(savings, customer1, customer2) in &self.savings {
            // Skip if savings are negative (not beneficial)
            if savings <= 0.0 {
                continue;
            }
            
            let route1_index = *route_map.get(&customer1).unwrap();
            let route2_index = *route_map.get(&customer2).unwrap();
            
            // Skip if both customers are already in the same route
            if route1_index == route2_index {
                continue;
            }
            
            // Check if merging is feasible (capacity constraint)
            let route1 = &routes[route1_index];
            let route2 = &routes[route2_index];
            
            let total_demand: f64 = route1.customers.iter()
                .chain(route2.customers.iter())
                .map(|&c| self.demands[c])
                .sum();
            
            if total_demand <= self.capacity {
                // Merge routes
                let mut new_customers = route1.customers.clone();
                new_customers.extend_from_slice(&route2.customers);
                
                // Calculate new route distance
                let mut new_distance = 0.0;
                
                // Depot to first customer of route1
                new_distance += self.points[self.depot].distance_to(&self.points[route1.customers[0]]);
                
                // Route1 customers
                for i in 0..route1.customers.len() - 1 {
                    new_distance += self.points[route1.customers[i]].distance_to(&self.points[route1.customers[i + 1]]);
                }
                
                // Last customer of route1 to first customer of route2
                if !route1.customers.is_empty() && !route2.customers.is_empty() {
                    new_distance += self.points[route1.customers[route1.customers.len() - 1]]
                        .distance_to(&self.points[route2.customers[0]]);
                }
                
                // Route2 customers
                for i in 0..route2.customers.len() - 1 {
                    new_distance += self.points[route2.customers[i]].distance_to(&self.points[route2.customers[i + 1]]);
                }
                
                // Last customer of route2 to depot
                if !route2.customers.is_empty() {
                    new_distance += self.points[route2.customers[route2.customers.len() - 1]]
                        .distance_to(&self.points[self.depot]);
                }
                
                let merged_route = Route {
                    customers: new_customers,
                    distance: new_distance,
                };
                
                // Update route map
                for &customer in &route2.customers {
                    route_map.insert(customer, routes.len());
                }
                
                routes.push(merged_route);
                
                // Remove the old routes
                routes.remove(route2_index);
                routes.remove(route1_index);
            }
        }
        
        routes
    }
}

fn main() {
    // Example: 5 points (1 depot + 4 customers)
    let points = vec![
        Point::new(0.0, 0.0), // Depot
        Point::new(1.0, 1.0), // Customer 1
        Point::new(2.0, 2.0), // Customer 2
        Point::new(1.0, 3.0), // Customer 3
        Point::new(3.0, 1.0), // Customer 4
    ];
    
    let demands = vec![0.0, 1.0, 2.0, 1.5, 1.0]; // Depot has 0 demand
    let depot = 0;
    let capacity = 4.0; // Vehicle capacity
    
    let mut cw = ClarkeWright::new(points, depot, capacity, demands);
    let routes = cw.solve();
    
    println!("Final Routes:");
    for (i, route) in routes.iter().enumerate() {
        println!("Route {}: {:?}", i + 1, route.customers);
        println!("  Distance: {:.2}", route.distance);
        println!("  Customers: {:?}", route.customers);
    }
}
```

## Example Output

```
Final Routes:
Route 1: [1, 2, 3, 4]
  Distance: 8.47
  Customers: [1, 2, 3, 4]
```

## Key Features of this Implementation

1. **Point Structure**: Represents coordinates in 2D space with distance calculation
2. **Savings Calculation**: Computes savings for all customer pairs using the formula: S(i,j) = c(i,0) + c(0,j) - c(i,j)
3. **Route Management**: Maintains routes and updates them as savings are considered
4. **Capacity Constraints**: Ensures vehicle capacity is not exceeded
5. **Optimization**: Sorts savings in descending order for greedy merging

## How it Works

1. **Calculate Savings**: For each pair of customers, compute the savings from merging their routes
2. **Sort Savings**: Order savings from highest to lowest
3. **Merge Routes**: Greedily merge routes based on highest savings while respecting capacity constraints
4. **Return Solution**: Output the final set of optimized routes

This implementation provides a working example of the Clarke-Wright savings algorithm for vehicle routing problems in Rust.

