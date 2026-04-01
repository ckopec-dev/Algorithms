# Christofides Algorithm in Rust

Here's a complete implementation of the Christofides algorithm for solving the Traveling Salesman Problem (TSP) in Rust:

```rust
use std::collections::BinaryHeap;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
    index: usize,
}

impl Point {
    fn new(x: f64, y: f64, index: usize) -> Self {
        Point { x, y, index }
    }
    
    fn distance(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

#[derive(Debug, Clone)]
struct Edge {
    from: usize,
    to: usize,
    weight: f64,
}

impl Edge {
    fn new(from: usize, to: usize, weight: f64) -> Self {
        Edge { from, to, weight }
    }
}

impl Ord for Edge {
    fn cmp(&self, other: &Self) -> Ordering {
        other.weight.partial_cmp(&self.weight).unwrap()
    }
}

impl PartialOrd for Edge {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Edge {
    fn eq(&self, other: &Self) -> bool {
        self.weight == other.weight
    }
}

impl Eq for Edge {}

struct Graph {
    vertices: Vec<Point>,
    adjacency_list: Vec<Vec<(usize, f64)>>,
    num_vertices: usize,
}

impl Graph {
    fn new(points: Vec<Point>) -> Self {
        let num_vertices = points.len();
        let mut adjacency_list = vec![Vec::new(); num_vertices];
        
        // Create adjacency list with all edges
        for i in 0..num_vertices {
            for j in 0..num_vertices {
                if i != j {
                    let distance = points[i].distance(&points[j]);
                    adjacency_list[i].push((j, distance));
                }
            }
        }
        
        Graph {
            vertices: points,
            adjacency_list,
            num_vertices,
        }
    }
    
    fn get_vertex(&self, index: usize) -> &Point {
        &self.vertices[index]
    }
    
    fn get_adjacent(&self, index: usize) -> &[(usize, f64)] {
        &self.adjacency_list[index]
    }
    
    fn get_num_vertices(&self) -> usize {
        self.num_vertices
    }
}

fn find_mst(graph: &Graph) -> Vec<Edge> {
    let mut edges = Vec::new();
    let mut visited = vec![false; graph.get_num_vertices()];
    let mut edge_heap = BinaryHeap::new();
    
    // Start with vertex 0
    visited[0] = true;
    
    // Add all edges from vertex 0 to heap
    for &(to, weight) in graph.get_adjacent(0) {
        edge_heap.push(Edge::new(0, to, weight));
    }
    
    while let Some(edge) = edge_heap.pop() {
        if visited[edge.to] {
            continue;
        }
        
        visited[edge.to] = true;
        edges.push(edge.clone());
        
        // Add new edges to heap
        for &(to, weight) in graph.get_adjacent(edge.to) {
            if !visited[to] {
                edge_heap.push(Edge::new(edge.to, to, weight));
            }
        }
    }
    
    edges
}

fn find_odd_degree_vertices(mst_edges: &Vec<Edge>, num_vertices: usize) -> Vec<usize> {
    let mut degree = vec![0; num_vertices];
    
    for edge in mst_edges {
        degree[edge.from] += 1;
        degree[edge.to] += 1;
    }
    
    let mut odd_vertices = Vec::new();
    for i in 0..num_vertices {
        if degree[i] % 2 == 1 {
            odd_vertices.push(i);
        }
    }
    
    odd_vertices
}

fn find_minimum_matching(odd_vertices: &Vec<usize>, graph: &Graph) -> Vec<Edge> {
    let mut matching = Vec::new();
    let mut visited = vec![false; graph.get_num_vertices()];
    
    // Simple greedy approach for minimum weight perfect matching
    for i in 0..odd_vertices.len() {
        if visited[odd_vertices[i]] {
            continue;
        }
        
        let mut min_weight = f64::MAX;
        let mut best_j = 0;
        
        for j in (i + 1)..odd_vertices.len() {
            if visited[odd_vertices[j]] {
                continue;
            }
            
            let weight = graph.get_vertex(odd_vertices[i]).distance(graph.get_vertex(odd_vertices[j]));
            if weight < min_weight {
                min_weight = weight;
                best_j = j;
            }
        }
        
        if min_weight < f64::MAX {
            visited[odd_vertices[i]] = true;
            visited[odd_vertices[best_j]] = true;
            matching.push(Edge::new(odd_vertices[i], odd_vertices[best_j], min_weight));
        }
    }
    
    matching
}

fn find_eulerian_circuit(mst_edges: &Vec<Edge>, matching_edges: &Vec<Edge>) -> Vec<usize> {
    // Create combined graph (MST + matching)
    let mut adjacency_list = vec![Vec::new(); 100]; // Assuming max 100 vertices
    
    // Add MST edges
    for edge in mst_edges {
        adjacency_list[edge.from].push(edge.to);
        adjacency_list[edge.to].push(edge.from);
    }
    
    // Add matching edges
    for edge in matching_edges {
        adjacency_list[edge.from].push(edge.to);
        adjacency_list[edge.to].push(edge.from);
    }
    
    // Find Eulerian circuit using DFS
    let mut circuit = Vec::new();
    let mut stack = vec![0]; // Start from vertex 0
    
    while let Some(current) = stack.pop() {
        if !adjacency_list[current].is_empty() {
            let next = adjacency_list[current].pop().unwrap();
            stack.push(current);
            stack.push(next);
        } else {
            circuit.push(current);
        }
    }
    
    circuit.reverse();
    circuit
}

fn find_hamiltonian_path(circuit: &Vec<usize>) -> Vec<usize> {
    let mut visited = vec![false; 100]; // Assuming max 100 vertices
    let mut path = Vec::new();
    
    for vertex in circuit {
        if !visited[*vertex] {
            visited[*vertex] = true;
            path.push(*vertex);
        }
    }
    
    path
}

fn christofides_tsp(graph: &Graph) -> Vec<usize> {
    // Step 1: Find Minimum Spanning Tree
    let mst_edges = find_mst(graph);
    
    // Step 2: Find vertices with odd degree
    let odd_vertices = find_odd_degree_vertices(&mst_edges, graph.get_num_vertices());
    
    // Step 3: Find minimum weight perfect matching on odd degree vertices
    let matching_edges = find_minimum_matching(&odd_vertices, graph);
    
    // Step 4: Combine MST and matching to form Eulerian graph
    let eulerian_circuit = find_eulerian_circuit(&mst_edges, &matching_edges);
    
    // Step 5: Find Hamiltonian path (remove repeated vertices)
    let hamiltonian_path = find_hamiltonian_path(&eulerian_circuit);
    
    hamiltonian_path
}

fn calculate_total_distance(path: &Vec<usize>, graph: &Graph) -> f64 {
    let mut total = 0.0;
    
    for i in 0..path.len() - 1 {
        let from = graph.get_vertex(path[i]);
        let to = graph.get_vertex(path[i + 1]);
        total += from.distance(to);
    }
    
    // Add distance from last to first (return to start)
    if !path.is_empty() {
        let first = graph.get_vertex(path[0]);
        let last = graph.get_vertex(path[path.len() - 1]);
        total += last.distance(first);
    }
    
    total
}

fn main() {
    // Create sample points
    let points = vec![
        Point::new(0.0, 0.0, 0),
        Point::new(1.0, 2.0, 1),
        Point::new(3.0, 1.0, 2),
        Point::new(2.0, 3.0, 3),
        Point::new(4.0, 4.0, 4),
    ];
    
    let graph = Graph::new(points);
    let path = christofides_tsp(&graph);
    let distance = calculate_total_distance(&path, &graph);
    
    println!("Christofides TSP Solution:");
    println!("Path: {:?}", path);
    println!("Total Distance: {:.2}", distance);
    
    // Print the path with coordinates
    println!("\nPath with coordinates:");
    for &vertex_index in &path {
        let point = graph.get_vertex(vertex_index);
        println!("({}, {})", point.x, point.y);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_distance_calculation() {
        let p1 = Point::new(0.0, 0.0, 0);
        let p2 = Point::new(3.0, 4.0, 1);
        assert_eq!(p1.distance(&p2), 5.0);
    }
    
    #[test]
    fn test_point_creation() {
        let point = Point::new(1.0, 2.0, 3);
        assert_eq!(point.x, 1.0);
        assert_eq!(point.y, 2.0);
        assert_eq!(point.index, 3);
    }
}
```

## Algorithm Explanation

The Christofides algorithm is a heuristic for solving the Traveling Salesman Problem with an approximation ratio of 1.5. Here's how it works:

### Steps:
1. **Minimum Spanning Tree (MST)**: Find the MST of the complete graph
2. **Odd Degree Vertices**: Identify vertices with odd degree in the MST
3. **Minimum Weight Matching**: Find a minimum weight perfect matching on odd-degree vertices
4. **Eulerian Circuit**: Combine MST and matching to create an Eulerian graph
5. **Hamiltonian Path**: Find a Hamiltonian path by removing repeated vertices

### Key Components:
- **Point structure**: Represents 2D coordinates with index
- **Edge structure**: Represents weighted edges between vertices
- **Graph structure**: Maintains adjacency list representation
- **MST algorithm**: Uses Prim's algorithm with binary heap
- **Matching**: Simple greedy approach for minimum weight perfect matching

### Time Complexity:
- O(n² log n) for the complete algorithm
- Dominated by MST construction and sorting operations

### Space Complexity:
- O(n²) for storing the adjacency matrix

This implementation provides a working example that can be extended with more sophisticated matching algorithms for better performance on larger datasets.

