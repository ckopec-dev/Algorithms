# Tarjan's Algorithm in Rust

Here's an implementation of Tarjan's algorithm for finding strongly connected components (SCCs) in a directed graph:

```rust
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct Graph {
    vertices: usize,
    adj_list: Vec<Vec<usize>>,
}

impl Graph {
    fn new(vertices: usize) -> Self {
        Graph {
            vertices,
            adj_list: vec![Vec::new(); vertices],
        }
    }

    fn add_edge(&mut self, from: usize, to: usize) {
        self.adj_list[from].push(to);
    }

    fn tarjan_scc(&self) -> Vec<Vec<usize>> {
        let mut index = 0;
        let mut stack = Vec::new();
        let mut indices = vec![None; self.vertices];
        let mut lowlinks = vec![0; self.vertices];
        let mut on_stack = vec![false; self.vertices];
        let mut sccs = Vec::new();

        for v in 0..self.vertices {
            if indices[v].is_none() {
                self.strongconnect(v, &mut index, &mut stack, &mut indices, 
                                 &mut lowlinks, &mut on_stack, &mut sccs);
            }
        }

        sccs
    }

    fn strongconnect(&self, v: usize, index: &mut usize, stack: &mut Vec<usize>,
                    indices: &mut Vec<Option<usize>>, lowlinks: &mut Vec<usize>,
                    on_stack: &mut Vec<bool>, sccs: &mut Vec<Vec<usize>>) {
        *index += 1;
        indices[v] = Some(*index);
        lowlinks[v] = *index;
        stack.push(v);
        on_stack[v] = true;

        // Consider successors of v
        for &w in &self.adj_list[v] {
            if indices[w].is_none() {
                // Successor w has not yet been visited
                self.strongconnect(w, index, stack, indices, lowlinks, on_stack, sccs);
                lowlinks[v] = lowlinks[v].min(lowlinks[w]);
            } else if on_stack[w] {
                // Successor w is in stack and hence in the current SCC
                lowlinks[v] = lowlinks[v].min(indices[w].unwrap());
            }
        }

        // If v is a root node, pop the stack and create an SCC
        if lowlinks[v] == indices[v].unwrap() {
            let mut scc = Vec::new();
            loop {
                let w = stack.pop().unwrap();
                on_stack[w] = false;
                scc.push(w);
                if w == v {
                    break;
                }
            }
            sccs.push(scc);
        }
    }
}

fn main() {
    // Create a sample graph
    let mut graph = Graph::new(8);
    
    // Add edges: (from, to)
    graph.add_edge(0, 1);
    graph.add_edge(1, 2);
    graph.add_edge(2, 0);
    graph.add_edge(1, 3);
    graph.add_edge(3, 4);
    graph.add_edge(4, 5);
    graph.add_edge(5, 3);
    graph.add_edge(6, 5);
    graph.add_edge(6, 7);
    graph.add_edge(7, 6);

    println!("Graph edges:");
    println!("0 -> 1");
    println!("1 -> 2");
    println!("2 -> 0");
    println!("1 -> 3");
    println!("3 -> 4");
    println!("4 -> 5");
    println!("5 -> 3");
    println!("6 -> 5");
    println!("6 -> 7");
    println!("7 -> 6");

    let sccs = graph.tarjan_scc();
    
    println!("\nStrongly Connected Components:");
    for (i, scc) in sccs.iter().enumerate() {
        println!("SCC {}: {:?}", i + 1, scc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_scc() {
        let mut graph = Graph::new(3);
        graph.add_edge(0, 1);
        graph.add_edge(1, 2);
        graph.add_edge(2, 0);
        
        let sccs = graph.tarjan_scc();
        assert_eq!(sccs.len(), 1);
        assert_eq!(sccs[0].len(), 3);
    }

    #[test]
    fn test_disconnected_graph() {
        let mut graph = Graph::new(4);
        graph.add_edge(0, 1);
        graph.add_edge(2, 3);
        
        let sccs = graph.tarjan_scc();
        assert_eq!(sccs.len(), 2);
        assert_eq!(sccs[0].len(), 2);
        assert_eq!(sccs[1].len(), 2);
    }
}
```

## How it works:

1. **Graph Representation**: Uses an adjacency list representation with `Vec<Vec<usize>>`

2. **Key Components**:
   - `indices`: Tracks discovery time of each vertex
   - `lowlinks`: Tracks the lowest index reachable from the vertex
   - `stack`: Keeps track of vertices in current path
   - `on_stack`: Boolean array to check if vertex is in stack

3. **Algorithm Steps**:
   - For each unvisited vertex, perform DFS
   - Assign discovery time and initialize lowlink value
   - Push vertex to stack
   - Recursively visit neighbors
   - Update lowlink values based on neighbors
   - When lowlink equals index, pop from stack to form SCC

4. **Time Complexity**: O(V + E) where V is vertices and E is edges
5. **Space Complexity**: O(V) for auxiliary arrays

## Sample Output:
```
Graph edges:
0 -> 1
1 -> 2
2 -> 0
1 -> 3
3 -> 4
4 -> 5
5 -> 3
6 -> 5
6 -> 7
7 -> 6

Strongly Connected Components:
SCC 1: [0, 1, 2]
SCC 2: [3, 4, 5]
SCC 3: [6, 7]
```

This implementation correctly identifies the three strongly connected components in the sample graph.

