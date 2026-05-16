# PageRank Algorithm Implementation in Python

Here's a complete implementation of the PageRank algorithm using Python:

```python
import numpy as np
import matplotlib.pyplot as plt

class PageRank:
    def __init__(self, damping_factor=0.85):
        self.damping_factor = damping_factor
        self.graph = {}
        self.nodes = []
        
    def add_node(self, node):
        """Add a node to the graph"""
        if node not in self.graph:
            self.graph[node] = []
            self.nodes.append(node)
    
    def add_edge(self, from_node, to_node):
        """Add a directed edge from from_node to to_node"""
        self.add_node(from_node)
        self.add_node(to_node)
        self.graph[from_node].append(to_node)
    
    def build_transition_matrix(self):
        """Build the transition matrix for PageRank"""
        n = len(self.nodes)
        # Create mapping from node names to indices
        node_to_index = {node: i for i, node in enumerate(self.nodes)}
        
        # Initialize transition matrix
        M = np.zeros((n, n))
        
        # Fill the transition matrix
        for from_node, to_nodes in self.graph.items():
            from_idx = node_to_index[from_node]
            num_out_links = len(to_nodes)
            
            if num_out_links > 0:
                # Distribute probability equally among outgoing links
                for to_node in to_nodes:
                    to_idx = node_to_index[to_node]
                    M[to_idx][from_idx] = 1.0 / num_out_links
        
        return M, node_to_index
    
    def calculate_pagerank(self, max_iterations=100, tolerance=1e-6):
        """Calculate PageRank scores"""
        if not self.graph:
            return {}
        
        # Build transition matrix
        M, node_to_index = self.build_transition_matrix()
        n = len(self.nodes)
        
        # Initialize PageRank scores
        pr = np.ones(n) / n
        
        # PageRank calculation
        for iteration in range(max_iterations):
            # Apply the PageRank formula
            new_pr = (1 - self.damping_factor) / n + self.damping_factor * np.dot(M, pr)
            
            # Check for convergence
            if np.linalg.norm(new_pr - pr, 1) < tolerance:
                print(f"Converged after {iteration + 1} iterations")
                break
                
            pr = new_pr
        
        # Return results as dictionary
        return {self.nodes[i]: pr[i] for i in range(n)}
    
    def print_graph(self):
        """Print the graph structure"""
        print("Graph structure:")
        for node, edges in self.graph.items():
            print(f"  {node} -> {edges}")

# Example usage
def main():
    # Create a sample web graph
    pagerank = PageRank(damping_factor=0.85)
    
    # Add edges to represent web links
    # Example: A links to B and C, B links to C, C links to A
    pagerank.add_edge('A', 'B')
    pagerank.add_edge('A', 'C')
    pagerank.add_edge('B', 'C')
    pagerank.add_edge('C', 'A')
    pagerank.add_edge('D', 'A')
    pagerank.add_edge('D', 'B')
    
    # Print graph structure
    pagerank.print_graph()
    
    # Calculate PageRank scores
    scores = pagerank.calculate_pagerank()
    
    # Display results
    print("\nPageRank Scores:")
    print("-" * 20)
    for node, score in sorted(scores.items(), key=lambda x: x[1], reverse=True):
        print(f"{node}: {score:.4f}")
    
    # Visualize results
    nodes = list(scores.keys())
    values = list(scores.values())
    
    plt.figure(figsize=(10, 6))
    bars = plt.bar(nodes, values, color='skyblue')
    plt.xlabel('Nodes')
    plt.ylabel('PageRank Score')
    plt.title('PageRank Algorithm Results')
    plt.ylim(0, max(values) * 1.1)
    
    # Add value labels on bars
    for bar, value in zip(bars, values):
        plt.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.001,
                f'{value:.3f}', ha='center', va='bottom')
    
    plt.tight_layout()
    plt.show()

# Alternative implementation using networkx (more advanced)
def example_with_networkx():
    """Example using networkx library for comparison"""
    try:
        import networkx as nx
        
        # Create a directed graph
        G = nx.DiGraph()
        
        # Add edges
        edges = [('A', 'B'), ('A', 'C'), ('B', 'C'), ('C', 'A'), ('D', 'A'), ('D', 'B')]
        G.add_edges_from(edges)
        
        # Calculate PageRank using networkx
        pagerank_scores = nx.pagerank(G, alpha=0.85)
        
        print("\nUsing NetworkX:")
        print("-" * 20)
        for node, score in sorted(pagerank_scores.items(), key=lambda x: x[1], reverse=True):
            print(f"{node}: {score:.4f}")
            
    except ImportError:
        print("NetworkX not installed. Install it with: pip install networkx")

if __name__ == "__main__":
    main()
    example_with_networkx()
```

## Key Features of This Implementation:

### 1. **Core Algorithm Components**
- **Transition Matrix**: Converts the web graph into a mathematical matrix
- **Damping Factor**: Represents the probability of following links (default 0.85)
- **Convergence Check**: Stops when PageRank scores stabilize

### 2. **Graph Representation**
- Uses adjacency list structure for efficient storage
- Supports directed edges between nodes
- Handles isolated nodes gracefully

### 3. **Mathematical Formula**
The PageRank algorithm uses the formula:
```
PR(A) = (1-d)/N + d * Σ(PR(Ti)/C(Ti))
```
Where:
- `d` = damping factor (0.85)
- `N` = total number of pages
- `PR(Ti)` = PageRank of page Ti
- `C(Ti)` = number of outbound links from page Ti

### 4. **Usage Example**
The code creates a sample web graph with nodes A, B, C, D and calculates their PageRank scores, showing which pages are most important based on link structure.

### 5. **Output**
The implementation will display:
- Graph structure
- PageRank scores for each node
- Visual bar chart of results

This implementation provides a solid foundation for understanding how PageRank works and can be extended for more complex scenarios.

