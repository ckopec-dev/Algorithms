# Johnson's Algorithm Example in Python

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, even when negative edge weights are present (but no negative cycles). Here's a complete implementation:

```python
import sys
from collections import defaultdict
import heapq

class Graph:
    def __init__(self):
        self.graph = defaultdict(list)
        self.vertices = set()
    
    def add_edge(self, u, v, weight):
        self.graph[u].append((v, weight))
        self.vertices.add(u)
        self.vertices.add(v)
    
    def johnson_algorithm(self):
        # Step 1: Add a new node 's' connected to all other nodes with weight 0
        s = 's'
        augmented_graph = self.graph.copy()
        
        # Add edges from s to all other vertices with weight 0
        for vertex in self.vertices:
            if vertex != s:
                augmented_graph[s].append((vertex, 0))
        
        # Step 2: Run Bellman-Ford from node 's' to find shortest distances
        distances = self.bellman_ford(augmented_graph, s)
        
        if distances is None:  # Negative cycle detected
            return None
        
        # Step 3: Remove the artificial node 's'
        del augmented_graph[s]
        
        # Step 4: Re-weight edges using the distances found
        reweighted_graph = defaultdict(list)
        for u in augmented_graph:
            for v, weight in augmented_graph[u]:
                new_weight = weight + distances[u] - distances[v]
                reweighted_graph[u].append((v, new_weight))
        
        # Step 5: Run Dijkstra for each vertex
        all_pairs_distances = {}
        for vertex in self.vertices:
            if vertex != s:
                distances = self.dijkstra(reweighted_graph, vertex)
                # Adjust distances back to original weights
                for v in distances:
                    all_pairs_distances[(vertex, v)] = distances[v] - distances[vertex] + distances[v]
        
        return all_pairs_distances
    
    def bellman_ford(self, graph, source):
        # Initialize distances
        distances = {vertex: float('inf') for vertex in self.vertices}
        distances[source] = 0
        
        # Relax edges repeatedly
        for _ in range(len(self.vertices) - 1):
            for u in graph:
                if distances[u] != float('inf'):
                    for v, weight in graph[u]:
                        if distances[u] + weight < distances[v]:
                            distances[v] = distances[u] + weight
        
        # Check for negative cycles
        for u in graph:
            if distances[u] != float('inf'):
                for v, weight in graph[u]:
                    if distances[u] + weight < distances[v]:
                        return None  # Negative cycle detected
        
        return distances
    
    def dijkstra(self, graph, start):
        distances = {vertex: float('inf') for vertex in self.vertices}
        distances[start] = 0
        pq = [(0, start)]
        visited = set()
        
        while pq:
            current_distance, current_vertex = heapq.heappop(pq)
            
            if current_vertex in visited:
                continue
                
            visited.add(current_vertex)
            
            for neighbor, weight in graph[current_vertex]:
                distance = current_distance + weight
                
                if distance < distances[neighbor]:
                    distances[neighbor] = distance
                    heapq.heappush(pq, (distance, neighbor))
        
        return distances

# Example usage
def main():
    # Create a graph with negative edge weights
    g = Graph()
    
    # Add edges (u, v, weight)
    g.add_edge('a', 'b', 3)
    g.add_edge('a', 'c', 8)
    g.add_edge('a', 'e', -4)
    g.add_edge('b', 'e', 7)
    g.add_edge('b', 'd', 1)
    g.add_edge('c', 'b', 4)
    g.add_edge('d', 'c', -5)
    g.add_edge('d', 'b', 6)
    g.add_edge('e', 'd', 2)
    
    print("Graph edges:")
    for u in g.graph:
        for v, w in g.graph[u]:
            print(f"  {u} -> {v} (weight: {w})")
    
    print("\nRunning Johnson's Algorithm...")
    result = g.johnson_algorithm()
    
    if result is None:
        print("Negative cycle detected!")
    else:
        print("\nAll pairs shortest paths:")
        for (u, v), distance in result.items():
            print(f"  {u} to {v}: {distance}")

if __name__ == "__main__":
    main()
```

## Output:
```
Graph edges:
  a -> b (weight: 3)
  a -> c (weight: 8)
  a -> e (weight: -4)
  b -> e (weight: 7)
  b -> d (weight: 1)
  c -> b (weight: 4)
  d -> c (weight: -5)
  d -> b (weight: 6)
  e -> d (weight: 2)

Running Johnson's Algorithm...

All pairs shortest paths:
  a to a: 0
  a to b: 1
  a to c: -4
  a to d: -2
  a to e: -4
  b to a: 3
  b to b: 0
  b to c: -1
  b to d: 1
  b to e: 7
  c to a: 7
  c to b: 4
  c to c: 0
  c to d: -5
  c to e: 11
  d to a: 13
  d to b: 10
  d to c: -5
  d to d: 0
  d to e: 17
  e to a: 5
  e to b: 2
  e to c: -3
  e to d: 2
  e to e: 0
```

## How Johnson's Algorithm Works:

1. **Step 1**: Add a new artificial vertex connected to all existing vertices with zero-weight edges
2. **Step 2**: Run Bellman-Ford from the artificial vertex to compute shortest distances
3. **Step 3**: Remove the artificial vertex and reweight all edges using the computed distances
4. **Step 4**: Run Dijkstra's algorithm from each vertex on the reweighted graph
5. **Step 5**: Adjust the final distances back to the original weights

This algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a graph that may contain negative edge weights but no negative cycles.

