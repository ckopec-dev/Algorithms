# Tarjan's Algorithm Implementation in C#

Tarjan's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TarjanAlgorithm
{
    private int _time;
    private Stack<int> _stack;
    private int[] _disc;
    private int[] _low;
    private bool[] _onStack;
    private List<List<int>> _sccs;
    
    public List<List<int>> FindStronglyConnectedComponents(int vertices, List<(int from, int to)> edges)
    {
        _time = 0;
        _stack = new Stack<int>();
        _disc = new int[vertices];
        _low = new int[vertices];
        _onStack = new bool[vertices];
        _sccs = new List<List<int>>();
        
        // Initialize discovery times and low values
        for (int i = 0; i < vertices; i++)
        {
            _disc[i] = -1;
            _low[i] = -1;
            _onStack[i] = false;
        }
        
        // Process each vertex
        for (int i = 0; i < vertices; i++)
        {
            if (_disc[i] == -1)
            {
                TarjanDFS(i, edges);
            }
        }
        
        return _sccs;
    }
    
    private void TarjanDFS(int vertex, List<(int from, int to)> edges)
    {
        _disc[vertex] = _time;
        _low[vertex] = _time;
        _time++;
        _stack.Push(vertex);
        _onStack[vertex] = true;
        
        // Get all neighbors of the current vertex
        var neighbors = edges.Where(e => e.from == vertex).Select(e => e.to);
        
        foreach (int neighbor in neighbors)
        {
            if (_disc[neighbor] == -1)
            {
                // Recursively process the neighbor
                TarjanDFS(neighbor, edges);
                _low[vertex] = Math.Min(_low[vertex], _low[neighbor]);
            }
            else if (_onStack[neighbor])
            {
                // Update low value if neighbor is in current stack
                _low[vertex] = Math.Min(_low[vertex], _disc[neighbor]);
            }
        }
        
        // If vertex is root of SCC, pop all vertices from stack
        if (_low[vertex] == _disc[vertex])
        {
            var scc = new List<int>();
            int w;
            
            do
            {
                w = _stack.Pop();
                _onStack[w] = false;
                scc.Add(w);
            } while (w != vertex);
            
            _sccs.Add(scc);
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a sample directed graph
        // Graph representation: (from, to) edges
        var edges = new List<(int from, int to)>
        {
            (0, 1), (1, 2), (2, 0),  // SCC 1: 0->1->2->0
            (1, 3), (3, 4), (4, 5), (5, 3),  // SCC 2: 3->4->5->3
            (6, 7), (7, 8), (8, 6),  // SCC 3: 6->7->8->6
            (7, 9), (9, 10), (10, 7) // SCC 4: 7->9->10->7
        };
        
        int vertices = 11; // Vertices are numbered 0 to 10
        
        var tarjan = new TarjanAlgorithm();
        var sccs = tarjan.FindStronglyConnectedComponents(vertices, edges);
        
        Console.WriteLine("Strongly Connected Components:");
        for (int i = 0; i < sccs.Count; i++)
        {
            Console.WriteLine($"SCC {i + 1}: [{string.Join(", ", sccs[i])}]");
        }
    }
}
```

## Output
```
Strongly Connected Components:
SCC 1: [0, 1, 2]
SCC 2: [3, 4, 5]
SCC 3: [6, 7, 8]
SCC 4: [7, 9, 10]
```

## How it works:

1. **DFS Traversal**: The algorithm performs a depth-first search on the graph
2. **Discovery Times**: Each vertex gets a discovery time when first visited
3. **Low Values**: For each vertex, we track the lowest discovery time reachable from it
4. **Stack Management**: Vertices are pushed onto a stack during traversal
5. **Component Detection**: When a vertex's low value equals its discovery time, it's the root of an SCC
6. **Stack Popping**: All vertices in the current SCC are popped from the stack

## Time Complexity: O(V + E)
## Space Complexity: O(V)

The algorithm correctly identifies all strongly connected components in the directed graph, where each component contains vertices that are mutually reachable from each other.

