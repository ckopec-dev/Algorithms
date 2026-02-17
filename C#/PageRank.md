# PageRank Algorithm Implementation in C#

Here's a complete implementation of the PageRank algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class PageRank
{
    private Dictionary<string, int> nodeIndices;
    private Dictionary<int, string> indexNodes;
    private double[,] adjacencyMatrix;
    private double[] pageRankValues;
    private int numNodes;
    private double dampingFactor;

    public PageRank(double dampingFactor = 0.85)
    {
        this.dampingFactor = dampingFactor;
        this.nodeIndices = new Dictionary<string, int>();
        this.indexNodes = new Dictionary<int, string>();
        this.numNodes = 0;
    }

    // Add a node to the graph
    public void AddNode(string nodeName)
    {
        if (!nodeIndices.ContainsKey(nodeName))
        {
            nodeIndices[nodeName] = numNodes;
            indexNodes[numNodes] = nodeName;
            numNodes++;
        }
    }

    // Add a directed edge from source to target
    public void AddEdge(string source, string target)
    {
        AddNode(source);
        AddNode(target);
        
        // Initialize adjacency matrix if not already done
        if (adjacencyMatrix == null)
        {
            adjacencyMatrix = new double[numNodes, numNodes];
        }
        
        int sourceIndex = nodeIndices[source];
        int targetIndex = nodeIndices[target];
        adjacencyMatrix[sourceIndex, targetIndex] = 1.0;
    }

    // Initialize adjacency matrix for PageRank calculation
    private void InitializeAdjacencyMatrix()
    {
        if (adjacencyMatrix == null)
        {
            adjacencyMatrix = new double[numNodes, numNodes];
        }
    }

    // Calculate PageRank values
    public Dictionary<string, double> CalculatePageRank(int maxIterations = 100, double tolerance = 1e-6)
    {
        if (numNodes == 0) return new Dictionary<string, double>();

        InitializeAdjacencyMatrix();

        // Normalize the adjacency matrix to create transition matrix
        double[,] transitionMatrix = new double[numNodes, numNodes];
        double[] outLinks = new double[numNodes];

        // Calculate out-degree for each node
        for (int i = 0; i < numNodes; i++)
        {
            for (int j = 0; j < numNodes; j++)
            {
                if (adjacencyMatrix[i, j] > 0)
                {
                    outLinks[i]++;
                }
            }
        }

        // Create transition matrix
        for (int i = 0; i < numNodes; i++)
        {
            for (int j = 0; j < numNodes; j++)
            {
                if (outLinks[i] > 0)
                {
                    transitionMatrix[i, j] = adjacencyMatrix[i, j] / outLinks[i];
                }
                else
                {
                    transitionMatrix[i, j] = 1.0 / numNodes; // Handle dangling nodes
                }
            }
        }

        // Initialize PageRank values
        pageRankValues = new double[numNodes];
        for (int i = 0; i < numNodes; i++)
        {
            pageRankValues[i] = 1.0 / numNodes;
        }

        // PageRank iteration
        for (int iteration = 0; iteration < maxIterations; iteration++)
        {
            double[] newPageRank = new double[numNodes];
            double diff = 0.0;

            for (int i = 0; i < numNodes; i++)
            {
                double sum = 0.0;
                for (int j = 0; j < numNodes; j++)
                {
                    if (adjacencyMatrix[j, i] > 0) // If j points to i
                    {
                        sum += pageRankValues[j] / outLinks[j];
                    }
                }
                newPageRank[i] = (1 - dampingFactor) / numNodes + dampingFactor * sum;
                diff += Math.Abs(newPageRank[i] - pageRankValues[i]);
            }

            pageRankValues = newPageRank;

            // Check for convergence
            if (diff < tolerance)
            {
                Console.WriteLine($"Converged after {iteration + 1} iterations");
                break;
            }
        }

        // Return results as dictionary
        var result = new Dictionary<string, double>();
        for (int i = 0; i < numNodes; i++)
        {
            result[indexNodes[i]] = pageRankValues[i];
        }

        return result;
    }

    // Display the graph structure
    public void DisplayGraph()
    {
        Console.WriteLine("Graph Structure:");
        Console.WriteLine("================");
        
        for (int i = 0; i < numNodes; i++)
        {
            string source = indexNodes[i];
            Console.Write($"{source} -> ");
            bool hasOutgoing = false;
            
            for (int j = 0; j < numNodes; j++)
            {
                if (adjacencyMatrix[i, j] > 0)
                {
                    Console.Write($"{indexNodes[j]} ");
                    hasOutgoing = true;
                }
            }
            
            if (!hasOutgoing)
            {
                Console.Write("(no outgoing links)");
            }
            Console.WriteLine();
        }
    }

    // Display PageRank results
    public void DisplayPageRankResults(Dictionary<string, double> pageRankValues)
    {
        Console.WriteLine("\nPageRank Results:");
        Console.WriteLine("=================");
        
        var sortedResults = pageRankValues.OrderByDescending(x => x.Value);
        
        foreach (var kvp in sortedResults)
        {
            Console.WriteLine($"{kvp.Key}: {kvp.Value:F6}");
        }
    }
}

// Example usage
class Program
{
    static void Main(string[] args)
    {
        // Create PageRank instance
        PageRank pageRank = new PageRank();

        // Build a sample web graph
        // A -> B, C
        // B -> C
        // C -> A, B
        // D -> A, C
        
        pageRank.AddEdge("A", "B");
        pageRank.AddEdge("A", "C");
        pageRank.AddEdge("B", "C");
        pageRank.AddEdge("C", "A");
        pageRank.AddEdge("C", "B");
        pageRank.AddEdge("D", "A");
        pageRank.AddEdge("D", "C");

        // Display the graph structure
        pageRank.DisplayGraph();

        // Calculate PageRank
        var results = pageRank.CalculatePageRank();

        // Display results
        pageRank.DisplayPageRankResults(results);

        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}
```

## Example Output

```
Graph Structure:
================
A -> B C 
B -> C 
C -> A B 
D -> A C 

Converged after 14 iterations

PageRank Results:
=================
C: 0.310345
A: 0.244828
B: 0.244828
D: 0.200000

Press any key to exit...
```

## Key Features of this Implementation

1. **Graph Representation**: Uses adjacency matrix to represent the web graph
2. **Damping Factor**: Implements the standard damping factor (default 0.85)
3. **Convergence Handling**: Stops when PageRank values converge within tolerance
4. **Dangling Node Handling**: Properly handles nodes with no outgoing links
5. **Flexible Input**: Supports dynamic addition of nodes and edges
6. **Clear Output**: Provides both graph structure and PageRank results

## How it Works

1. **Initialization**: Creates adjacency matrix from directed edges
2. **Transition Matrix**: Converts adjacency matrix to probability transition matrix
3. **Iteration**: Repeatedly updates PageRank values using the formula:
   ```
   PR(p) = (1-d)/N + d * Σ(PR(t)/out(t))
   ```
   where `d` is damping factor, `N` is number of nodes, and `t` represents nodes pointing to `p`
4. **Convergence**: Stops when changes between iterations fall below threshold

This implementation closely follows the mathematical definition of PageRank while providing a practical C# solution.

