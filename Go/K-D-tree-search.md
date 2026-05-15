# K-D Tree Search Algorithm in Go

Here's a complete implementation of a K-D tree search algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Point represents a point in k-dimensional space
type Point struct {
    Coordinates []float64
    ID          string
}

// KdNode represents a node in the K-D tree
type KdNode struct {
    Point     Point
    Left      *KdNode
    Right     *KdNode
    Dimension int // Current dimension for splitting
}

// KdTree represents the K-D tree structure
type KdTree struct {
    Root *KdNode
    Dim  int // Number of dimensions
}

// NewKdTree creates a new K-D tree
func NewKdTree(dim int) *KdTree {
    return &KdTree{
        Root: nil,
        Dim:  dim,
    }
}

// Insert adds a point to the K-D tree
func (tree *KdTree) Insert(point Point) {
    tree.Root = insertNode(tree.Root, point, 0, tree.Dim)
}

func insertNode(node *KdNode, point Point, depth int, dim int) *KdNode {
    if node == nil {
        return &KdNode{
            Point:     point,
            Left:      nil,
            Right:     nil,
            Dimension: depth % dim,
        }
    }

    // Compare along the current dimension
    currentDim := node.Dimension
    if point.Coordinates[currentDim] < node.Point.Coordinates[currentDim] {
        node.Left = insertNode(node.Left, point, depth+1, dim)
    } else {
        node.Right = insertNode(node.Right, point, depth+1, dim)
    }

    return node
}

// Euclidean distance between two points
func euclideanDistance(p1, p2 Point) float64 {
    sum := 0.0
    for i := 0; i < len(p1.Coordinates); i++ {
        diff := p1.Coordinates[i] - p2.Coordinates[i]
        sum += diff * diff
    }
    return math.Sqrt(sum)
}

// KNNSearch finds the k nearest neighbors to a query point
func (tree *KdTree) KNNSearch(query Point, k int) []Point {
    if tree.Root == nil {
        return []Point{}
    }

    // Use a max heap to keep track of k nearest neighbors
    neighbors := make([]Point, 0, k)
    var bestDistances []float64
    
    // Initialize with infinity
    bestDistances = make([]float64, k)
    for i := range bestDistances {
        bestDistances[i] = math.Inf(1)
    }
    
    knnSearch(tree.Root, query, &neighbors, &bestDistances, k)
    
    return neighbors
}

func knnSearch(node *KdNode, query Point, neighbors *[]Point, bestDistances *[]float64, k int) {
    if node == nil {
        return
    }

    // Calculate distance to current node
    dist := euclideanDistance(query, node.Point)
    
    // If we have fewer than k points, or this point is closer than the furthest
    // in our current k nearest neighbors
    if len(*neighbors) < k || dist < (*bestDistances)[k-1] {
        // Add the point to neighbors
        if len(*neighbors) < k {
            *neighbors = append(*neighbors, node.Point)
            (*bestDistances)[len(*neighbors)-1] = dist
        } else {
            // Replace the farthest point
            maxIdx := 0
            for i, d := range *bestDistances {
                if d > (*bestDistances)[maxIdx] {
                    maxIdx = i
                }
            }
            (*neighbors)[maxIdx] = node.Point
            (*bestDistances)[maxIdx] = dist
        }
        
        // Sort distances in descending order
        for i := 0; i < len(*bestDistances)-1; i++ {
            for j := i + 1; j < len(*bestDistances); j++ {
                if (*bestDistances)[i] < (*bestDistances)[j] {
                    (*bestDistances)[i], (*bestDistances)[j] = (*bestDistances)[j], (*bestDistances)[i]
                    (*neighbors)[i], (*neighbors)[j] = (*neighbors)[j], (*neighbors)[i]
                }
            }
        }
    }

    // Determine which subtree to search first
    currentDim := node.Dimension
    if query.Coordinates[currentDim] < node.Point.Coordinates[currentDim] {
        knnSearch(node.Left, query, neighbors, bestDistances, k)
        // Check if we need to search the other subtree
        if len(*neighbors) < k || math.Abs(query.Coordinates[currentDim]-node.Point.Coordinates[currentDim]) < (*bestDistances)[k-1] {
            knnSearch(node.Right, query, neighbors, bestDistances, k)
        }
    } else {
        knnSearch(node.Right, query, neighbors, bestDistances, k)
        // Check if we need to search the other subtree
        if len(*neighbors) < k || math.Abs(query.Coordinates[currentDim]-node.Point.Coordinates[currentDim]) < (*bestDistances)[k-1] {
            knnSearch(node.Left, query, neighbors, bestDistances, k)
        }
    }
}

// Search finds the closest point to a query point
func (tree *KdTree) Search(query Point) *Point {
    if tree.Root == nil {
        return nil
    }
    
    closest := &Point{}
    minDist := math.Inf(1)
    
    searchNode(tree.Root, query, closest, &minDist)
    
    if minDist == math.Inf(1) {
        return nil
    }
    
    return closest
}

func searchNode(node *KdNode, query Point, closest *Point, minDist *float64) {
    if node == nil {
        return
    }
    
    // Calculate distance to current node
    dist := euclideanDistance(query, node.Point)
    
    // Update closest point if this is closer
    if dist < *minDist {
        *minDist = dist
        closest.Coordinates = make([]float64, len(node.Point.Coordinates))
        copy(closest.Coordinates, node.Point.Coordinates)
        closest.ID = node.Point.ID
    }
    
    // Determine which subtree to search first
    currentDim := node.Dimension
    if query.Coordinates[currentDim] < node.Point.Coordinates[currentDim] {
        searchNode(node.Left, query, closest, minDist)
        // Check if we need to search the other subtree
        if math.Abs(query.Coordinates[currentDim]-node.Point.Coordinates[currentDim]) < *minDist {
            searchNode(node.Right, query, closest, minDist)
        }
    } else {
        searchNode(node.Right, query, closest, minDist)
        // Check if we need to search the other subtree
        if math.Abs(query.Coordinates[currentDim]-node.Point.Coordinates[currentDim]) < *minDist {
            searchNode(node.Left, query, closest, minDist)
        }
    }
}

// PrintTree prints the tree structure (for debugging)
func (tree *KdTree) PrintTree() {
    printNode(tree.Root, 0)
}

func printNode(node *KdNode, depth int) {
    if node == nil {
        return
    }
    
    indent := ""
    for i := 0; i < depth; i++ {
        indent += "  "
    }
    
    fmt.Printf("%sPoint: %v (dim: %d)\n", indent, node.Point.Coordinates, node.Dimension)
    printNode(node.Left, depth+1)
    printNode(node.Right, depth+1)
}

func main() {
    // Create a 2D K-D tree
    tree := NewKdTree(2)
    
    // Insert some points
    points := []Point{
        {Coordinates: []float64{2, 3}, ID: "A"},
        {Coordinates: []float64{5, 4}, ID: "B"},
        {Coordinates: []float64{9, 6}, ID: "C"},
        {Coordinates: []float64{4, 7}, ID: "D"},
        {Coordinates: []float64{8, 1}, ID: "E"},
        {Coordinates: []float64{7, 2}, ID: "F"},
    }
    
    for _, point := range points {
        tree.Insert(point)
    }
    
    fmt.Println("K-D Tree structure:")
    tree.PrintTree()
    
    fmt.Println("\nSearching for the closest point to (4, 5):")
    query := Point{Coordinates: []float64{4, 5}}
    closest := tree.Search(query)
    if closest != nil {
        fmt.Printf("Closest point: %v (ID: %s)\n", closest.Coordinates, closest.ID)
    }
    
    fmt.Println("\nFinding 3 nearest neighbors to (4, 5):")
    neighbors := tree.KNNSearch(query, 3)
    for i, neighbor := range neighbors {
        dist := euclideanDistance(query, neighbor)
        fmt.Printf("Neighbor %d: %v (ID: %s), Distance: %.2f\n", 
                   i+1, neighbor.Coordinates, neighbor.ID, dist)
    }
}
```

## Key Features of this Implementation:

1. **Point Structure**: Represents points in k-dimensional space with coordinates and an ID
2. **KdNode Structure**: Represents nodes in the K-D tree with point data, left/right children, and dimension information
3. **KdTree Structure**: Main tree structure with root node and dimension count
4. **Insert Method**: Builds the tree by recursively inserting points
5. **Search Method**: Finds the closest point to a query point
6. **KNNSearch Method**: Finds k nearest neighbors to a query point
7. **Efficient Traversal**: Uses the dimension-based splitting to optimize search performance

## Time Complexity:
- **Construction**: O(n log n) average case
- **Search**: O(log n) average case, O(n) worst case
- **KNN Search**: O(log n + k) average case

## Usage:
The example demonstrates creating a 2D K-D tree, inserting points, and performing both single nearest neighbor search and k-nearest neighbor searches.

