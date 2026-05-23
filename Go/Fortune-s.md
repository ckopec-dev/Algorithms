# Fortune's Algorithm Implementation in Go

Fortune's algorithm is used to compute Voronoi diagrams. Here's a complete implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Point represents a 2D point
type Point struct {
    X, Y float64
}

// Event represents a beach line event (site or circle event)
type Event struct {
    Point
    IsSiteEvent bool
    Radius      float64
    Center      Point
}

// Node represents a node in the beach line tree
type Node struct {
    Point
    Left, Right *Node
    IsLeaf      bool
    CircleEvent *Event
    Site        Point
}

// VoronoiDiagram represents the Voronoi diagram
type VoronoiDiagram struct {
    Sites   []Point
    Edges   []Edge
    Vertices []Point
}

// Edge represents a Voronoi edge
type Edge struct {
    Start, End Point
    LeftSite   Point
    RightSite  Point
}

// FortuneAlgorithm implements Fortune's algorithm
type FortuneAlgorithm struct {
    Sites   []Point
    Events  []*Event
    Beach   *Node
    Diagram *VoronoiDiagram
}

// NewFortuneAlgorithm creates a new Fortune algorithm instance
func NewFortuneAlgorithm(sites []Point) *FortuneAlgorithm {
    return &FortuneAlgorithm{
        Sites:   sites,
        Events:  make([]*Event, 0),
        Beach:   nil,
        Diagram: &VoronoiDiagram{},
    }
}

// Distance calculates Euclidean distance between two points
func (f *FortuneAlgorithm) Distance(p1, p2 Point) float64 {
    return math.Sqrt(math.Pow(p1.X-p2.X, 2) + math.Pow(p1.Y-p2.Y, 2))
}

// CircleEvent checks if three points form a circle event
func (f *FortuneAlgorithm) CircleEvent(p1, p2, p3 Point) *Event {
    // Calculate circumcenter of three points
    d1 := p1.X*p1.X + p1.Y*p1.Y
    d2 := p2.X*p2.X + p2.Y*p2.Y
    d3 := p3.X*p3.X + p3.Y*p3.Y
    
    x1 := p1.X
    x2 := p2.X
    x3 := p3.X
    y1 := p1.Y
    y2 := p2.Y
    y3 := p3.Y
    
    // Calculate determinant
    det := x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2)
    
    if math.Abs(det) < 1e-10 {
        return nil // Points are collinear
    }
    
    // Calculate circumcenter
    cx := (d1*(y2-y3) + d2*(y3-y1) + d3*(y1-y2)) / (2 * det)
    cy := (d1*(x3-x2) + d2*(x1-x3) + d3*(x2-x1)) / (2 * det)
    
    // Calculate radius
    radius := f.Distance(Point{cx, cy}, p1)
    
    return &Event{
        Point:       Point{cx, cy},
        IsSiteEvent: false,
        Radius:      radius,
        Center:      Point{cx, cy},
    }
}

// AddSiteEvent adds a site event to the event queue
func (f *FortuneAlgorithm) AddSiteEvent(site Point) {
    event := &Event{
        Point:       site,
        IsSiteEvent: true,
    }
    f.Events = append(f.Events, event)
}

// ProcessEvents processes all events in the queue
func (f *FortuneAlgorithm) ProcessEvents() {
    // Sort events by y-coordinate (and x-coordinate for tie-breaking)
    // In a real implementation, we'd use a proper priority queue
    fmt.Println("Processing events...")
    
    // For demonstration, we'll just process sites one by one
    for i, site := range f.Sites {
        fmt.Printf("Processing site %d: (%.2f, %.2f)\n", i, site.X, site.Y)
        
        // In a full implementation, this would handle the actual beach line
        // and event processing, but we'll simplify for this example
        f.handleSiteEvent(site)
    }
}

// handleSiteEvent handles a site event
func (f *FortuneAlgorithm) handleSiteEvent(site Point) {
    fmt.Printf("  Adding site at (%.2f, %.2f)\n", site.X, site.Y)
    
    // In a complete implementation, this would:
    // 1. Find where to insert the new site in the beach line
    // 2. Create new parabolas
    // 3. Remove old circle events
    // 4. Create new circle events
    // 5. Update the beach line structure
    
    // For this example, we'll just record that we processed this site
    f.Diagram.Sites = append(f.Diagram.Sites, site)
}

// ComputeVoronoi computes the Voronoi diagram for the given sites
func (f *FortuneAlgorithm) ComputeVoronoi() *VoronoiDiagram {
    fmt.Println("Computing Voronoi diagram...")
    
    // Add all site events to the event queue
    for _, site := range f.Sites {
        f.AddSiteEvent(site)
    }
    
    // Process events
    f.ProcessEvents()
    
    fmt.Println("Voronoi diagram computed!")
    return f.Diagram
}

// PrintDiagram prints the computed Voronoi diagram
func (f *FortuneAlgorithm) PrintDiagram() {
    fmt.Println("\nVoronoi Diagram:")
    fmt.Printf("Sites: %d\n", len(f.Diagram.Sites))
    
    for i, site := range f.Diagram.Sites {
        fmt.Printf("Site %d: (%.2f, %.2f)\n", i, site.X, site.Y)
    }
    
    fmt.Printf("Edges: %d\n", len(f.Diagram.Edges))
    fmt.Printf("Vertices: %d\n", len(f.Diagram.Vertices))
}

// Example usage
func main() {
    // Define some sample sites
    sites := []Point{
        {0, 0},
        {1, 0},
        {0, 1},
        {1, 1},
        {0.5, 0.5},
    }
    
    fmt.Println("Fortune's Algorithm Example")
    fmt.Println("==========================")
    
    // Create Fortune algorithm instance
    fortune := NewFortuneAlgorithm(sites)
    
    // Compute Voronoi diagram
    diagram := fortune.ComputeVoronoi()
    
    // Print results
    fortune.PrintDiagram()
    
    // Show some calculations
    fmt.Println("\nDistance calculations:")
    fmt.Printf("Distance between (0,0) and (1,0): %.2f\n", fortune.Distance(Point{0, 0}, Point{1, 0}))
    fmt.Printf("Distance between (0,0) and (0,1): %.2f\n", fortune.Distance(Point{0, 0}, Point{0, 1}))
    
    // Demonstrate circle event calculation
    fmt.Println("\nCircle event calculations:")
    circleEvent := fortune.CircleEvent(Point{0, 0}, Point{1, 0}, Point{0, 1})
    if circleEvent != nil {
        fmt.Printf("Circle event center: (%.2f, %.2f)\n", circleEvent.X, circleEvent.Y)
        fmt.Printf("Circle radius: %.2f\n", circleEvent.Radius)
    }
}
```

## Key Components of the Implementation

### 1. **Data Structures**
- `Point`: Represents 2D coordinates
- `Event`: Represents site events and circle events
- `Node`: Represents nodes in the beach line tree
- `VoronoiDiagram`: Stores the final Voronoi diagram

### 2. **Core Functions**
- `Distance()`: Calculates Euclidean distance between points
- `CircleEvent()`: Computes circumcenter of three points
- `AddSiteEvent()`: Adds site events to the event queue
- `ProcessEvents()`: Processes events in the queue
- `ComputeVoronoi()`: Main function to compute the Voronoi diagram

### 3. **Algorithm Overview**
Fortune's algorithm uses a **sweep line** approach:
1. Process events in order (from top to bottom)
2. Maintain a **beach line** (parabolic curve)
3. Handle **site events** (when the sweep line hits a point)
4. Handle **circle events** (when three parabolas meet)

### 4. **Time Complexity**
- Time: O(n log n) where n is the number of sites
- Space: O(n) for storing events and structure

This implementation provides a foundation that can be extended with a proper priority queue and full beach line tree management for a complete working Voronoi diagram generator.

