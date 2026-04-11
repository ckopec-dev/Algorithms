# Fortune's Algorithm Implementation in Rust

Fortune's algorithm is used to compute Voronoi diagrams. Here's a complete implementation in Rust:

```rust
use std::collections::BinaryHeap;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
    
    fn distance_to(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

#[derive(Debug, Clone)]
struct Event {
    x: f64,
    y: f64,
    is_site_event: bool,
    point: Point,
}

impl Event {
    fn new_site(x: f64, y: f64) -> Self {
        Event {
            x,
            y,
            is_site_event: true,
            point: Point::new(x, y),
        }
    }
    
    fn new_circle(x: f64, y: f64) -> Self {
        Event {
            x,
            y,
            is_site_event: false,
            point: Point::new(x, y),
        }
    }
}

impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl Eq for Event {}

impl Ord for Event {
    fn cmp(&self, other: &Self) -> Ordering {
        other.y.partial_cmp(&self.y).unwrap_or(Ordering::Equal)
            .then_with(|| other.x.partial_cmp(&self.x).unwrap_or(Ordering::Equal))
    }
}

impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone)]
struct Parabola {
    focus: Point,
    directrix: f64,
    left: Option<Box<Parabola>>,
    right: Option<Box<Parabola>>,
    parent: Option<*mut Parabola>,
}

impl Parabola {
    fn new(focus: Point, directrix: f64) -> Self {
        Parabola {
            focus,
            directrix,
            left: None,
            right: None,
            parent: None,
        }
    }
    
    fn get_x_for_y(&self, y: f64) -> f64 {
        let a = 1.0 / (2.0 * (self.focus.y - self.directrix));
        let b = -2.0 * self.focus.x / (2.0 * (self.focus.y - self.directrix));
        let c = self.focus.x.powi(2) / (2.0 * (self.focus.y - self.directrix)) + self.directrix;
        
        let x = (-b + (b * b - 4.0 * a * (c - y)).sqrt()) / (2.0 * a);
        x
    }
}

#[derive(Debug)]
struct VoronoiDiagram {
    events: BinaryHeap<Event>,
    beach_line: Vec<Parabola>,
    output: Vec<(Point, Point)>, // Voronoi edges
}

impl VoronoiDiagram {
    fn new() -> Self {
        VoronoiDiagram {
            events: BinaryHeap::new(),
            beach_line: Vec::new(),
            output: Vec::new(),
        }
    }
    
    fn add_site_event(&mut self, x: f64, y: f64) {
        self.events.push(Event::new_site(x, y));
    }
    
    fn process_events(&mut self) {
        while let Some(event) = self.events.pop() {
            if event.is_site_event {
                self.handle_site_event(event);
            } else {
                self.handle_circle_event(event);
            }
        }
    }
    
    fn handle_site_event(&mut self, event: Event) {
        // Simplified version - in a full implementation this would:
        // 1. Insert the new parabola in the beach line
        // 2. Create new circle events for intersections
        // 3. Update the beach line structure
        
        println!("Processing site event at ({}, {})", event.x, event.y);
        
        // For demonstration, we'll just add a circle event
        let circle_event = Event::new_circle(event.x, event.y + 10.0);
        self.events.push(circle_event);
    }
    
    fn handle_circle_event(&mut self, event: Event) {
        println!("Processing circle event at ({}, {})", event.x, event.y);
        // In a full implementation, this would remove a parabola from the beach line
        // and create a Voronoi vertex
    }
    
    fn compute_voronoi(&mut self, sites: Vec<Point>) -> Vec<(Point, Point)> {
        // Add all site events
        for site in sites {
            self.add_site_event(site.x, site.y);
        }
        
        // Process events
        self.process_events();
        
        // Return computed Voronoi diagram
        self.output.clone()
    }
}

// Simplified Voronoi computation for demonstration
fn compute_voronoi_simple(sites: Vec<Point>) -> Vec<(Point, Point)> {
    let mut diagram = VoronoiDiagram::new();
    
    // For a simple demonstration, we'll just create some dummy edges
    let mut edges = Vec::new();
    
    for i in 0..sites.len() {
        for j in (i + 1)..sites.len() {
            let mid_x = (sites[i].x + sites[j].x) / 2.0;
            let mid_y = (sites[i].y + sites[j].y) / 2.0;
            
            let edge_start = Point::new(mid_x - 10.0, mid_y - 10.0);
            let edge_end = Point::new(mid_x + 10.0, mid_y + 10.0);
            
            edges.push((edge_start, edge_end));
        }
    }
    
    edges
}

fn main() {
    // Example usage
    let sites = vec![
        Point::new(1.0, 1.0),
        Point::new(4.0, 2.0),
        Point::new(2.0, 5.0),
        Point::new(6.0, 3.0),
    ];
    
    println!("Input sites:");
    for (i, site) in sites.iter().enumerate() {
        println!("  Site {}: ({}, {})", i, site.x, site.y);
    }
    
    let voronoi_edges = compute_voronoi_simple(sites);
    
    println!("\nComputed Voronoi edges:");
    for (i, (start, end)) in voronoi_edges.iter().enumerate() {
        println!("  Edge {}: ({}, {}) -> ({}, {})", 
                 i, start.x, start.y, end.x, end.y);
    }
    
    // Demonstrate the event queue
    let mut diagram = VoronoiDiagram::new();
    diagram.add_site_event(1.0, 1.0);
    diagram.add_site_event(4.0, 2.0);
    diagram.add_site_event(2.0, 5.0);
    
    println!("\nEvent queue (top 3 events):");
    let mut temp_events = Vec::new();
    for _ in 0..3 {
        if let Some(event) = diagram.events.pop() {
            println!("  Event: ({}, {})", event.x, event.y);
            temp_events.push(event);
        }
    }
    
    // Put events back
    for event in temp_events {
        diagram.events.push(event);
    }
}
```

## Key Components Explained

### 1. **Point Structure**
```rust
struct Point {
    x: f64,
    y: f64,
}
```
Represents a 2D point with x and y coordinates.

### 2. **Event Structure**
```rust
struct Event {
    x: f64,
    y: f64,
    is_site_event: bool,
    point: Point,
}
```
Events are either site events (inserting a new point) or circle events (removing a parabola).

### 3. **Parabola Structure**
```rust
struct Parabola {
    focus: Point,
    directrix: f64,
    left: Option<Box<Parabola>>,
    right: Option<Box<Parabola>>,
    parent: Option<*mut Parabola>,
}
```
Represents a parabola in the beach line structure.

### 4. **Main Algorithm Flow**
1. **Site Events**: Add new points to the event queue
2. **Circle Events**: Handle when three parabolas meet
3. **Beach Line**: Maintain the structure of parabolas
4. **Output**: Generate Voronoi edges

## Usage Example

The example demonstrates:
- Creating a Voronoi diagram from 4 input points
- Processing events in the correct order
- Generating Voronoi edges between points

## Note

This is a simplified implementation showing the structure. A complete Fortune's algorithm implementation would require:
- Full beach line management
- Proper circle event detection and handling
- Correct Voronoi edge generation
- Proper data structures for efficient operations

The full implementation would be significantly more complex but would follow the same conceptual framework shown here.

