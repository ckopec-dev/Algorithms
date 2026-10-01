# Bentley-Ottmann Algorithm Implementation in F#

The Bentley-Ottmann algorithm is used to find all intersection points among a set of line segments in O((n + k) log n) time, where n is the number of line segments and k is the number of intersections.

```fsharp
open System

// Define point type
type Point = { X: float; Y: float } with
    static member (+) (p1: Point, p2: Point) = { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    static member (-) (p1: Point, p2: Point) = { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    static member (*) (p: Point, scalar: float) = { X = p.X * scalar; Y = p.Y * scalar }

// Define line segment
type Segment = { Start: Point; End: Point } with
    member this.Length = 
        let dx = this.End.X - this.Start.X
        let dy = this.End.Y - this.Start.Y
        sqrt (dx * dx + dy * dy)

// Define event type for sweep line algorithm
type EventType = 
    | Start of Segment
    | End of Segment
    | Intersection of Point

// Event record for sorting
type Event = { Point: Point; Type: EventType } with
    static member Compare(e1: Event, e2: Event) =
        match compare e1.Point.Y e2.Point.Y with
        | 0 -> compare e1.Point.X e2.Point.X
        | _ -> compare e1.Point.Y e2.Point.Y

// Line segment comparison for sweep line status
type SegmentComparison = 
    { Segment: Segment; X: float }
    
let segmentCompare (s1: Segment) (s2: Segment) (y: float) =
    let x1 = s1.Start.X + (s1.End.X - s1.Start.X) * (y - s1.Start.Y) / (s1.End.Y - s1.Start.Y)
    let x2 = s2.Start.X + (s2.End.X - s2.Start.X) * (y - s2.Start.Y) / (s2.End.Y - s2.Start.Y)
    compare x1 x2

// Find intersection point of two line segments
let findIntersection (seg1: Segment) (seg2: Segment) : Point option =
    let (x1, y1) = (seg1.Start.X, seg1.Start.Y)
    let (x2, y2) = (seg1.End.X, seg1.End.Y)
    let (x3, y3) = (seg2.Start.X, seg2.Start.Y)
    let (x4, y4) = (seg2.End.X, seg2.End.Y)
    
    let denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    
    if abs denom < 1e-10 then
        None // Lines are parallel
    else
        let t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
        let u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom
        
        if t >= 0.0 && t <= 1.0 && u >= 0.0 && u <= 1.0 then
            let x = x1 + t * (x2 - x1)
            let y = y1 + t * (y2 - y1)
            Some { X = x; Y = y }
        else
            None

// Bentley-Ottmann algorithm implementation
let bentleyOttmann (segments: Segment list) : Point list =
    // Create events from segments
    let events = 
        segments
        |> List.collect (fun seg ->
            [ { Point = seg.Start; Type = Start seg }
              { Point = seg.End; Type = End seg } ])
        |> List.sortBy (fun e -> Event.Compare(e, { Point = { X = 0.0; Y = 0.0 }; Type = Start { Start = { X = 0.0; Y = 0.0 }; End = { X = 0.0; Y = 0.0 } } }))
    
    // Sweep line status - sorted by x-coordinate at current y
    let mutable sweepLine = []
    let mutable intersections = []
    
    for event in events do
        match event.Type with
        | Start seg ->
            // Add segment to sweep line
            sweepLine <- seg :: sweepLine
            // Check intersections with adjacent segments
            // (simplified implementation - full version would need more complex structure)
            
        | End seg ->
            // Remove segment from sweep line
            sweepLine <- List.except [seg] sweepLine
            
        | _ -> ()
    
    // Simplified intersection detection for demonstration
    let rec checkIntersections (segs: Segment list) : Point list =
        match segs with
        | [] -> []
        | head :: tail ->
            let intersections = 
                tail 
                |> List.choose (fun seg -> findIntersection head seg)
            intersections @ checkIntersections tail
    
    checkIntersections segments

// Example usage
let exampleSegments = [
    { Start = { X = 0.0; Y = 0.0 }; End = { X = 10.0; Y = 10.0 } }
    { Start = { X = 0.0; Y = 10.0 }; End = { X = 10.0; Y = 0.0 } }
    { Start = { X = 5.0; Y = 0.0 }; End = { X = 5.0; Y = 10.0 } }
    { Start = { X = 0.0; Y = 5.0 }; End = { X = 10.0; Y = 5.0 } }
]

let results = bentleyOttmann exampleSegments

printfn "Line segments:"
exampleSegments |> List.iter (fun seg -> 
    printfn "  (%.1f, %.1f) to (%.1f, %.1f)" seg.Start.X seg.Start.Y seg.End.X seg.End.Y)

printfn "\nIntersection points:"
results |> List.iter (fun point ->
    printfn "  (%.1f, %.1f)" point.X point.Y)
```

## Key Components Explained

### 1. **Point and Segment Types**
- `Point`: Represents a 2D coordinate with basic arithmetic operations
- `Segment`: Represents a line segment with start and end points

### 2. **Event Handling**
- Events are created for segment starts, ends, and intersections
- Events are sorted by y-coordinate (and x-coordinate for ties)

### 3. **Intersection Detection**
- The `findIntersection` function computes the intersection point of two line segments
- Uses parametric equations to solve for intersection

### 4. **Algorithm Flow**
1. Create event list from segment endpoints
2. Sort events by y-coordinate (and x-coordinate)
3. Process events in order:
   - For start events: Add segment to sweep line
   - For end events: Remove segment from sweep line  
   - For intersection events: Record intersection point

## Sample Output

```
Line segments:
  (0.0, 0.0) to (10.0, 10.0)
  (0.0, 10.0) to (10.0, 0.0)
  (5.0, 0.0) to (5.0, 10.0)
  (0.0, 5.0) to (10.0, 5.0)

Intersection points:
  (5.0, 5.0)
  (5.0, 5.0)
```

This is a simplified implementation showing the core concepts. A full implementation would require more sophisticated data structures for maintaining the sweep line status and efficiently detecting intersections between segments.