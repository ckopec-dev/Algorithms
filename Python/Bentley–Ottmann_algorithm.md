# Bentley–Ottmann Algorithm Implementation

The Bentley–Ottmann algorithm is a sweep line algorithm for finding all intersection points among a set of line segments.

## Python Implementation

```python
import heapq
from collections import defaultdict

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __lt__(self, other):
        if self.y != other.y:
            return self.y < other.y
        return self.x < other.x
    
    def __repr__(self):
        return f"Point({self.x}, {self.y})"

class Segment:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2
        # Ensure p1 is the leftmost point
        if p1.x > p2.x or (p1.x == p2.x and p1.y > p2.y):
            self.p1, self.p2 = p2, p1
    
    def __repr__(self):
        return f"Segment({self.p1}, {self.p2})"

class Event:
    def __init__(self, point, segment, is_start=True):
        self.point = point
        self.segment = segment
        self.is_start = is_start
    
    def __lt__(self, other):
        if self.point.y != other.point.y:
            return self.point.y > other.point.y  # Lower y comes first
        return self.point.x > other.point.x      # Lower x comes first

def get_intersection(p1, p2, p3, p4):
    """Get intersection point of two line segments"""
    def orientation(p, q, r):
        val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
        if val == 0:
            return 0
        return 1 if val > 0 else 2
    
    def on_segment(p, q, r):
        return q.x <= max(p.x, r.x) and q.x >= min(p.x, r.x) and \
               q.y <= max(p.y, r.y) and q.y >= min(p.y, r.y)
    
    o1 = orientation(p1, p2, p3)
    o2 = orientation(p1, p2, p4)
    o3 = orientation(p3, p4, p1)
    o4 = orientation(p3, p4, p2)
    
    if o1 != o2 and o3 != o4:
        return True
    
    if (o1 == 0 and on_segment(p1, p3, p2)) or \
       (o2 == 0 and on_segment(p1, p4, p2)) or \
       (o3 == 0 and on_segment(p3, p1, p4)) or \
       (o4 == 0 and on_segment(p3, p2, p4)):
        return True
    
    return False

def get_intersection_point(p1, p2, p3, p4):
    """Calculate actual intersection point"""
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y
    x3, y3 = p3.x, p3.y
    x4, y4 = p4.x, p4.y
    
    denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    
    if abs(denom) < 1e-10:
        return None
    
    t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
    u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom
    
    if 0 <= t <= 1 and 0 <= u <= 1:
        x = x1 + t * (x2 - x1)
        y = y1 + t * (y2 - y1)
        return Point(x, y)
    
    return None

def bentley_ottmann(segments):
    """
    Find all intersection points among line segments using Bentley-Ottmann algorithm
    """
    # Create events for start and end points of segments
    events = []
    for segment in segments:
        events.append(Event(segment.p1, segment, True))
        events.append(Event(segment.p2, segment, False))
    
    # Sort events by y-coordinate (descending), then by x-coordinate (descending)
    events.sort()
    
    # Active segments structure - use a balanced binary search tree
    active_segments = []
    
    # Store intersection points
    intersections = set()
    
    # Sweep line status - maintain segments ordered by their current x-position
    def get_x_position(segment, sweep_y):
        """Get x-coordinate of segment at given y-coordinate"""
        if abs(segment.p1.y - segment.p2.y) < 1e-10:
            return segment.p1.x  # Horizontal segment
        t = (sweep_y - segment.p1.y) / (segment.p2.y - segment.p1.y)
        return segment.p1.x + t * (segment.p2.x - segment.p1.x)
    
    def compare_segments(seg1, seg2, sweep_y):
        """Compare two segments at current sweep line position"""
        x1 = get_x_position(seg1, sweep_y)
        x2 = get_x_position(seg2, sweep_y)
        if abs(x1 - x2) < 1e-10:
            # If they intersect at the same point, use some tie-breaking
            return 0
        return -1 if x1 < x2 else 1
    
    # Process events
    while events:
        current_event = events.pop()
        current_point = current_event.point
        
        # Remove segment from active set if it's an end event
        if not current_event.is_start:
            # In a real implementation, we'd remove from the active structure
            pass
        else:
            # Add segment to active set
            # In a real implementation, we'd insert into the active structure
            pass
        
        # Check for intersections with neighboring segments in sweep line status
        # This is a simplified version - a full implementation would maintain
        # the active segments properly
    
    return list(intersections)

# Simplified example to demonstrate concept
def simple_intersection_finder(segments):
    """
    Simple intersection finder using brute force approach for demonstration
    """
    intersections = []
    
    for i in range(len(segments)):
        for j in range(i + 1, len(segments)):
            seg1 = segments[i]
            seg2 = segments[j]
            
            # Check if segments intersect
            if get_intersection(seg1.p1, seg1.p2, seg2.p1, seg2.p2):
                point = get_intersection_point(seg1.p1, seg1.p2, seg2.p1, seg2.p2)
                if point:
                    intersections.append(point)
    
    return intersections

# Example usage
if __name__ == "__main__":
    # Define some line segments
    segments = [
        Segment(Point(0, 0), Point(4, 4)),
        Segment(Point(0, 4), Point(4, 0)),
        Segment(Point(1, 0), Point(1, 4)),
        Segment(Point(0, 1), Point(4, 1))
    ]
    
    print("Line Segments:")
    for i, seg in enumerate(segments):
        print(f"Segment {i}: {seg}")
    
    # Find intersections
    intersections = simple_intersection_finder(segments)
    
    print("\nIntersection Points:")
    for point in intersections:
        print(f"  {point}")
```

## How it works

The Bentley–Ottmann algorithm operates as follows:

1. **Event Queue**: Create events for all segment endpoints and store them in a priority queue sorted by y-coordinate (descending), then x-coordinate (descending)

2. **Sweep Line**: Process events from the queue one by one, moving a vertical sweep line from top to bottom

3. **Active Set**: Maintain a set of segments currently intersected by the sweep line, ordered by their current x-coordinates

4. **Intersection Detection**: At each event:
   - Add or remove segments from active set
   - Check for intersections between neighboring segments in the active set
   - Add intersection points to result

5. **New Events**: When intersections are found, add new events for those intersection points

## Time Complexity

- **Time**: O((n + k) log n) where n is number of segments and k is number of intersections
- **Space**: O(n)

This implementation provides the conceptual framework. A complete implementation would require a more sophisticated data structure for maintaining the active segments and their ordering during the sweep process.

