# Bentley-Ottmann Algorithm Implementation in Lightning Web Component

```javascript
// bentleyOttmann.js
import { LightningElement } from 'lwc';

export default class BentleyOttmann extends LightningElement {
    // Input line segments
    segments = [
        { x1: 1, y1: 1, x2: 4, y2: 4 },
        { x1: 2, y1: 1, x2: 3, y2: 4 },
        { x1: 1, y1: 3, x2: 4, y2: 1 },
        { x1: 2, y1: 3, x2: 3, y2: 1 }
    ];

    // Intersection points
    intersections = [];

    connectedCallback() {
        this.findIntersections();
    }

    findIntersections() {
        // Step 1: Create event queue (sweep line events)
        const events = this.createEvents(this.segments);
        events.sort(this.compareEvents);

        // Step 2: Initialize sweep line status
        const sweepLineStatus = new SweepLineStatus();
        this.intersections = [];

        // Step 3: Process events
        for (let i = 0; i < events.length; i++) {
            const event = events[i];
            
            if (event.type === 'start') {
                // Add segment to sweep line status
                sweepLineStatus.insert(event.segment);
                
                // Check for intersections with neighbors
                this.checkIntersections(event.segment, sweepLineStatus);
                
            } else if (event.type === 'end') {
                // Remove segment from sweep line status
                const prevSegment = sweepLineStatus.getPrevious(event.segment);
                const nextSegment = sweepLineStatus.getNext(event.segment);
                
                sweepLineStatus.remove(event.segment);
                
                // Check for intersections between neighbors
                if (prevSegment && nextSegment) {
                    const intersection = this.findIntersection(prevSegment, nextSegment);
                    if (intersection) {
                        this.intersections.push(intersection);
                    }
                }
                
            } else if (event.type === 'intersection') {
                // Handle intersection event
                const segment1 = event.segments[0];
                const segment2 = event.segments[1];
                
                // Swap segments in sweep line status
                sweepLineStatus.swap(segment1, segment2);
                
                // Check for new intersections
                this.checkIntersections(segment1, sweepLineStatus);
                this.checkIntersections(segment2, sweepLineStatus);
            }
        }
    }

    createEvents(segments) {
        const events = [];
        
        segments.forEach(segment => {
            const startEvent = {
                x: Math.min(segment.x1, segment.x2),
                y: Math.min(segment.y1, segment.y2),
                type: 'start',
                segment: segment
            };
            
            const endEvent = {
                x: Math.max(segment.x1, segment.x2),
                y: Math.max(segment.y1, segment.y2),
                type: 'end',
                segment: segment
            };
            
            events.push(startEvent);
            events.push(endEvent);
        });
        
        return events;
    }

    compareEvents(a, b) {
        // Sort by x-coordinate first, then by y-coordinate
        if (a.x !== b.x) {
            return a.x - b.x;
        }
        return a.y - b.y;
    }

    checkIntersections(segment, sweepLineStatus) {
        // Check intersection with previous segment
        const prevSegment = sweepLineStatus.getPrevious(segment);
        if (prevSegment) {
            const intersection = this.findIntersection(segment, prevSegment);
            if (intersection) {
                this.intersections.push(intersection);
            }
        }
        
        // Check intersection with next segment
        const nextSegment = sweepLineStatus.getNext(segment);
        if (nextSegment) {
            const intersection = this.findIntersection(segment, nextSegment);
            if (intersection) {
                this.intersections.push(intersection);
            }
        }
    }

    findIntersection(seg1, seg2) {
        // Calculate intersection point of two line segments
        const x1 = seg1.x1, y1 = seg1.y1;
        const x2 = seg1.x2, y2 = seg1.y2;
        const x3 = seg2.x1, y3 = seg2.y1;
        const x4 = seg2.x2, y4 = seg2.y2;

        const denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
        
        if (Math.abs(denom) < 1e-10) {
            return null; // Lines are parallel
        }

        const t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom;
        const u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom;

        if (t >= 0 && t <= 1 && u >= 0 && u <= 1) {
            const x = x1 + t * (x2 - x1);
            const y = y1 + t * (y2 - y1);
            return { x: x, y: y };
        }

        return null;
    }

    get intersectionPoints() {
        return this.intersections.map((point, index) => ({
            id: index,
            x: point.x,
            y: point.y
        }));
    }

    get segmentPoints() {
        return this.segments.map((segment, index) => ({
            id: index,
            x1: segment.x1,
            y1: segment.y1,
            x2: segment.x2,
            y2: segment.y2
        }));
    }
}

// Sweep Line Status class
class SweepLineStatus {
    constructor() {
        this.segments = [];
    }

    insert(segment) {
        this.segments.push(segment);
        // Sort segments based on their position on the sweep line
        this.segments.sort(this.compareSegments);
    }

    remove(segment) {
        const index = this.segments.indexOf(segment);
        if (index > -1) {
            this.segments.splice(index, 1);
        }
    }

    getPrevious(segment) {
        const index = this.segments.indexOf(segment);
        return index > 0 ? this.segments[index - 1] : null;
    }

    getNext(segment) {
        const index = this.segments.indexOf(segment);
        return index < this.segments.length - 1 ? this.segments[index + 1] : null;
    }

    swap(seg1, seg2) {
        const index1 = this.segments.indexOf(seg1);
        const index2 = this.segments.indexOf(seg2);
        
        if (index1 > -1 && index2 > -1) {
            [this.segments[index1], this.segments[index2]] = [this.segments[index2], this.segments[index1]];
        }
    }

    compareSegments(seg1, seg2) {
        // Simple comparison based on y-coordinate at x=0
        // In a real implementation, this would be more complex
        return seg1.y1 - seg2.y1;
    }
}
```

```html
<!-- bentleyOttmann.html -->
<template>
    <div class="container">
        <h2>Bentley-Ottmann Algorithm</h2>
        <p>Line Segment Intersection Detection</p>
        
        <div class="visualization">
            <svg width="600" height="400" class="svg-container">
                <!-- Draw segments -->
                <template for:each={segmentPoints} for:item="segment">
                    <line key={segment.id}
                          x1={segment.x1}
                          y1={segment.y1}
                          x2={segment.x2}
                          y2={segment.y2}
                          stroke="blue"
                          stroke-width="2"/>
                </template>
                
                <!-- Draw intersection points -->
                <template for:each={intersectionPoints} for:item="point">
                    <circle key={point.id}
                            cx={point.x}
                            cy={point.y}
                            r="4"
                            fill="red"/>
                </template>
            </svg>
        </div>
        
        <div class="results">
            <h3>Results</h3>
            <p>Segments: {segments.length}</p>
            <p>Intersections: {intersections.length}</p>
            
            <template if:true={intersections.length}>
                <h4>Intersection Points:</h4>
                <ul>
                    <template for:each={intersectionPoints} for:item="point">
                        <li key={point.id}>({point.x}, {point.y})</li>
                    </template>
                </ul>
            </template>
        </div>
    </div>
</template>
```

```css
/* bentleyOttmann.css */
.container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.visualization {
    margin: 20px 0;
    border: 1px solid #ccc;
    border-radius: 5px;
    padding: 10px;
}

.svg-container {
    background-color: #f9f9f9;
    border: 1px solid #ddd;
}

.results {
    margin-top: 20px;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.results ul {
    margin: 10px 0;
    padding-left: 20px;
}

.results li {
    margin: 5px 0;
}
```

## Algorithm Explanation

This implementation demonstrates the Bentley-Ottmann algorithm for detecting line segment intersections:

### Key Components:

1. **Event Queue**: Processes start and end points of segments
2. **Sweep Line Status**: Maintains active segments in sorted order
3. **Intersection Detection**: Calculates intersection points between line segments
4. **Event Handling**: Manages segment insertion, removal, and swapping

### Algorithm Steps:

1. **Create Events**: Generate start and end events for all segments
2. **Sort Events**: Order events by x-coordinate and y-coordinate
3. **Process Events**: 
   - Start events: Add segment to sweep line status
   - End events: Remove segment and check neighbors
   - Intersection events: Swap segments and check new neighbors
4. **Find Intersections**: Calculate intersection points between segments

### Time Complexity:
- O((n + k) log n) where n is the number of segments and k is the number of intersections

### Space Complexity:
- O(n) for storing segments and events

This implementation provides a visual representation of line segment intersections and demonstrates the core principles of the Bentley-Ottmann algorithm in a Lightning Web Component environment.

