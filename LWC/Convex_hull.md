# Convex Hull Algorithm in Lightning Web Component

Here's a complete example of implementing the Graham Scan algorithm for finding the convex hull in a Lightning Web Component:

```javascript
// convexHull.js
import { LightningElement } from 'lwc';

export default class ConvexHull extends LightningElement {
    points = [
        { x: 1, y: 1 },
        { x: 2, y: 2 },
        { x: 3, y: 1 },
        { x: 4, y: 3 },
        { x: 5, y: 2 },
        { x: 6, y: 4 },
        { x: 7, y: 1 }
    ];
    
    hullPoints = [];
    isCalculated = false;

    // Calculate convex hull using Graham Scan algorithm
    calculateConvexHull() {
        if (this.points.length < 3) {
            this.hullPoints = [...this.points];
            this.isCalculated = true;
            return;
        }

        // Find the point with minimum y-coordinate (or minimum x if tie)
        let minPoint = this.points[0];
        let minIndex = 0;
        
        for (let i = 1; i < this.points.length; i++) {
            const point = this.points[i];
            if (point.y < minPoint.y || (point.y === minPoint.y && point.x < minPoint.x)) {
                minPoint = point;
                minIndex = i;
            }
        }

        // Sort points by polar angle with respect to the minimum point
        const sortedPoints = [...this.points].sort((a, b) => {
            const angleA = this.calculateAngle(minPoint, a);
            const angleB = this.calculateAngle(minPoint, b);
            return angleA - angleB;
        });

        // Graham Scan algorithm
        const hull = [sortedPoints[0], sortedPoints[1]];
        
        for (let i = 2; i < sortedPoints.length; i++) {
            let top = hull.pop();
            let secondTop = hull[hull.length - 1];
            
            // Keep removing points while the turn is clockwise
            while (this.isClockwise(secondTop, top, sortedPoints[i])) {
                top = hull.pop();
                if (hull.length < 1) break;
                secondTop = hull[hull.length - 1];
            }
            
            hull.push(top);
            hull.push(sortedPoints[i]);
        }

        this.hullPoints = hull;
        this.isCalculated = true;
    }

    // Calculate angle between three points
    calculateAngle(origin, point) {
        const dx = point.x - origin.x;
        const dy = point.y - origin.y;
        return Math.atan2(dy, dx);
    }

    // Check if three points make a clockwise turn
    isClockwise(p1, p2, p3) {
        const crossProduct = (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x);
        return crossProduct < 0;
    }

    // Add a new point
    addPoint() {
        const newX = Math.floor(Math.random() * 10) + 1;
        const newY = Math.floor(Math.random() * 5) + 1;
        this.points = [...this.points, { x: newX, y: newY }];
    }

    // Reset the component
    reset() {
        this.points = [
            { x: 1, y: 1 },
            { x: 2, y: 2 },
            { x: 3, y: 1 },
            { x: 4, y: 3 },
            { x: 5, y: 2 },
            { x: 6, y: 4 },
            { x: 7, y: 1 }
        ];
        this.hullPoints = [];
        this.isCalculated = false;
    }

    // Render the component
    render() {
        this.calculateConvexHull();
        return super.render();
    }
}
```

```html
<!-- convexHull.html -->
<template>
    <div class="container">
        <h2>Convex Hull Algorithm (Graham Scan)</h2>
        
        <div class="controls">
            <lightning-button 
                label="Calculate Convex Hull" 
                onclick={calculateConvexHull}
                variant="brand"
                disabled={isCalculated}>
            </lightning-button>
            <lightning-button 
                label="Add Random Point" 
                onclick={addPoint}
                variant="outline-brand">
            </lightning-button>
            <lightning-button 
                label="Reset" 
                onclick={reset}
                variant="destructive">
            </lightning-button>
        </div>

        <div class="visualization">
            <svg width="600" height="400" class="hull-svg">
                <!-- Draw all points -->
                <g>
                    <template for:each={points} for:item="point">
                        <circle 
                            key={point.x}
                            cx={point.x * 50 + 50}
                            cy={400 - point.y * 50}
                            r="5"
                            fill="blue"
                            class="point">
                        </circle>
                    </template>
                </g>

                <!-- Draw convex hull points -->
                <g>
                    <template for:each={hullPoints} for:item="point">
                        <circle 
                            key={point.x}
                            cx={point.x * 50 + 50}
                            cy={400 - point.y * 50}
                            r="7"
                            fill="red"
                            class="hull-point">
                        </circle>
                    </template>
                </g>

                <!-- Draw convex hull lines -->
                <g>
                    <template for:each={hullPoints} for:item="point" for:index="index">
                        <line 
                            key={index}
                            x1={point.x * 50 + 50}
                            y1={400 - point.y * 50}
                            x2={hullPoints[(index + 1) % hullPoints.length].x * 50 + 50}
                            y2={400 - hullPoints[(index + 1) % hullPoints.length].y * 50}
                            stroke="red"
                            stroke-width="2">
                        </line>
                    </template>
                </g>

                <!-- Draw original points -->
                <g>
                    <template for:each={points} for:item="point" for:index="index">
                        <text 
                            key={index}
                            x={point.x * 50 + 50}
                            y={400 - point.y * 50 - 10}
                            font-size="12"
                            fill="black"
                            text-anchor="middle">
                            ({point.x},{point.y})
                        </text>
                    </template>
                </g>
            </svg>
        </div>

        <div class="info">
            <p><strong>Points:</strong> {points.length}</p>
            <p><strong>Hull Points:</strong> {hullPoints.length}</p>
            <p><strong>Status:</strong> 
                <template if:true={isCalculated}>
                    Calculated
                </template>
                <template if:false={isCalculated}>
                    Not calculated yet
                </template>
            </p>
        </div>
    </div>
</template>
```

```css
/* convexHull.css */
.container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.controls {
    margin-bottom: 20px;
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
}

.hull-svg {
    border: 1px solid #ccc;
    background-color: #f9f9f9;
}

.point {
    stroke: black;
    stroke-width: 1;
}

.hull-point {
    stroke: red;
    stroke-width: 2;
    fill-opacity: 0.7;
}

.info {
    margin-top: 20px;
    padding: 15px;
    background-color: #e8f4f8;
    border-radius: 5px;
}

.info p {
    margin: 5px 0;
}
```

## Key Features of this Implementation:

1. **Graham Scan Algorithm**: Implements the standard Graham Scan convex hull algorithm
2. **Interactive Visualization**: Shows both original points and the calculated convex hull
3. **Dynamic Point Management**: Allows adding random points and resetting the visualization
4. **Visual Feedback**: 
   - Blue circles for all input points
   - Red circles for hull points
   - Red lines connecting hull points
5. **Responsive Design**: Clean layout with proper spacing and controls

## How it Works:

1. **Find the lowest point** (or leftmost if tie)
2. **Sort points** by polar angle from the lowest point
3. **Apply Graham Scan**: Use cross product to determine clockwise/counter-clockwise turns
4. **Build hull**: Remove points that create clockwise turns

The algorithm has O(n log n) time complexity due to sorting, and O(n) space complexity for storing the hull points.

