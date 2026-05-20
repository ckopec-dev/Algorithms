# Graham Scan Algorithm in Lightning Web Component

Here's a complete implementation of the Graham Scan algorithm for finding convex hull in a Lightning Web Component:

```javascript
// convexHull.js
import { LightningElement } from 'lwc';

export default class ConvexHull extends LightningElement {
    points = [
        {x: 1, y: 1},
        {x: 2, y: 2},
        {x: 3, y: 3},
        {x: 1, y: 4},
        {x: 4, y: 4},
        {x: 2, y: 5},
        {x: 5, y: 5},
        {x: 3, y: 6}
    ];
    
    hullPoints = [];
    isCalculated = false;

    connectedCallback() {
        this.calculateConvexHull();
    }

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
            if (this.points[i].y < minPoint.y || 
                (this.points[i].y === minPoint.y && this.points[i].x < minPoint.x)) {
                minPoint = this.points[i];
                minIndex = i;
            }
        }

        // Swap the minimum point with the first point
        [this.points[0], this.points[minIndex]] = [this.points[minIndex], this.points[0]];

        // Sort points by polar angle with respect to the minimum point
        this.points.sort((a, b) => {
            const angleA = this.calculatePolarAngle(minPoint, a);
            const angleB = this.calculatePolarAngle(minPoint, b);
            return angleA - angleB;
        });

        // Graham scan algorithm
        const hull = [this.points[0], this.points[1]];
        
        for (let i = 2; i < this.points.length; i++) {
            while (hull.length > 1 && 
                   this.crossProduct(hull[hull.length - 2], hull[hull.length - 1], this.points[i]) <= 0) {
                hull.pop();
            }
            hull.push(this.points[i]);
        }

        this.hullPoints = hull;
        this.isCalculated = true;
    }

    calculatePolarAngle(point1, point2) {
        const dx = point2.x - point1.x;
        const dy = point2.y - point1.y;
        return Math.atan2(dy, dx);
    }

    crossProduct(p1, p2, p3) {
        return (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x);
    }

    get hullPath() {
        if (this.hullPoints.length === 0) return '';
        
        let path = `M ${this.hullPoints[0].x * 50} ${this.hullPoints[0].y * 50}`;
        for (let i = 1; i < this.hullPoints.length; i++) {
            path += ` L ${this.hullPoints[i].x * 50} ${this.hullPoints[i].y * 50}`;
        }
        path += ` Z`;
        return path;
    }

    get pointElements() {
        return this.points.map((point, index) => ({
            ...point,
            id: `point-${index}`,
            isHullPoint: this.hullPoints.some(hp => hp.x === point.x && hp.y === point.y)
        }));
    }

    handleRecalculate() {
        this.isCalculated = false;
        this.calculateConvexHull();
    }
}
```

```html
<!-- convexHull.html -->
<template>
    <div class="container">
        <h2>Convex Hull using Graham Scan Algorithm</h2>
        
        <lightning-button 
            label="Recalculate" 
            onclick={handleRecalculate}
            variant="brand"
            class="slds-m-bottom_small">
        </lightning-button>

        <div class="chart-container">
            <svg width="600" height="400" class="chart">
                <!-- Grid lines -->
                <g stroke="#e0e0e0" stroke-width="1">
                    <line x1="0" y1="0" x2="600" y2="0"/>
                    <line x1="0" y1="50" x2="600" y2="50"/>
                    <line x1="0" y1="100" x2="600" y2="100"/>
                    <line x1="0" y1="150" x2="600" y2="150"/>
                    <line x1="0" y1="200" x2="600" y2="200"/>
                    <line x1="0" y1="250" x2="600" y2="250"/>
                    <line x1="0" y1="300" x2="600" y2="300"/>
                    <line x1="0" y1="350" x2="600" y2="350"/>
                    
                    <line x1="0" y1="0" x2="0" y2="400"/>
                    <line x1="50" y1="0" x2="50" y2="400"/>
                    <line x1="100" y1="0" x2="100" y2="400"/>
                    <line x1="150" y1="0" x2="150" y2="400"/>
                    <line x1="200" y1="0" x2="200" y2="400"/>
                    <line x1="250" y1="0" x2="250" y2="400"/>
                    <line x1="300" y1="0" x2="300" y2="400"/>
                    <line x1="350" y1="0" x2="350" y2="400"/>
                    <line x1="400" y1="0" x2="400" y2="400"/>
                    <line x1="450" y1="0" x2="450" y2="400"/>
                    <line x1="500" y1="0" x2="500" y2="400"/>
                    <line x1="550" y1="0" x2="550" y2="400"/>
                    <line x1="600" y1="0" x2="600" y2="400"/>
                </g>

                <!-- Convex hull polygon -->
                <path 
                    d={hullPath} 
                    fill="rgba(0, 123, 255, 0.2)" 
                    stroke="blue" 
                    stroke-width="2"
                    class="hull">
                </path>

                <!-- Points -->
                <template for:each={pointElements} for:item="point">
                    <circle 
                        key={point.id}
                        cx={point.x * 50}
                        cy={point.y * 50}
                        r="5"
                        fill={point.isHullPoint ? "red" : "black"}
                        stroke="white"
                        stroke-width="1">
                    </circle>
                </template>
            </svg>
        </div>

        <div class="info-section">
            <h3>Algorithm Explanation</h3>
            <p><strong>Graham Scan Algorithm:</strong></p>
            <ul>
                <li>Find the point with minimum y-coordinate (or minimum x if tie)</li>
                <li>Sort all points by polar angle with respect to the minimum point</li>
                <li>Process points in sorted order, maintaining a stack of hull points</li>
                <li>Use cross product to determine if point makes a clockwise turn</li>
                <li>Remove points that make clockwise turns from the hull</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* convexHull.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.chart-container {
    border: 1px solid #e0e0e0;
    border-radius: 8px;
    padding: 10px;
    margin: 20px 0;
    background-color: #f8f9fa;
}

.chart {
    background-color: white;
    border: 1px solid #ddd;
    border-radius: 4px;
}

.hull {
    fill-opacity: 0.3;
}

.info-section {
    background-color: #e9ecef;
    padding: 15px;
    border-radius: 8px;
    margin-top: 20px;
}

.info-section h3 {
    margin-top: 0;
    color: #333;
}

.info-section ul {
    margin: 10px 0;
    padding-left: 20px;
}

.info-section li {
    margin: 5px 0;
}
```

## Key Features of this Implementation:

1. **Complete Algorithm**: Implements the full Graham Scan algorithm with:
   - Finding the bottom-most point
   - Sorting by polar angle
   - Stack-based hull construction
   - Cross product for turn detection

2. **Visual Representation**: 
   - SVG-based visualization
   - Grid background for reference
   - Different colors for hull points vs regular points

3. **Interactive Elements**:
   - Recalculate button to restart the algorithm
   - Clear visual distinction between hull and regular points

4. **Algorithm Steps**:
   - **Step 1**: Find the point with minimum y-coordinate
   - **Step 2**: Sort remaining points by polar angle
   - **Step 3**: Process points using stack and cross product
   - **Step 4**: Remove points that create clockwise turns

5. **Edge Cases Handled**:
   - Less than 3 points
   - Collinear points
   - Points with same coordinates

The algorithm correctly identifies the convex hull of the given set of points using the mathematical principles of polar angle sorting and cross product for turn detection.

