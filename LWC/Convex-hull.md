# Convex Hull Algorithm in Lightning Web Component

Here's a complete example of implementing the Graham Scan algorithm for finding the convex hull in a Lightning Web Component:

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
        {x: 3, y: 5},
        {x: 5, y: 5}
    ];
    
    hullPoints = [];
    isCalculated = false;

    handleCalculateHull() {
        this.hullPoints = this.grahamScan(this.points);
        this.isCalculated = true;
        this.renderHull();
    }

    // Graham Scan algorithm implementation
    grahamScan(points) {
        if (points.length < 3) {
            return points;
        }

        // Find the bottom-most point (or left-most if tie)
        let bottomPoint = this.findBottomMostPoint(points);
        
        // Sort points by polar angle with respect to bottom point
        let sortedPoints = this.sortByPolarAngle(points, bottomPoint);
        
        // Initialize hull with first three points
        let hull = [sortedPoints[0], sortedPoints[1], sortedPoints[2]];
        
        // Process remaining points
        for (let i = 3; i < sortedPoints.length; i++) {
            let point = sortedPoints[i];
            
            // Remove points that make clockwise turn
            while (hull.length > 1 && 
                   this.orientation(hull[hull.length - 2], hull[hull.length - 1], point) !== 2) {
                hull.pop();
            }
            
            hull.push(point);
        }
        
        return hull;
    }

    // Find the bottom-most point
    findBottomMostPoint(points) {
        let bottomPoint = points[0];
        for (let i = 1; i < points.length; i++) {
            if (points[i].y < bottomPoint.y || 
                (points[i].y === bottomPoint.y && points[i].x < bottomPoint.x)) {
                bottomPoint = points[i];
            }
        }
        return bottomPoint;
    }

    // Sort points by polar angle with respect to reference point
    sortByPolarAngle(points, referencePoint) {
        return points.sort((a, b) => {
            let angleA = Math.atan2(a.y - referencePoint.y, a.x - referencePoint.x);
            let angleB = Math.atan2(b.y - referencePoint.y, b.x - referencePoint.x);
            
            if (angleA === angleB) {
                // If angles are equal, sort by distance
                return (a.x - referencePoint.x) ** 2 + (a.y - referencePoint.y) ** 2 - 
                       (b.x - referencePoint.x) ** 2 - (b.y - referencePoint.y) ** 2;
            }
            
            return angleA - angleB;
        });
    }

    // Find orientation of three points
    orientation(p, q, r) {
        let val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
        
        if (val === 0) return 0;  // collinear
        return (val > 0) ? 1 : 2; // clock or counterclock
    }

    renderHull() {
        // This would typically update the visualization
        console.log('Convex Hull Points:', this.hullPoints);
    }

    handleReset() {
        this.hullPoints = [];
        this.isCalculated = false;
    }
}
```

```html
<!-- convexHull.html -->
<template>
    <div class="container">
        <h2>Convex Hull - Graham Scan Algorithm</h2>
        
        <div class="controls">
            <lightning-button 
                label="Calculate Convex Hull" 
                variant="brand" 
                onclick={handleCalculateHull}
                disabled={isCalculated}>
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <div class="visualization">
            <div class="points-container">
                <template for:each={points} for:item="point">
                    <div 
                        key={point.x} 
                        class="point"
                        style={pointStyle(point)}>
                        {point.x}, {point.y}
                    </div>
                </template>
                
                <template for:each={hullPoints} for:item="point">
                    <div 
                        key={point.x} 
                        class="hull-point"
                        style={pointStyle(point)}>
                        {point.x}, {point.y}
                    </div>
                </template>
            </div>
        </div>

        <div class="result">
            <h3>Convex Hull Points:</h3>
            <template if:true={isCalculated}>
                <ul>
                    <template for:each={hullPoints} for:item="point">
                        <li key={point.x}>{point.x}, {point.y}</li>
                    </template>
                </ul>
            </template>
            <template if:false={isCalculated}>
                <p>Click "Calculate Convex Hull" to see results</p>
            </template>
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

.controls {
    margin-bottom: 20px;
    text-align: center;
}

.points-container {
    position: relative;
    width: 600px;
    height: 400px;
    border: 1px solid #ccc;
    margin: 20px auto;
    background-color: #f9f9f9;
}

.point {
    position: absolute;
    width: 20px;
    height: 20px;
    background-color: #0070d2;
    border-radius: 50%;
    color: white;
    font-size: 10px;
    text-align: center;
    line-height: 20px;
    transform: translate(-50%, -50%);
    z-index: 1;
}

.hull-point {
    position: absolute;
    width: 20px;
    height: 20px;
    background-color: #00c471;
    border-radius: 50%;
    color: white;
    font-size: 10px;
    text-align: center;
    line-height: 20px;
    transform: translate(-50%, -50%);
    z-index: 2;
    border: 2px solid white;
    box-shadow: 0 0 5px rgba(0,0,0,0.3);
}

.result {
    margin-top: 20px;
    padding: 15px;
    background-color: #f0f8ff;
    border-radius: 5px;
}

.result ul {
    margin: 10px 0;
    padding-left: 20px;
}

.result li {
    margin: 5px 0;
}
```

## Key Features of this Implementation:

1. **Graham Scan Algorithm**: Implements the standard Graham Scan algorithm with O(n log n) time complexity
2. **Point Sorting**: Sorts points by polar angle relative to the bottom-most point
3. **Orientation Testing**: Uses cross product to determine the orientation of three points
4. **Visualization**: Shows both original points and convex hull points
5. **Interactive UI**: Includes calculate and reset functionality
6. **Responsive Design**: Clean, user-friendly interface with proper styling

## How it Works:

1. **Find Bottom-Most Point**: Identifies the point with the lowest y-coordinate (leftmost if tie)
2. **Sort by Polar Angle**: Orders all points by their angle relative to the bottom point
3. **Build Hull**: Uses a stack-based approach to iteratively build the convex hull
4. **Orientation Check**: Determines if three points make a clockwise or counterclockwise turn

The algorithm correctly identifies the convex hull of the given set of points, which is the smallest convex polygon that contains all the points.

