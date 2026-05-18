# Jarvis March Algorithm in Lightning Web Component

Here's a complete implementation of the Jarvis March (Gift Wrapping) algorithm for finding the convex hull of a set of points in a Lightning Web Component:

```javascript
// convexHull.js
import { LightningElement } from 'lwc';

export default class ConvexHull extends LightningElement {
    points = [
        {x: 1, y: 1},
        {x: 2, y: 2},
        {x: 3, y: 3},
        {x: 1, y: 4},
        {x: 4, y: 1},
        {x: 5, y: 2},
        {x: 3, y: 5},
        {x: 2, y: 3}
    ];
    
    convexHullPoints = [];
    isCalculated = false;

    handleCalculateHull() {
        this.convexHullPoints = this.jarvisMarch(this.points);
        this.isCalculated = true;
        this.renderCanvas();
    }

    jarvisMarch(points) {
        if (points.length < 3) {
            return points;
        }

        // Find the leftmost point
        let leftmost = 0;
        for (let i = 1; i < points.length; i++) {
            if (points[i].x < points[leftmost].x) {
                leftmost = i;
            }
        }

        let hull = [];
        let current = leftmost;
        let next;

        do {
            hull.push(points[current]);
            next = (current + 1) % points.length;

            for (let i = 0; i < points.length; i++) {
                if (this.orientation(points[current], points[i], points[next]) === 2) {
                    next = i;
                }
            }
            current = next;
        } while (current !== leftmost);

        return hull;
    }

    // Cross product to determine orientation
    orientation(p, q, r) {
        let val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
        if (val === 0) return 0;  // collinear
        return (val > 0) ? 1 : 2; // clock or counterclock wise
    }

    renderCanvas() {
        // This would typically update the visual representation
        console.log('Convex Hull Points:', this.convexHullPoints);
    }

    get hullPointsString() {
        return this.convexHullPoints.map(point => 
            `(${point.x}, ${point.y})`
        ).join(' -> ');
    }
}
```

```html
<!-- convexHull.html -->
<template>
    <div class="container">
        <h2>Convex Hull - Jarvis March Algorithm</h2>
        
        <lightning-button 
            label="Calculate Convex Hull" 
            onclick={handleCalculateHull}
            variant="brand"
            class="slds-m-bottom_small">
        </lightning-button>

        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <h3>Input Points</h3>
                <ul class="slds-list_dotted">
                    <template for:each={points} for:item="point">
                        <li key={point.x}>{point.x}, {point.y}</li>
                    </template>
                </ul>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <h3>Convex Hull Points</h3>
                <template if:true={isCalculated}>
                    <p>{hullPointsString}</p>
                </template>
                <template if:false={isCalculated}>
                    <p>Click "Calculate Convex Hull" to see results</p>
                </template>
            </div>
        </div>

        <div class="slds-m-top_small">
            <h3>Algorithm Explanation</h3>
            <p>The Jarvis March (Gift Wrapping) algorithm works by:</p>
            <ol>
                <li>Starting with the leftmost point</li>
                <li>For each point, finding the next point that makes the smallest clockwise angle</li>
                <li>Continuing until returning to the starting point</li>
            </ol>
            <p>Time Complexity: O(nh) where n is the number of points and h is the number of hull points.</p>
        </div>
    </div>
</template>
```

```css
/* convexHull.css */
.container {
    padding: 1rem;
    background-color: #f8f9fa;
    border-radius: 0.25rem;
    box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,.075);
}

.slds-list_dotted {
    padding-left: 1.5rem;
}

.slds-list_dotted li {
    margin-bottom: 0.25rem;
}
```

## Key Features of this Implementation:

1. **Algorithm Implementation**: Complete Jarvis March algorithm with orientation checking
2. **Visual Interface**: Clean LWC component with input/output display
3. **Point Handling**: Proper coordinate system management
4. **Orientation Calculation**: Cross product-based orientation detection
5. **User Interaction**: Calculate button to trigger the algorithm
6. **Responsive Design**: Uses SLDS styling for consistent look

## How the Algorithm Works:

1. **Find Starting Point**: Identify the leftmost point (or lowest if tie)
2. **Build Hull**: Iteratively find the next point by checking which point creates the smallest clockwise angle
3. **Termination**: Continue until returning to the starting point
4. **Orientation Test**: Uses cross product to determine if three points make a left turn (counter-clockwise)

## Time Complexity:
- **O(nh)** where n is the number of input points and h is the number of points on the hull
- **Space Complexity**: O(h) for storing the hull points

This implementation demonstrates a practical application of computational geometry algorithms in the Lightning Web Component framework.

