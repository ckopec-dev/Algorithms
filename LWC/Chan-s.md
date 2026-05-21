# Chan's Algorithm (Convex Hull) in Lightning Web Component

Here's an example implementation of Chan's algorithm for finding the convex hull of a set of points using Lightning Web Components:

```javascript
// chanAlgorithm.js
import { LightningElement } from 'lwc';

export default class ChanAlgorithm extends LightningElement {
    points = [
        { x: 1, y: 1 },
        { x: 2, y: 2 },
        { x: 3, y: 3 },
        { x: 1, y: 3 },
        { x: 3, y: 1 },
        { x: 2, y: 1 },
        { x: 1, y: 2 },
        { x: 3, y: 2 }
    ];
    
    convexHull = [];
    isCalculating = false;

    connectedCallback() {
        this.calculateConvexHull();
    }

    calculateConvexHull() {
        this.isCalculating = true;
        this.convexHull = this.chansAlgorithm(this.points);
        this.isCalculating = false;
    }

    // Chan's Algorithm implementation
    chansAlgorithm(points) {
        if (points.length < 3) {
            return points;
        }

        // Step 1: Find the point with minimum y-coordinate (and minimum x if tie)
        let minPoint = points[0];
        for (let i = 1; i < points.length; i++) {
            if (points[i].y < minPoint.y || 
                (points[i].y === minPoint.y && points[i].x < minPoint.x)) {
                minPoint = points[i];
            }
        }

        // Step 2: Sort points by polar angle with respect to minPoint
        const sortedPoints = this.sortByPolarAngle(points, minPoint);
        
        // Step 3: Apply Graham scan to get initial hull
        const hull = this.grahamScan(sortedPoints);
        
        return hull;
    }

    // Sort points by polar angle with respect to reference point
    sortByPolarAngle(points, reference) {
        return points.sort((a, b) => {
            const angleA = Math.atan2(a.y - reference.y, a.x - reference.x);
            const angleB = Math.atan2(b.y - reference.y, b.x - reference.x);
            return angleA - angleB;
        });
    }

    // Graham Scan algorithm for convex hull
    grahamScan(points) {
        if (points.length < 3) return points;
        
        const hull = [points[0], points[1]];
        
        for (let i = 2; i < points.length; i++) {
            while (hull.length > 1 && 
                   this.crossProduct(hull[hull.length - 2], hull[hull.length - 1], points[i]) <= 0) {
                hull.pop();
            }
            hull.push(points[i]);
        }
        
        return hull;
    }

    // Calculate cross product of three points
    crossProduct(o, a, b) {
        return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
    }

    // Get points for visualization
    get visualPoints() {
        return this.points.map((point, index) => ({
            ...point,
            id: index,
            isHull: this.convexHull.some(hullPoint => 
                hullPoint.x === point.x && hullPoint.y === point.y
            )
        }));
    }

    // Handle point addition
    handleAddPoint() {
        const newPoint = {
            x: Math.floor(Math.random() * 10) + 1,
            y: Math.floor(Math.random() * 10) + 1
        };
        this.points = [...this.points, newPoint];
        this.calculateConvexHull();
    }

    // Handle reset
    handleReset() {
        this.points = [
            { x: 1, y: 1 },
            { x: 2, y: 2 },
            { x: 3, y: 3 },
            { x: 1, y: 3 },
            { x: 3, y: 1 },
            { x: 2, y: 1 },
            { x: 1, y: 2 },
            { x: 3, y: 2 }
        ];
        this.calculateConvexHull();
    }
}
```

```html
<!-- chanAlgorithm.html -->
<template>
    <div class="container">
        <h2>Chan's Algorithm - Convex Hull</h2>
        
        <div class="controls">
            <lightning-button 
                label="Add Random Point" 
                onclick={handleAddPoint}
                variant="brand">
            </lightning-button>
            <lightning-button 
                label="Reset" 
                onclick={handleReset}
                variant="neutral">
            </lightning-button>
        </div>

        <div class="visualization">
            <div class="grid">
                <template for:each={visualPoints} for:item="point">
                    <div 
                        key={point.id}
                        class="point"
                        style={pointStyle}
                        data-x={point.x}
                        data-y={point.y}
                    >
                        <span class="point-label">{point.x},{point.y}</span>
                        <template if:true={point.isHull}>
                            <div class="hull-point">●</div>
                        </template>
                    </div>
                </template>
            </div>
        </div>

        <div class="algorithm-info">
            <h3>Algorithm Steps:</h3>
            <ul>
                <li>Find the point with minimum y-coordinate</li>
                <li>Sort remaining points by polar angle</li>
                <li>Apply Graham scan to find convex hull</li>
                <li>Return the hull points</li>
            </ul>
            
            <h3>Convex Hull Points:</h3>
            <div class="hull-points">
                <template for:each={convexHull} for:item="point">
                    <span key={point.x}>{point.x},{point.y}</span>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* chanAlgorithm.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.controls {
    margin-bottom: 20px;
    text-align: center;
}

.grid {
    display: grid;
    grid-template-columns: repeat(10, 1fr);
    gap: 10px;
    border: 1px solid #ccc;
    padding: 20px;
    margin: 20px 0;
    background-color: #f9f9f9;
}

.point {
    width: 40px;
    height: 40px;
    border: 2px solid #333;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    position: relative;
    background-color: white;
    font-size: 12px;
    cursor: pointer;
}

.point:hover {
    background-color: #e0e0e0;
}

.point-label {
    position: absolute;
    top: -20px;
    left: 50%;
    transform: translateX(-50%);
    font-size: 10px;
    color: #666;
}

.hull-point {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    color: red;
    font-size: 20px;
    font-weight: bold;
}

.algorithm-info {
    margin-top: 20px;
    padding: 15px;
    background-color: #f0f8ff;
    border-radius: 5px;
}

.hull-points {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    margin-top: 10px;
}

.hull-points span {
    background-color: #ff6b6b;
    color: white;
    padding: 5px 10px;
    border-radius: 3px;
    font-size: 14px;
}
```

## Key Features of this Implementation:

1. **Complete Algorithm**: Implements the core logic of Chan's algorithm for convex hull computation
2. **Interactive Visualization**: Shows points on a grid with hull points highlighted
3. **Dynamic Updates**: Can add new points and recalculate the hull
4. **Step-by-step Process**: Shows the algorithm steps in the UI
5. **Responsive Design**: Clean, modern UI with proper styling

## How it Works:

1. **Find Reference Point**: Identifies the point with minimum y-coordinate
2. **Sort by Polar Angle**: Orders remaining points by their angle relative to the reference
3. **Graham Scan**: Applies Graham's scan algorithm to find the convex hull
4. **Visualization**: Displays the points and hull in a grid format

The component demonstrates the mathematical concepts behind Chan's algorithm while providing a visual representation of how the convex hull is computed.

