# Bezier Curve Generation in Lightning Web Component

Here's a complete example of a Bezier curve generation algorithm implemented in Lightning Web Component:

```javascript
// bezier-curve.js
import { LightningElement, api, track } from 'lwc';

export default class BezierCurve extends LightningElement {
    @track points = [];
    @track curvePoints = [];
    @track isDrawing = false;
    
    // Control points for the Bezier curve
    @api controlPoints = [
        { x: 50, y: 200 },
        { x: 150, y: 50 },
        { x: 250, y: 300 },
        { x: 350, y: 100 }
    ];

    connectedCallback() {
        this.generateBezierCurve();
    }

    // Generate Bezier curve points
    generateBezierCurve() {
        const points = this.controlPoints;
        const curvePoints = [];
        
        if (points.length < 2) {
            return;
        }

        // For cubic Bezier curve (4 control points)
        if (points.length >= 4) {
            const steps = 100;
            for (let i = 0; i <= steps; i++) {
                const t = i / steps;
                const point = this.cubicBezier(points, t);
                curvePoints.push(point);
            }
        } else {
            // For linear interpolation between two points
            const steps = 100;
            for (let i = 0; i <= steps; i++) {
                const t = i / steps;
                const point = this.linearInterpolation(points[0], points[1], t);
                curvePoints.push(point);
            }
        }

        this.curvePoints = curvePoints;
    }

    // Cubic Bezier curve calculation
    cubicBezier(points, t) {
        const [p0, p1, p2, p3] = points;
        
        // Bezier curve formula: B(t) = (1-t)³P0 + 3(1-t)²tP1 + 3(1-t)t²P2 + t³P3
        const x = Math.pow(1 - t, 3) * p0.x +
                  3 * Math.pow(1 - t, 2) * t * p1.x +
                  3 * (1 - t) * Math.pow(t, 2) * p2.x +
                  Math.pow(t, 3) * p3.x;
                  
        const y = Math.pow(1 - t, 3) * p0.y +
                  3 * Math.pow(1 - t, 2) * t * p1.y +
                  3 * (1 - t) * Math.pow(t, 2) * p2.y +
                  Math.pow(t, 3) * p3.y;
        
        return { x, y };
    }

    // Linear interpolation between two points
    linearInterpolation(p1, p2, t) {
        const x = p1.x + (p2.x - p1.x) * t;
        const y = p1.y + (p2.y - p1.y) * t;
        return { x, y };
    }

    // Add a new control point
    handleAddPoint() {
        const newPoint = { 
            x: this.controlPoints.length * 100 + 50, 
            y: Math.random() * 200 + 50 
        };
        this.controlPoints = [...this.controlPoints, newPoint];
        this.generateBezierCurve();
    }

    // Update control point position
    handlePointUpdate(event) {
        const index = event.target.dataset.index;
        const point = this.controlPoints[index];
        const updatedPoint = {
            ...point,
            x: parseInt(event.target.value),
            y: parseInt(event.target.dataset.y)
        };
        
        this.controlPoints[index] = updatedPoint;
        this.generateBezierCurve();
    }

    // Get SVG path data for the curve
    get curvePath() {
        if (this.curvePoints.length === 0) return '';
        
        const firstPoint = this.curvePoints[0];
        let path = `M ${firstPoint.x} ${firstPoint.y}`;
        
        for (let i = 1; i < this.curvePoints.length; i++) {
            const point = this.curvePoints[i];
            path += ` L ${point.x} ${point.y}`;
        }
        
        return path;
    }

    // Get SVG path data for control points
    get controlPointPath() {
        if (this.controlPoints.length === 0) return '';
        
        let path = '';
        this.controlPoints.forEach((point, index) => {
            if (index === 0) {
                path += `M ${point.x} ${point.y}`;
            } else {
                path += ` L ${point.x} ${point.y}`;
            }
        });
        
        return path;
    }

    // Get control point circles for SVG
    get controlPointCircles() {
        return this.controlPoints.map((point, index) => ({
            cx: point.x,
            cy: point.y,
            index: index
        }));
    }

    // Get curve points for SVG
    get curvePointsForSVG() {
        return this.curvePoints.map(point => ({
            cx: point.x,
            cy: point.y
        }));
    }
}
```

```html
<!-- bezier-curve.html -->
<template>
    <div class="container">
        <h2>Bezier Curve Generator</h2>
        
        <div class="controls">
            <lightning-button 
                label="Add Point" 
                variant="brand" 
                onclick={handleAddPoint}>
            </lightning-button>
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={resetCurve}>
            </lightning-button>
        </div>

        <div class="svg-container">
            <svg width="600" height="400" class="bezier-svg">
                <!-- Control point lines -->
                <path 
                    d={controlPointPath} 
                    stroke="gray" 
                    stroke-width="1" 
                    fill="none" 
                    stroke-dasharray="5,5">
                </path>
                
                <!-- Bezier curve -->
                <path 
                    d={curvePath} 
                    stroke="blue" 
                    stroke-width="2" 
                    fill="none">
                </path>
                
                <!-- Control points -->
                <template for:each={controlPointCircles} for:item="point">
                    <circle 
                        key={point.index}
                        cx={point.cx}
                        cy={point.cy}
                        r="5"
                        fill="red"
                        stroke="black"
                        stroke-width="1">
                    </circle>
                </template>
                
                <!-- Curve points -->
                <template for:each={curvePointsForSVG} for:item="point">
                    <circle 
                        key={point.cy}
                        cx={point.cx}
                        cy={point.cy}
                        r="1"
                        fill="blue">
                    </circle>
                </template>
            </svg>
        </div>

        <div class="control-points">
            <h3>Control Points:</h3>
            <template for:each={controlPoints} for:item="point" for:index="index">
                <div key={index} class="point-input">
                    <span>Point {index + 1}:</span>
                    <lightning-input 
                        type="number" 
                        label="X" 
                        value={point.x}
                        data-index={index}
                        data-y="x"
                        onchange={handlePointUpdate}>
                    </lightning-input>
                    <lightning-input 
                        type="number" 
                        label="Y" 
                        value={point.y}
                        data-index={index}
                        data-y="y"
                        onchange={handlePointUpdate}>
                    </lightning-input>
                </div>
            </template>
        </div>
    </div>
</template>
```

```css
/* bezier-curve.css */
.container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.controls {
    margin-bottom: 20px;
}

.svg-container {
    border: 1px solid #ccc;
    margin-bottom: 20px;
    background-color: #f9f9f9;
}

.bezier-svg {
    background-color: white;
}

.control-points {
    margin-top: 20px;
}

.point-input {
    display: flex;
    align-items: center;
    margin-bottom: 10px;
    gap: 10px;
}

.point-input span {
    min-width: 60px;
    font-weight: bold;
}

.point-input lightning-input {
    width: 100px;
}
```

## Key Features of this Implementation:

1. **Cubic Bezier Curve Calculation**: Uses the mathematical formula for cubic Bezier curves
2. **Interactive Control Points**: Users can modify control point positions
3. **Real-time Updates**: Curve updates immediately when control points change
4. **Visual Feedback**: Shows both control points and the generated curve
5. **Responsive Design**: Clean, user-friendly interface with input controls

## How it Works:

1. **Mathematical Foundation**: Implements the cubic Bezier curve formula
2. **Dynamic Generation**: Recalculates curve points whenever control points change
3. **SVG Visualization**: Renders both control point connections and the final curve
4. **User Interaction**: Allows adding points and modifying existing ones

The algorithm uses the standard Bezier curve mathematical formula where each point on the curve is calculated using the Bernstein polynomial basis functions.

