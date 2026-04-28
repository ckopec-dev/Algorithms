# Monte Carlo Method in Lightning Web Component

Here's an example of implementing a Monte Carlo simulation to estimate π using Lightning Web Components:

```javascript
// monteCarloPi.js
import { LightningElement } from 'lwc';

export default class MonteCarloPi extends LightningElement {
    piEstimate = 0;
    totalPoints = 0;
    pointsInCircle = 0;
    isRunning = false;
    iterations = 10000;

    handleStart() {
        this.isRunning = true;
        this.piEstimate = 0;
        this.totalPoints = 0;
        this.pointsInCircle = 0;
        
        // Start the Monte Carlo simulation
        this.runMonteCarlo();
    }

    handleStop() {
        this.isRunning = false;
    }

    handleReset() {
        this.isRunning = false;
        this.piEstimate = 0;
        this.totalPoints = 0;
        this.pointsInCircle = 0;
    }

    async runMonteCarlo() {
        if (!this.isRunning) return;

        const batchSize = 1000;
        let batchPoints = 0;

        // Process points in batches to avoid blocking the UI
        const processBatch = () => {
            if (!this.isRunning) return;

            for (let i = 0; i < batchSize && this.isRunning; i++) {
                this.totalPoints++;
                batchPoints++;
                
                // Generate random point in unit square [0,1] x [0,1]
                const x = Math.random();
                const y = Math.random();
                
                // Check if point is inside unit circle
                if (x * x + y * y <= 1) {
                    this.pointsInCircle++;
                }
            }

            // Update estimate every batch
            if (this.totalPoints > 0) {
                this.piEstimate = 4 * (this.pointsInCircle / this.totalPoints);
            }

            // Continue processing if we haven't reached the target
            if (this.totalPoints < this.iterations) {
                setTimeout(processBatch, 0);
            } else {
                this.isRunning = false;
            }
        };

        processBatch();
    }

    get progressPercentage() {
        return (this.totalPoints / this.iterations) * 100;
    }

    get isComplete() {
        return this.totalPoints >= this.iterations;
    }
}
```

```html
<!-- monteCarloPi.html -->
<template>
    <div class="container">
        <h2>Monte Carlo π Estimation</h2>
        
        <div class="controls">
            <lightning-button 
                label="Start" 
                variant="brand" 
                onclick={handleStart}
                disabled={isRunning}>
            </lightning-button>
            
            <lightning-button 
                label="Stop" 
                variant="destructive" 
                onclick={handleStop}
                disabled={!isRunning}>
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <div class="stats">
            <div class="stat-item">
                <span class="label">Total Points:</span>
                <span class="value">{totalPoints}</span>
            </div>
            
            <div class="stat-item">
                <span class="label">Points in Circle:</span>
                <span class="value">{pointsInCircle}</span>
            </div>
            
            <div class="stat-item">
                <span class="label">π Estimate:</span>
                <span class="value">{piEstimate}</span>
            </div>
            
            <div class="stat-item">
                <span class="label">Accuracy:</span>
                <span class="value">{(Math.abs(piEstimate - Math.PI) / Math.PI * 100).toFixed(4)}%</span>
            </div>
        </div>

        <div class="progress-container">
            <lightning-progress-bar 
                value={progressPercentage}
                label="Progress"
                size="medium">
            </lightning-progress-bar>
            <p>Progress: {progressPercentage.toFixed(2)}%</p>
        </div>

        <div class="info">
            <p>This simulation estimates π using the Monte Carlo method.</p>
            <p>Random points are generated in a unit square, and the ratio of points inside the unit circle to total points is used to estimate π.</p>
            <p>Formula: π ≈ 4 × (points inside circle / total points)</p>
        </div>
    </div>
</template>
```

```css
/* monteCarloPi.css */
.container {
    padding: 20px;
    max-width: 600px;
    margin: 0 auto;
}

.controls {
    display: flex;
    gap: 10px;
    margin-bottom: 20px;
    flex-wrap: wrap;
}

.stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 15px;
    margin-bottom: 20px;
}

.stat-item {
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 8px;
    text-align: center;
}

.label {
    display: block;
    font-weight: bold;
    color: #333;
    margin-bottom: 5px;
}

.value {
    display: block;
    font-size: 1.2em;
    font-weight: bold;
    color: #0070d2;
}

.progress-container {
    margin-bottom: 20px;
}

.info {
    background-color: #e8f5fe;
    padding: 15px;
    border-radius: 8px;
    border-left: 4px solid #0070d2;
}

.info p {
    margin: 5px 0;
    line-height: 1.4;
}
```

## How it works:

1. **Monte Carlo Method**: The algorithm generates random points in a unit square and determines how many fall inside a unit circle
2. **π Estimation**: Uses the ratio of points inside the circle to total points: π ≈ 4 × (points inside circle / total points)
3. **Batch Processing**: Processes points in batches to prevent UI blocking
4. **Real-time Updates**: Updates the π estimate and progress as points are generated
5. **Controls**: Start, stop, and reset functionality for the simulation

## Key Features:

- **Asynchronous Processing**: Uses `setTimeout` to process batches without blocking the UI
- **Real-time Updates**: Shows progress and current π estimate
- **Responsive Design**: Adapts to different screen sizes
- **User Controls**: Start, stop, and reset buttons
- **Progress Tracking**: Visual progress bar and percentage indicator

This implementation demonstrates how to perform computationally intensive Monte Carlo simulations in Lightning Web Components while maintaining good user experience.

