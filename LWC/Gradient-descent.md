# Gradient Descent in Lightning Web Component

Here's an example of implementing gradient descent algorithm in Lightning Web Component:

```javascript
// gradientDescent.js
import { LightningElement } from 'lwc';

export default class GradientDescent extends LightningElement {
    // Input data
    inputX = [1, 2, 3, 4, 5];
    inputY = [2, 4, 6, 8, 10];
    
    // Algorithm parameters
    learningRate = 0.01;
    iterations = 1000;
    tolerance = 0.0001;
    
    // Results
    slope = 0;
    intercept = 0;
    costHistory = [];
    
    // Initialize the algorithm
    connectedCallback() {
        this.performGradientDescent();
    }
    
    performGradientDescent() {
        // Initialize parameters
        let m = 0; // slope
        let b = 0; // intercept
        let cost = 0;
        
        // Store cost history for visualization
        this.costHistory = [];
        
        // Gradient descent loop
        for (let i = 0; i < this.iterations; i++) {
            // Calculate predictions
            let predictions = this.inputX.map(x => m * x + b);
            
            // Calculate cost (Mean Squared Error)
            cost = this.calculateCost(this.inputY, predictions);
            this.costHistory.push(cost);
            
            // Calculate gradients
            let dm = 0; // gradient for slope
            let db = 0; // gradient for intercept
            
            for (let j = 0; j < this.inputX.length; j++) {
                let error = predictions[j] - this.inputY[j];
                dm += (2 / this.inputX.length) * error * this.inputX[j];
                db += (2 / this.inputX.length) * error;
            }
            
            // Update parameters
            m = m - this.learningRate * dm;
            b = b - this.learningRate * db;
            
            // Check for convergence
            if (Math.abs(dm) < this.tolerance && Math.abs(db) < this.tolerance) {
                console.log(`Converged at iteration ${i}`);
                break;
            }
        }
        
        this.slope = m;
        this.intercept = b;
    }
    
    calculateCost(actual, predicted) {
        let sum = 0;
        for (let i = 0; i < actual.length; i++) {
            sum += Math.pow(actual[i] - predicted[i], 2);
        }
        return sum / (2 * actual.length);
    }
    
    // Get the linear equation
    get equation() {
        return `y = ${this.slope.toFixed(4)}x + ${this.intercept.toFixed(4)}`;
    }
    
    // Get the final cost
    get finalCost() {
        return this.costHistory.length > 0 ? this.costHistory[this.costHistory.length - 1].toFixed(6) : 0;
    }
    
    // Handle button click to re-run algorithm
    handleRunClick() {
        this.performGradientDescent();
    }
}
```

```html
<!-- gradientDescent.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                <div class="slds-media__body">
                    <h2 class="slds-card__header-title slds-truncate" title="Gradient Descent Algorithm">
                        Gradient Descent Algorithm
                    </h2>
                </div>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters slds-wrap">
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-form-element">
                        <div class="slds-form-element__control">
                            <div class="slds-grid slds-gutters">
                                <div class="slds-col slds-size_1-of-2">
                                    <lightning-input 
                                        type="number"
                                        label="Learning Rate"
                                        value={learningRate}
                                        onchange={handleLearningRateChange}>
                                    </lightning-input>
                                </div>
                                <div class="slds-col slds-size_1-of-2">
                                    <lightning-input 
                                        type="number"
                                        label="Iterations"
                                        value={iterations}
                                        onchange={handleIterationsChange}>
                                    </lightning-input>
                                </div>
                            </div>
                        </div>
                    </div>
                    
                    <div class="slds-form-element">
                        <div class="slds-form-element__control">
                            <lightning-button 
                                label="Run Gradient Descent" 
                                onclick={handleRunClick}
                                variant="brand">
                            </lightning-button>
                        </div>
                    </div>
                    
                    <div class="slds-p-around_medium">
                        <h3>Results</h3>
                        <p><strong>Equation:</strong> {equation}</p>
                        <p><strong>Final Cost:</strong> {finalCost}</p>
                        <p><strong>Slope:</strong> {slope}</p>
                        <p><strong>Intercept:</strong> {intercept}</p>
                    </div>
                </div>
                
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-card">
                        <div class="slds-card__header slds-grid">
                            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                                <div class="slds-media__body">
                                    <h3 class="slds-card__header-title slds-truncate" title="Cost Function">Cost Function</h3>
                                </div>
                            </header>
                        </div>
                        <div class="slds-card__body">
                            <canvas id="costChart" width="400" height="300"></canvas>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>
```

```css
/* gradientDescent.css */
.slds-card__header {
    background-color: #f4f6f9;
}

.slds-card__header-title {
    font-size: 1.25rem;
    font-weight: 600;
}

.slds-form-element__control {
    margin-bottom: 1rem;
}
```

## Key Features of this Implementation:

1. **Core Algorithm**: Implements batch gradient descent for linear regression
2. **Interactive Controls**: Allows adjusting learning rate and iterations
3. **Real-time Results**: Displays equation, cost, slope, and intercept
4. **Visualization Ready**: Includes canvas for cost function charting
5. **Convergence Detection**: Stops when gradients become small enough
6. **LWC Best Practices**: Uses proper component structure and lifecycle methods

## How it Works:

1. **Initialization**: Sets up data points and algorithm parameters
2. **Gradient Calculation**: Computes gradients for slope and intercept
3. **Parameter Update**: Updates weights using gradient descent formula
4. **Convergence Check**: Stops when changes become negligible
5. **Results Display**: Shows final linear equation and cost value

This implementation demonstrates how to use gradient descent to find the optimal parameters for a linear regression model within the Lightning Web Component framework.

