# SVM Algorithm in Lightning Web Component

Here's an example of implementing a Support Vector Machine (SVM) algorithm in a Lightning Web Component using JavaScript:

```javascript
// svm.js
import { LightningElement, track } from 'lwc';

export default class SvmExample extends LightningElement {
    @track data = [
        { x: 1, y: 2, label: 1 },
        { x: 2, y: 3, label: 1 },
        { x: 3, y: 1, label: 1 },
        { x: 4, y: 2, label: -1 },
        { x: 5, y: 3, label: -1 },
        { x: 6, y: 1, label: -1 }
    ];

    @track svmModel = {
        weights: [0, 0],
        bias: 0,
        supportVectors: []
    };

    @track prediction = null;
    @track isTrained = false;

    // Simple SVM implementation
    trainSVM() {
        // This is a simplified version - real SVM would use optimization algorithms
        const X = this.data.map(point => [point.x, point.y]);
        const y = this.data.map(point => point.label);
        
        // Calculate basic SVM parameters (simplified approach)
        const weights = this.calculateWeights(X, y);
        const bias = this.calculateBias(X, y, weights);
        
        this.svmModel = {
            weights: weights,
            bias: bias,
            supportVectors: this.findSupportVectors(X, y, weights, bias)
        };
        
        this.isTrained = true;
    }

    calculateWeights(X, y) {
        // Simplified weight calculation
        // In practice, this would use optimization algorithms like Sequential Minimal Optimization
        let w1 = 0, w2 = 0;
        let count1 = 0, count2 = 0;
        
        for (let i = 0; i < X.length; i++) {
            if (y[i] === 1) {
                w1 += X[i][0];
                w2 += X[i][1];
                count1++;
            } else {
                w1 -= X[i][0];
                w2 -= X[i][1];
                count2++;
            }
        }
        
        return [w1 / count1, w2 / count2];
    }

    calculateBias(X, y, weights) {
        // Simplified bias calculation
        let sum = 0;
        let count = 0;
        
        for (let i = 0; i < X.length; i++) {
            const dotProduct = weights[0] * X[i][0] + weights[1] * X[i][1];
            sum += y[i] - dotProduct;
            count++;
        }
        
        return sum / count;
    }

    findSupportVectors(X, y, weights, bias) {
        // Find support vectors (points closest to decision boundary)
        const distances = [];
        
        for (let i = 0; i < X.length; i++) {
            const dotProduct = weights[0] * X[i][0] + weights[1] * X[i][1] + bias;
            const distance = Math.abs(dotProduct);
            distances.push({ index: i, distance: distance });
        }
        
        // Sort by distance and return top 3
        distances.sort((a, b) => a.distance - b.distance);
        return distances.slice(0, 3).map(item => this.data[item.index]);
    }

    predict(x, y) {
        if (!this.isTrained) {
            this.trainSVM();
        }
        
        const prediction = this.svmModel.weights[0] * x + 
                          this.svmModel.weights[1] * y + 
                          this.svmModel.bias;
        
        this.prediction = prediction > 0 ? 1 : -1;
        return this.prediction;
    }

    handlePredict() {
        const xInput = this.template.querySelector('[data-id="x-input"]');
        const yInput = this.template.querySelector('[data-id="y-input"]');
        
        if (xInput && yInput) {
            const x = parseFloat(xInput.value);
            const y = parseFloat(yInput.value);
            
            if (!isNaN(x) && !isNaN(y)) {
                this.predict(x, y);
            }
        }
    }

    handleTrain() {
        this.trainSVM();
    }

    get supportVectorData() {
        return this.svmModel.supportVectors.map((sv, index) => ({
            ...sv,
            id: index
        }));
    }

    get decisionBoundary() {
        if (!this.isTrained) return '';
        
        // Generate decision boundary line
        const weights = this.svmModel.weights;
        const bias = this.svmModel.bias;
        
        // For visualization purposes - simple line equation
        return `y = ${-weights[0]/weights[1]}x + ${-bias/weights[1]}`;
    }
}
```

```html
<!-- svm.html -->
<template>
    <div class="svm-container">
        <lightning-card title="Support Vector Machine Example" icon-name="custom:custom14">
            <div class="svm-content">
                <div class="controls">
                    <lightning-button 
                        label="Train SVM" 
                        variant="brand" 
                        onclick={handleTrain}
                        disabled={isTrained}>
                    </lightning-button>
                    
                    <lightning-button 
                        label="Predict" 
                        variant="success" 
                        onclick={handlePredict}>
                    </lightning-button>
                </div>

                <div class="input-section">
                    <lightning-input 
                        label="X coordinate" 
                        type="number" 
                        data-id="x-input"
                        value="3">
                    </lightning-input>
                    <lightning-input 
                        label="Y coordinate" 
                        type="number" 
                        data-id="y-input"
                        value="2">
                    </lightning-input>
                </div>

                <div class="results">
                    <template if:true={isTrained}>
                        <div class="model-info">
                            <h3>Model Parameters</h3>
                            <p>Weights: [{svmModel.weights[0].toFixed(2)}, {svmModel.weights[1].toFixed(2)}]</p>
                            <p>Bias: {svmModel.bias.toFixed(2)}</p>
                            <p>Decision Boundary: {decisionBoundary}</p>
                        </div>

                        <div class="prediction">
                            <h3>Prediction Result</h3>
                            <p>Class: {prediction}</p>
                        </div>

                        <div class="support-vectors">
                            <h3>Support Vectors</h3>
                            <template for:each={supportVectorData} for:item="sv">
                                <div key={sv.id} class="sv-item">
                                    <p>Point ({sv.x}, {sv.y}): Class {sv.label}</p>
                                </div>
                            </template>
                        </div>
                    </template>

                    <template if:false={isTrained}>
                        <p>Click "Train SVM" to train the model with sample data</p>
                    </template>
                </div>
            </div>
        </lightning-card>
    </div>
</template>
```

```css
/* svm.css */
.svm-container {
    padding: 1rem;
}

.svm-content {
    margin: 1rem 0;
}

.controls {
    margin-bottom: 1rem;
    display: flex;
    gap: 1rem;
}

.input-section {
    margin: 1rem 0;
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
}

.results {
    margin-top: 1rem;
    border-top: 1px solid #e5e5e5;
    padding-top: 1rem;
}

.model-info {
    background-color: #f0f8ff;
    padding: 1rem;
    border-radius: 0.5rem;
    margin-bottom: 1rem;
}

.prediction {
    background-color: #e8f5e9;
    padding: 1rem;
    border-radius: 0.5rem;
    margin-bottom: 1rem;
}

.support-vectors {
    background-color: #fff3e0;
    padding: 1rem;
    border-radius: 0.5rem;
}

.sv-item {
    padding: 0.5rem;
    border-bottom: 1px solid #ddd;
}

.sv-item:last-child {
    border-bottom: none;
}

.lightning-card {
    margin-bottom: 1rem;
}
```

## Key Features of this SVM Implementation:

1. **Data Handling**: Processes 2D data points with class labels
2. **Training**: Simplified SVM training algorithm
3. **Prediction**: Makes predictions for new data points
4. **Visualization**: Shows support vectors and decision boundary
5. **User Interface**: Interactive controls for training and prediction

## How it works:

1. **Data Input**: Sample 2D points with class labels (1 or -1)
2. **Training**: Calculates weights and bias for the decision boundary
3. **Prediction**: Uses the trained model to classify new points
4. **Support Vectors**: Identifies the most important data points
5. **Visualization**: Displays model parameters and results

## Note:

This is a simplified educational implementation. A production-ready SVM would use:
- Proper optimization algorithms (SMO, etc.)
- Kernel functions (RBF, polynomial, etc.)
- Cross-validation
- More sophisticated parameter tuning
- Better numerical stability

The LWC component provides a clean UI for interacting with the SVM algorithm while demonstrating core machine learning concepts.

