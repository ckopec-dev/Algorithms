# Perceptron Learning Algorithm in Lightning Web Component

Here's a complete example of implementing a Perceptron learning algorithm in a Lightning Web Component:

## HTML Template (perceptron.html)
```html
<template>
    <div class="slds-box slds-box_small slds-theme_default">
        <h2>Perceptron Learning Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Training Data">
                    <div class="slds-p-around_small">
                        <lightning-datatable
                            data={trainingData}
                            columns={columns}
                            key-field="id"
                            hide-checkbox-column="true">
                        </lightning-datatable>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Results">
                    <div class="slds-p-around_small">
                        <p><strong>Weights:</strong> {weightsString}</p>
                        <p><strong>Bias:</strong> {bias}</p>
                        <p><strong>Iterations:</strong> {iterations}</p>
                        <p><strong>Accuracy:</strong> {accuracy}%</p>
                    </div>
                </lightning-card>
            </div>
        </div>
        
        <div class="slds-p-around_small">
            <lightning-button 
                label="Train Perceptron" 
                variant="brand" 
                onclick={handleTrain}
                disabled={isTraining}>
            </lightning-button>
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}
                class="slds-m-left_x-small">
            </lightning-button>
        </div>
        
        <div class="slds-p-around_small">
            <lightning-card title="Prediction">
                <div class="slds-p-around_small">
                    <lightning-input 
                        label="Input X1" 
                        type="number" 
                        value={inputX1} 
                        onchange={handleInputX1Change}>
                    </lightning-input>
                    <lightning-input 
                        label="Input X2" 
                        type="number" 
                        value={inputX2} 
                        onchange={handleInputX2Change}>
                    </lightning-input>
                    <lightning-button 
                        label="Predict" 
                        variant="success" 
                        onclick={handlePredict}
                        class="slds-m-top_small">
                    </lightning-button>
                    <p class="slds-p-top_small">
                        <strong>Prediction:</strong> {prediction}
                    </p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (perceptron.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class Perceptron extends LightningElement {
    @track trainingData = [
        { id: 1, x1: 0, x2: 0, y: 0 },
        { id: 2, x1: 0, x2: 1, y: 1 },
        { id: 3, x1: 1, x2: 0, y: 1 },
        { id: 4, x1: 1, x2: 1, y: 1 }
    ];
    
    @track weights = [0, 0];
    @track bias = 0;
    @track iterations = 0;
    @track accuracy = 0;
    @track isTraining = false;
    
    @track inputX1 = 0;
    @track inputX2 = 0;
    @track prediction = '';
    
    columns = [
        { label: 'X1', fieldName: 'x1', type: 'number' },
        { label: 'X2', fieldName: 'x2', type: 'number' },
        { label: 'Y', fieldName: 'y', type: 'number' }
    ];
    
    get weightsString() {
        return `W1: ${this.weights[0].toFixed(2)}, W2: ${this.weights[1].toFixed(2)}`;
    }
    
    handleTrain() {
        this.isTraining = true;
        this.iterations = 0;
        this.accuracy = 0;
        
        // Initialize weights and bias
        this.weights = [0, 0];
        this.bias = 0;
        
        // Train the perceptron
        this.trainPerceptron();
        
        this.isTraining = false;
    }
    
    trainPerceptron() {
        const learningRate = 0.1;
        const maxIterations = 1000;
        let converged = false;
        let iteration = 0;
        
        while (!converged && iteration < maxIterations) {
            let errors = 0;
            
            // For each training example
            for (let i = 0; i < this.trainingData.length; i++) {
                const data = this.trainingData[i];
                const x1 = data.x1;
                const x2 = data.x2;
                const target = data.y;
                
                // Calculate prediction
                const weightedSum = this.weights[0] * x1 + this.weights[1] * x2 + this.bias;
                const prediction = this.stepFunction(weightedSum);
                
                // Calculate error
                const error = target - prediction;
                
                // Update weights and bias
                if (error !== 0) {
                    this.weights[0] += learningRate * error * x1;
                    this.weights[1] += learningRate * error * x2;
                    this.bias += learningRate * error;
                    errors++;
                }
            }
            
            iteration++;
            this.iterations = iteration;
            
            // Check if converged
            if (errors === 0) {
                converged = true;
            }
        }
        
        // Calculate accuracy
        this.calculateAccuracy();
    }
    
    stepFunction(sum) {
        return sum >= 0 ? 1 : 0;
    }
    
    calculateAccuracy() {
        let correct = 0;
        let total = this.trainingData.length;
        
        for (let i = 0; i < this.trainingData.length; i++) {
            const data = this.trainingData[i];
            const x1 = data.x1;
            const x2 = data.x2;
            const target = data.y;
            
            const weightedSum = this.weights[0] * x1 + this.weights[1] * x2 + this.bias;
            const prediction = this.stepFunction(weightedSum);
            
            if (prediction === target) {
                correct++;
            }
        }
        
        this.accuracy = Math.round((correct / total) * 100);
    }
    
    handlePredict() {
        const x1 = parseFloat(this.inputX1);
        const x2 = parseFloat(this.inputX2);
        
        if (isNaN(x1) || isNaN(x2)) {
            this.prediction = 'Please enter valid numbers';
            return;
        }
        
        const weightedSum = this.weights[0] * x1 + this.weights[1] * x2 + this.bias;
        const prediction = this.stepFunction(weightedSum);
        
        this.prediction = `Output: ${prediction}`;
    }
    
    handleReset() {
        this.weights = [0, 0];
        this.bias = 0;
        this.iterations = 0;
        this.accuracy = 0;
        this.inputX1 = 0;
        this.inputX2 = 0;
        this.prediction = '';
    }
    
    handleInputX1Change(event) {
        this.inputX1 = event.target.value;
    }
    
    handleInputX2Change(event) {
        this.inputX2 = event.target.value;
    }
}
```

## CSS Styles (perceptron.css)
```css
.slds-box {
    margin: 1rem 0;
}

.slds-p-around_small {
    padding: 0.5rem;
}

.slds-m-top_small {
    margin-top: 0.5rem;
}

.slds-m-left_x-small {
    margin-left: 0.5rem;
}
```

## Key Features of this Implementation:

1. **Training Data Display**: Shows the input data in a table format
2. **Weight and Bias Tracking**: Displays current weights and bias values
3. **Training Process**: Implements the perceptron learning algorithm with:
   - Weight updates using gradient descent
   - Bias adjustment
   - Convergence detection
4. **Accuracy Calculation**: Shows training accuracy after completion
5. **Prediction Capability**: Allows user to test the trained model with new inputs
6. **Reset Functionality**: Resets the perceptron to initial state
7. **Responsive Design**: Uses Lightning Design System components

## How it Works:

1. The perceptron is trained on a simple AND gate dataset
2. During training, weights and bias are updated using the perceptron learning rule
3. The learning rate controls how quickly the weights change
4. Training continues until all examples are classified correctly or max iterations reached
5. Users can make predictions with new input values after training

This implementation demonstrates the core concepts of perceptron learning in a user-friendly LWC interface.

