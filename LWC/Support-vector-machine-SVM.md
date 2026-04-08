# Support Vector Machine (SVM) in Lightning Web Component

Here's a complete example of implementing SVM in a Lightning Web Component for machine learning classification:

## HTML Template (svm-component.html)

```html
<template>
    <div class="svm-container">
        <lightning-card title="Support Vector Machine Classifier" icon-name="custom:custom63">
            <div class="svm-content">
                <div class="svm-controls">
                    <lightning-button 
                        label="Train Model" 
                        variant="brand" 
                        onclick={handleTrainModel}
                        disabled={isTraining}>
                    </lightning-button>
                    <lightning-button 
                        label="Predict" 
                        variant="success" 
                        onclick={handlePredict}
                        disabled={isPredicting}>
                    </lightning-button>
                    <lightning-button 
                        label="Reset" 
                        variant="neutral" 
                        onclick={handleReset}>
                    </lightning-button>
                </div>

                <div class="svm-data-input">
                    <lightning-input 
                        label="Feature 1 (X1)" 
                        type="number" 
                        value={feature1}
                        onchange={handleFeature1Change}>
                    </lightning-input>
                    <lightning-input 
                        label="Feature 2 (X2)" 
                        type="number" 
                        value={feature2}
                        onchange={handleFeature2Change}>
                    </lightning-input>
                </div>

                <div class="svm-results">
                    <lightning-output-field 
                        label="Prediction Result" 
                        value={predictionResult}>
                    </lightning-output-field>
                    <lightning-output-field 
                        label="Confidence Score" 
                        value={confidenceScore}>
                    </lightning-output-field>
                </div>

                <div class="svm-model-info">
                    <h3>Model Information</h3>
                    <p>Support Vectors: {supportVectorsCount}</p>
                    <p>Kernel Type: {kernelType}</p>
                    <p>Regularization Parameter: {regularizationParameter}</p>
                </div>
            </div>
        </lightning-card>
    </div>
</template>
```

## JavaScript Controller (svm-component.js)

```javascript
import { LightningElement, track } from 'lwc';

export default class SvmComponent extends LightningElement {
    @track feature1 = '';
    @track feature2 = '';
    @track predictionResult = '';
    @track confidenceScore = '';
    @track isTraining = false;
    @track isPredicting = false;
    
    // SVM Model parameters
    @track supportVectorsCount = 0;
    @track kernelType = 'rbf';
    @track regularizationParameter = 1.0;
    @track modelTrained = false;
    
    // Sample training data (in real implementation, this would come from Apex)
    trainingData = [
        { x1: 1.0, x2: 2.0, label: 0 },
        { x1: 2.0, x2: 3.0, label: 0 },
        { x1: 3.0, x2: 1.0, label: 0 },
        { x1: 4.0, x2: 2.0, label: 1 },
        { x1: 5.0, x2: 3.0, label: 1 },
        { x1: 6.0, x2: 4.0, label: 1 }
    ];
    
    // Support vectors (simplified for demo)
    supportVectors = [
        { x1: 2.0, x2: 3.0, label: 0 },
        { x1: 5.0, x2: 3.0, label: 1 }
    ];

    handleTrainModel() {
        this.isTraining = true;
        this.modelTrained = true;
        
        // Simulate training process
        setTimeout(() => {
            this.supportVectorsCount = this.supportVectors.length;
            this.isTraining = false;
            this.showToast('Success', 'Model trained successfully!', 'success');
        }, 1500);
    }

    handlePredict() {
        if (!this.modelTrained) {
            this.showToast('Error', 'Please train the model first!', 'error');
            return;
        }

        if (!this.feature1 || !this.feature2) {
            this.showToast('Error', 'Please enter both features!', 'error');
            return;
        }

        this.isPredicting = true;
        
        // Simulate prediction process
        setTimeout(() => {
            const prediction = this.svmPredict(
                parseFloat(this.feature1), 
                parseFloat(this.feature2)
            );
            
            this.predictionResult = prediction.label;
            this.confidenceScore = prediction.confidence.toFixed(4);
            this.isPredicting = false;
        }, 1000);
    }

    handleReset() {
        this.feature1 = '';
        this.feature2 = '';
        this.predictionResult = '';
        this.confidenceScore = '';
        this.modelTrained = false;
    }

    handleFeature1Change(event) {
        this.feature1 = event.target.value;
    }

    handleFeature2Change(event) {
        this.feature2 = event.target.value;
    }

    // SVM Prediction Logic
    svmPredict(x1, x2) {
        // Simplified SVM prediction algorithm
        // In real implementation, this would use actual SVM kernel calculations
        
        // Calculate distance from support vectors
        let minDistance = Infinity;
        let predictedLabel = 0;
        let maxDistance = -Infinity;
        
        this.supportVectors.forEach(sv => {
            const distance = Math.sqrt(
                Math.pow(x1 - sv.x1, 2) + 
                Math.pow(x2 - sv.x2, 2)
            );
            
            if (distance < minDistance) {
                minDistance = distance;
                predictedLabel = sv.label;
            }
            
            maxDistance = Math.max(maxDistance, distance);
        });
        
        // Calculate confidence (inverse of distance)
        const confidence = maxDistance > 0 ? 
            (1 - (minDistance / maxDistance)) : 0.5;
        
        return {
            label: predictedLabel,
            confidence: confidence
        };
    }

    showToast(title, message, variant) {
        const event = new ShowToastEvent({
            title: title,
            message: message,
            variant: variant
        });
        this.dispatchEvent(event);
    }
}
```

## CSS Styling (svm-component.css)

```css
.svm-container {
    padding: 1rem;
    max-width: 800px;
    margin: 0 auto;
}

.svm-content {
    padding: 1rem;
}

.svm-controls {
    display: flex;
    gap: 1rem;
    margin-bottom: 1.5rem;
    flex-wrap: wrap;
}

.svm-data-input {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1rem;
    margin-bottom: 1.5rem;
}

.svm-results {
    margin-bottom: 1.5rem;
    padding: 1rem;
    background-color: #f8f9fa;
    border-radius: 0.5rem;
}

.svm-model-info {
    padding: 1rem;
    background-color: #e9ecef;
    border-radius: 0.5rem;
    margin-top: 1rem;
}

.svm-model-info h3 {
    margin-top: 0;
    color: #495057;
}

.svm-model-info p {
    margin: 0.25rem 0;
    color: #6c757d;
}

@media (max-width: 768px) {
    .svm-data-input {
        grid-template-columns: 1fr;
    }
    
    .svm-controls {
        justify-content: center;
    }
}
```

## Key Features of this SVM Implementation:

1. **Training Process**: Simulates SVM model training with support vector counting
2. **Prediction**: Takes user input features and predicts class labels
3. **Confidence Scoring**: Provides confidence levels for predictions
4. **Model Information**: Displays SVM parameters and statistics
5. **Responsive Design**: Works on mobile and desktop devices
6. **User Feedback**: Toast notifications for user actions

## How it Works:

1. **Training**: Click "Train Model" to initialize the SVM classifier
2. **Input**: Enter feature values (X1, X2) in the input fields
3. **Prediction**: Click "Predict" to classify the input data
4. **Results**: View prediction result and confidence score

This example demonstrates the core concepts of SVM classification in a Lightning Web Component, though in a production environment, you would integrate with Apex classes that contain actual SVM algorithms or use external libraries.

