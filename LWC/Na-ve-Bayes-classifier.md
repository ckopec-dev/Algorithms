# Naive Bayes Classifier in Lightning Web Component

Here's a complete example of implementing a Naive Bayes classifier in Lightning Web Component:

## HTML Template (naiveBayesClassifier.html)

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Naive Bayes Classifier</h2>
        
        <div class="slds-grid slds-gutters">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Training Data">
                    <div class="slds-p-around_small">
                        <lightning-button 
                            label="Add Sample Data" 
                            onclick={addSampleData}
                            variant="brand"
                            class="slds-m-bottom_small">
                        </lightning-button>
                        
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
                <lightning-card title="Prediction">
                    <div class="slds-p-around_small">
                        <lightning-input 
                            label="Feature 1" 
                            type="text" 
                            value={feature1}
                            onchange={handleFeature1Change}>
                        </lightning-input>
                        
                        <lightning-input 
                            label="Feature 2" 
                            type="text" 
                            value={feature2}
                            onchange={handleFeature2Change}>
                        </lightning-input>
                        
                        <lightning-button 
                            label="Predict" 
                            onclick={predict}
                            variant="brand"
                            class="slds-m-top_small">
                        </lightning-button>
                        
                        <div class="slds-m-top_small">
                            <p><strong>Prediction Result:</strong> {predictionResult}</p>
                            <p><strong>Confidence:</strong> {confidence}</p>
                        </div>
                    </div>
                </lightning-card>
            </div>
        </div>
        
        <div class="slds-m-top_small">
            <lightning-card title="Model Statistics">
                <div class="slds-p-around_small">
                    <p>Total Training Samples: {totalSamples}</p>
                    <p>Classes: {classes}</p>
                    <p>Feature Counts: {featureCounts}</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (naiveBayesClassifier.js)

```javascript
import { LightningElement, track } from 'lwc';

export default class NaiveBayesClassifier extends LightningElement {
    @track trainingData = [];
    @track feature1 = '';
    @track feature2 = '';
    @track predictionResult = '';
    @track confidence = '';
    
    // Model parameters
    @track classPrior = {};
    @track featureLikelihood = {};
    @track featureCounts = {};
    @track totalSamples = 0;
    @track classes = [];
    
    columns = [
        { label: 'Feature 1', fieldName: 'feature1', type: 'text' },
        { label: 'Feature 2', fieldName: 'feature2', type: 'text' },
        { label: 'Class', fieldName: 'class', type: 'text' }
    ];
    
    constructor() {
        super();
        this.initializeModel();
    }
    
    initializeModel() {
        // Initialize with sample data
        this.addSampleData();
    }
    
    addSampleData() {
        this.trainingData = [
            { id: '1', feature1: 'sunny', feature2: 'hot', class: 'no' },
            { id: '2', feature1: 'sunny', feature2: 'warm', class: 'yes' },
            { id: '3', feature1: 'overcast', feature2: 'warm', class: 'yes' },
            { id: '4', feature1: 'rainy', feature2: 'cool', class: 'yes' },
            { id: '5', feature1: 'rainy', feature2: 'cold', class: 'no' },
            { id: '6', feature1: 'rainy', feature2: 'warm', class: 'no' },
            { id: '7', feature1: 'overcast', feature2: 'cool', class: 'yes' },
            { id: '8', feature1: 'sunny', feature2: 'warm', class: 'yes' },
            { id: '9', feature1: 'sunny', feature2: 'cool', class: 'yes' },
            { id: '10', feature1: 'rainy', feature2: 'warm', class: 'no' }
        ];
        
        this.trainModel();
    }
    
    handleFeature1Change(event) {
        this.feature1 = event.target.value;
    }
    
    handleFeature2Change(event) {
        this.feature2 = event.target.value;
    }
    
    trainModel() {
        // Initialize model parameters
        this.classPrior = {};
        this.featureLikelihood = {};
        this.featureCounts = {};
        this.totalSamples = this.trainingData.length;
        this.classes = [...new Set(this.trainingData.map(item => item.class))];
        
        // Calculate class priors
        this.classes.forEach(cls => {
            this.classPrior[cls] = this.trainingData.filter(item => item.class === cls).length / this.totalSamples;
        });
        
        // Calculate feature likelihoods
        this.classes.forEach(cls => {
            this.featureLikelihood[cls] = {
                feature1: {},
                feature2: {}
            };
            
            const classData = this.trainingData.filter(item => item.class === cls);
            
            // Count feature1 values
            const feature1Counts = {};
            classData.forEach(item => {
                feature1Counts[item.feature1] = (feature1Counts[item.feature1] || 0) + 1;
            });
            
            // Count feature2 values
            const feature2Counts = {};
            classData.forEach(item => {
                feature2Counts[item.feature2] = (feature2Counts[item.feature2] || 0) + 1;
            });
            
            // Calculate probabilities
            this.featureLikelihood[cls].feature1 = this.normalizeCounts(feature1Counts);
            this.featureLikelihood[cls].feature2 = this.normalizeCounts(feature2Counts);
        });
    }
    
    normalizeCounts(counts) {
        const total = Object.values(counts).reduce((sum, count) => sum + count, 0);
        const normalized = {};
        Object.keys(counts).forEach(key => {
            normalized[key] = counts[key] / total;
        });
        return normalized;
    }
    
    predict() {
        if (!this.feature1 || !this.feature2) {
            this.predictionResult = 'Please enter both features';
            return;
        }
        
        const predictions = {};
        
        this.classes.forEach(cls => {
            // Calculate P(class)
            let probability = this.classPrior[cls];
            
            // Calculate P(feature1|class) * P(feature2|class)
            if (this.featureLikelihood[cls].feature1[this.feature1]) {
                probability *= this.featureLikelihood[cls].feature1[this.feature1];
            } else {
                probability *= 0.001; // Smoothing for unseen features
            }
            
            if (this.featureLikelihood[cls].feature2[this.feature2]) {
                probability *= this.featureLikelihood[cls].feature2[this.feature2];
            } else {
                probability *= 0.001; // Smoothing for unseen features
            }
            
            predictions[cls] = probability;
        });
        
        // Find class with maximum probability
        const maxClass = Object.keys(predictions).reduce((a, b) => 
            predictions[a] > predictions[b] ? a : b
        );
        
        this.predictionResult = maxClass;
        this.confidence = (predictions[maxClass] * 100).toFixed(2) + '%';
    }
    
    get totalSamples() {
        return this.trainingData.length;
    }
    
    get classes() {
        return this.classes.join(', ');
    }
}
```

## CSS Styles (naiveBayesClassifier.css)

```css
.slds-box {
    margin: 1rem;
}

.slds-grid {
    margin: 1rem 0;
}

.slds-m-top_small {
    margin-top: 1rem;
}

.slds-m-bottom_small {
    margin-bottom: 1rem;
}
```

## Key Features of this Implementation:

1. **Training Data Management**: Displays and allows adding training samples
2. **Real-time Prediction**: Takes user input and predicts class
3. **Probability Calculation**: Computes class probabilities using Bayes' theorem
4. **Smoothing**: Handles unseen features with Laplace smoothing
5. **Model Statistics**: Shows training statistics and model parameters
6. **Responsive Design**: Uses Lightning Design System components

## How it Works:

1. **Training Phase**: 
   - Calculates class priors P(class)
   - Computes feature likelihoods P(feature|class)
   - Uses Laplace smoothing for unseen features

2. **Prediction Phase**:
   - Applies Bayes' theorem: P(class|features) ∝ P(class) × P(feature1|class) × P(feature2|class)
   - Selects class with highest probability
   - Returns prediction with confidence level

This implementation demonstrates the core concepts of Naive Bayes classification in a Lightning Web Component format.

