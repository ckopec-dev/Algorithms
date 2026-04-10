# Gradient Boosting Machines (GBM) in Lightning Web Component

```javascript
// gbmComponent.js
import { LightningElement, track } from 'lwc';

export default class GbmComponent extends LightningElement {
    @track modelData = {
        trees: [],
        learningRate: 0.1,
        maxDepth: 6,
        nEstimators: 100,
        featureImportance: []
    };

    @track predictions = [];
    @track isTraining = false;
    @track trainingProgress = 0;

    // Sample training data
    @track trainingData = [
        { features: [2.5, 3.1, 1.2], target: 1 },
        { features: [1.8, 2.9, 0.8], target: 0 },
        { features: [3.2, 4.1, 1.5], target: 1 },
        { features: [1.2, 1.8, 0.5], target: 0 },
        { features: [2.8, 3.5, 1.1], target: 1 }
    ];

    // Sample test data
    @track testData = [
        { features: [2.0, 2.5, 1.0] },
        { features: [3.0, 3.8, 1.3] }
    ];

    // GBM Algorithm Implementation
    trainModel() {
        this.isTraining = true;
        this.trainingProgress = 0;

        // Initialize base model (constant prediction)
        let basePrediction = this.calculateMeanTarget();
        
        // Gradient Boosting iterations
        for (let i = 0; i < this.modelData.nEstimators; i++) {
            // Calculate residuals (negative gradient)
            let residuals = this.calculateResiduals(basePrediction);
            
            // Train weak learner (decision tree) on residuals
            let weakLearner = this.trainWeakLearner(residuals);
            
            // Update base prediction with new tree contribution
            basePrediction = this.updatePrediction(basePrediction, weakLearner);
            
            // Store tree in model
            this.modelData.trees.push(weakLearner);
            
            // Update progress
            this.trainingProgress = Math.round(((i + 1) / this.modelData.nEstimators) * 100);
            
            // Simulate async training
            if (i % 10 === 0) {
                this.updateModelData();
            }
        }
        
        this.isTraining = false;
        this.predict();
    }

    // Calculate mean of target values for base prediction
    calculateMeanTarget() {
        const targets = this.trainingData.map(item => item.target);
        const sum = targets.reduce((acc, val) => acc + val, 0);
        return sum / targets.length;
    }

    // Calculate residuals (difference between actual and predicted)
    calculateResiduals(predictions) {
        return this.trainingData.map((item, index) => {
            // For simplicity, assuming predictions is an array
            const predicted = Array.isArray(predictions) ? predictions[index] : predictions;
            return item.target - predicted;
        });
    }

    // Train a weak learner (decision tree) on residuals
    trainWeakLearner(residuals) {
        // Simplified tree training - in practice this would be more complex
        return {
            depth: this.modelData.maxDepth,
            splits: this.findBestSplits(residuals),
            leafValues: this.calculateLeafValues(residuals),
            featureImportance: this.calculateFeatureImportance()
        };
    }

    // Find best splits for decision tree
    findBestSplits(residuals) {
        // Simplified splitting logic
        return [
            { featureIndex: 0, threshold: 2.0 },
            { featureIndex: 1, threshold: 3.0 }
        ];
    }

    // Calculate leaf values for tree nodes
    calculateLeafValues(residuals) {
        // Simplified leaf value calculation
        return residuals.map(residual => residual / this.modelData.learningRate);
    }

    // Calculate feature importance
    calculateFeatureImportance() {
        // Simplified feature importance calculation
        return [0.4, 0.3, 0.3];
    }

    // Update prediction with new tree contribution
    updatePrediction(currentPrediction, tree) {
        if (Array.isArray(currentPrediction)) {
            return currentPrediction.map((pred, index) => {
                return pred + (this.modelData.learningRate * this.predictTree(tree, this.trainingData[index].features));
            });
        } else {
            return currentPrediction + (this.modelData.learningRate * this.predictTree(tree, [0, 0, 0]));
        }
    }

    // Predict using a single tree
    predictTree(tree, features) {
        // Simplified tree prediction
        return features[0] > tree.splits[0].threshold ? tree.leafValues[0] : tree.leafValues[1];
    }

    // Make predictions on test data
    predict() {
        this.predictions = this.testData.map(item => {
            let prediction = this.modelData.trees.reduce((acc, tree) => {
                return acc + (this.modelData.learningRate * this.predictTree(tree, item.features));
            }, 0);
            
            // Apply sigmoid for classification
            const probability = 1 / (1 + Math.exp(-prediction));
            return {
                probability: probability,
                prediction: probability > 0.5 ? 1 : 0
            };
        });
    }

    // Update model data in UI
    updateModelData() {
        this.modelData = { ...this.modelData };
    }

    // Handle training button click
    handleTrain() {
        this.trainModel();
    }

    // Handle parameter changes
    handleLearningRateChange(event) {
        this.modelData.learningRate = parseFloat(event.target.value);
    }

    handleMaxDepthChange(event) {
        this.modelData.maxDepth = parseInt(event.target.value);
    }

    handleNEstimatorsChange(event) {
        this.modelData.nEstimators = parseInt(event.target.value);
    }
}
```

```html
<!-- gbmComponent.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Gradient Boosting Machines (GBM)</h2>
        
        <div class="slds-grid slds-gutters">
            <!-- Training Controls -->
            <div class="slds-col slds-size_1-of-3">
                <lightning-card title="Training Parameters">
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Learning Rate</label>
                        <lightning-input 
                            type="number" 
                            value={modelData.learningRate}
                            step="0.01"
                            min="0.01"
                            max="1.0"
                            onchange={handleLearningRateChange}>
                        </lightning-input>
                    </div>
                    
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Max Depth</label>
                        <lightning-input 
                            type="number" 
                            value={modelData.maxDepth}
                            min="1"
                            max="20"
                            onchange={handleMaxDepthChange}>
                        </lightning-input>
                    </div>
                    
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Number of Estimators</label>
                        <lightning-input 
                            type="number" 
                            value={modelData.nEstimators}
                            min="1"
                            max="1000"
                            onchange={handleNEstimatorsChange}>
                        </lightning-input>
                    </div>
                    
                    <lightning-button 
                        label="Train Model" 
                        variant="brand"
                        onclick={handleTrain}
                        disabled={isTraining}>
                    </lightning-button>
                    
                    <lightning-progress-bar 
                        value={trainingProgress}
                        variant="brand"
                        size="medium"
                        label="Training Progress">
                    </lightning-progress-bar>
                </lightning-card>
                
                <lightning-card title="Feature Importance">
                    <lightning-datatable
                        data={modelData.featureImportance}
                        columns={columns}
                        key-field="id">
                    </lightning-datatable>
                </lightning-card>
            </div>
            
            <!-- Results -->
            <div class="slds-col slds-size_2-of-3">
                <lightning-card title="Training Data">
                    <lightning-datatable
                        data={trainingData}
                        columns={trainingColumns}
                        key-field="id">
                    </lightning-datatable>
                </lightning-card>
                
                <lightning-card title="Predictions">
                    <template if:true={predictions.length}>
                        <lightning-datatable
                            data={predictions}
                            columns={predictionColumns}
                            key-field="id">
                        </lightning-datatable>
                    </template>
                    <template if:false={predictions.length}>
                        <p>No predictions available. Train the model first.</p>
                    </template>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

```javascript
// gbmComponent.js-meta.xml
<?xml version="1.0" encoding="UTF-8"?>
<LightningComponentBundle xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>58.0</apiVersion>
    <isExposed>true</isExposed>
    <targets>
        <target>lightning__AppPage</target>
        <target>lightning__RecordPage</target>
        <target>lightning__HomePage</target>
    </targets>
</LightningComponentBundle>
```

## Key Features of this GBM Implementation:

1. **Gradient Boosting Algorithm**: Implements the core GBM concept of sequentially adding trees to correct previous errors
2. **Training Process**: 
   - Calculates residuals (errors) at each step
   - Trains weak learners (decision trees) on residuals
   - Updates predictions with learning rate
3. **Model Parameters**: 
   - Learning rate control
   - Maximum tree depth
   - Number of estimators
4. **Prediction**: Makes predictions using the ensemble of trees
5. **UI Components**: 
   - Training controls
   - Progress indicator
   - Data visualization
   - Results display

This example demonstrates the core concepts of GBM in a LWC framework, showing how the algorithm builds an ensemble of weak learners to create a strong predictive model.

