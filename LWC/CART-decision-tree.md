# CART Decision Tree in Lightning Web Component

Here's an example implementation of a CART (Classification and Regression Trees) decision tree algorithm in Lightning Web Component:

```javascript
// cartDecisionTree.js
import { LightningElement, api } from 'lwc';

export default class CartDecisionTree extends LightningElement {
    @api data;
    @api targetColumn;
    @api maxDepth = 5;
    @api minSamplesSplit = 2;
    
    tree = null;
    isTraining = false;

    connectedCallback() {
        if (this.data && this.targetColumn) {
            this.train();
        }
    }

    @api
    train() {
        this.isTraining = true;
        // Simulate async training
        setTimeout(() => {
            this.tree = this.buildTree(this.data, this.targetColumn, 0);
            this.isTraining = false;
            this.dispatchEvent(new CustomEvent('treeready'));
        }, 100);
    }

    buildTree(data, targetColumn, depth) {
        // Base cases
        if (depth >= this.maxDepth || data.length < this.minSamplesSplit) {
            return this.createLeaf(data, targetColumn);
        }

        // Find best split
        const bestSplit = this.findBestSplit(data, targetColumn);
        
        if (!bestSplit || bestSplit.gain <= 0) {
            return this.createLeaf(data, targetColumn);
        }

        // Split data
        const leftData = data.filter(row => row[bestSplit.feature] <= bestSplit.threshold);
        const rightData = data.filter(row => row[bestSplit.feature] > bestSplit.threshold);

        // Create node
        const node = {
            feature: bestSplit.feature,
            threshold: bestSplit.threshold,
            left: this.buildTree(leftData, targetColumn, depth + 1),
            right: this.buildTree(rightData, targetColumn, depth + 1)
        };

        return node;
    }

    findBestSplit(data, targetColumn) {
        let bestGain = -1;
        let bestFeature = null;
        let bestThreshold = null;

        const features = Object.keys(data[0]).filter(key => key !== targetColumn);
        const targetValues = data.map(row => row[targetColumn]);

        for (const feature of features) {
            const thresholds = this.getUniqueValues(data, feature).sort((a, b) => a - b);
            
            for (let i = 0; i < thresholds.length - 1; i++) {
                const threshold = (thresholds[i] + thresholds[i + 1]) / 2;
                const gain = this.calculateInformationGain(data, targetColumn, feature, threshold);
                
                if (gain > bestGain) {
                    bestGain = gain;
                    bestFeature = feature;
                    bestThreshold = threshold;
                }
            }
        }

        return {
            feature: bestFeature,
            threshold: bestThreshold,
            gain: bestGain
        };
    }

    calculateInformationGain(data, targetColumn, feature, threshold) {
        const targetValues = data.map(row => row[targetColumn]);
        const totalEntropy = this.calculateEntropy(targetValues);
        
        const leftData = data.filter(row => row[feature] <= threshold);
        const rightData = data.filter(row => row[feature] > threshold);
        
        if (leftData.length === 0 || rightData.length === 0) {
            return 0;
        }

        const leftEntropy = this.calculateEntropy(leftData.map(row => row[targetColumn]));
        const rightEntropy = this.calculateEntropy(rightData.map(row => row[targetColumn]));
        
        const leftWeight = leftData.length / data.length;
        const rightWeight = rightData.length / data.length;
        
        const weightedEntropy = leftWeight * leftEntropy + rightWeight * rightEntropy;
        
        return totalEntropy - weightedEntropy;
    }

    calculateEntropy(values) {
        if (values.length === 0) return 0;
        
        const counts = {};
        values.forEach(value => {
            counts[value] = (counts[value] || 0) + 1;
        });
        
        const probabilities = Object.values(counts).map(count => count / values.length);
        const entropy = probabilities.reduce((sum, prob) => {
            return prob > 0 ? sum - prob * Math.log2(prob) : sum;
        }, 0);
        
        return entropy;
    }

    createLeaf(data, targetColumn) {
        const targetValues = data.map(row => row[targetColumn]);
        const counts = {};
        targetValues.forEach(value => {
            counts[value] = (counts[value] || 0) + 1;
        });
        
        const majorityClass = Object.keys(counts).reduce((a, b) => 
            counts[a] > counts[b] ? a : b
        );
        
        return {
            prediction: majorityClass,
            samples: data.length
        };
    }

    getUniqueValues(data, column) {
        return [...new Set(data.map(row => row[column]))];
    }

    @api
    predict(inputData) {
        if (!this.tree) {
            throw new Error('Tree not trained yet');
        }
        return this.predictRecursive(this.tree, inputData);
    }

    predictRecursive(node, inputData) {
        if (node.prediction !== undefined) {
            return node.prediction;
        }

        if (inputData[node.feature] <= node.threshold) {
            return this.predictRecursive(node.left, inputData);
        } else {
            return this.predictRecursive(node.right, inputData);
        }
    }

    @api
    getTreeStructure() {
        return this.tree;
    }

    @api
    reset() {
        this.tree = null;
    }
}
```

```html
<!-- cartDecisionTree.html -->
<template>
    <div class="tree-container">
        <lightning-card title="CART Decision Tree" icon-name="custom:custom18">
            <div class="slds-m-around_medium">
                <lightning-button 
                    label="Train Model" 
                    variant="brand"
                    onclick={train}
                    disabled={isTraining}
                    class="slds-m-bottom_small">
                </lightning-button>
                
                <lightning-button 
                    label="Reset Model" 
                    variant="destructive"
                    onclick={reset}
                    class="slds-m-bottom_small">
                </lightning-button>
                
                <template if:true={isTraining}>
                    <lightning-spinner alternative-text="Training in progress"></lightning-spinner>
                    <p>Training the decision tree...</p>
                </template>
                
                <template if:true={tree}>
                    <div class="slds-m-top_small">
                        <h3>Tree Structure</h3>
                        <pre>{treeJson}</pre>
                    </div>
                    
                    <div class="slds-m-top_small">
                        <lightning-input 
                            label="Sample Prediction Input"
                            value={sampleInput}
                            onchange={handleInputChange}
                            class="slds-m-bottom_small">
                        </lightning-input>
                        <lightning-button 
                            label="Make Prediction" 
                            variant="success"
                            onclick={makePrediction}
                            class="slds-m-bottom_small">
                        </lightning-button>
                        <p>Prediction: <strong>{predictionResult}</strong></p>
                    </div>
                </template>
            </div>
        </lightning-card>
    </div>
</template>
```

```css
/* cartDecisionTree.css */
.tree-container {
    max-width: 800px;
    margin: 0 auto;
}

pre {
    background-color: #f4f6f9;
    padding: 1rem;
    border-radius: 0.25rem;
    overflow-x: auto;
    font-size: 0.875rem;
}
```

## Usage Example

```html
<!-- Usage in parent component -->
<template>
    <c-cart-decision-tree 
        data={trainingData}
        target-column="target"
        max-depth="3"
        min-samples-split="5"
        ontreeready={handleTreeReady}>
    </c-cart-decision-tree>
</template>
```

## Key Features

1. **CART Algorithm Implementation**: Uses binary splits based on information gain
2. **Configurable Parameters**: 
   - `maxDepth`: Maximum tree depth
   - `minSamplesSplit`: Minimum samples required to split a node
3. **Training Process**: Asynchronous training with loading indicator
4. **Prediction Capability**: Can make predictions on new data
5. **Tree Visualization**: Shows tree structure in JSON format
6. **Error Handling**: Proper error handling for untrained models

## Algorithm Steps

1. **Data Preparation**: Process input data and target column
2. **Tree Building**: Recursively split data based on information gain
3. **Stopping Criteria**: Depth limit or minimum samples
4. **Leaf Creation**: Create leaf nodes with majority class prediction
5. **Prediction**: Traverse tree based on input features

This implementation provides a foundation for CART decision tree that can be extended with additional features like cross-validation, pruning, or different splitting criteria.

