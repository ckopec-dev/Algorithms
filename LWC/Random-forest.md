# Random Forest Algorithm in Lightning Web Component

```javascript
// randomForest.js - Lightning Web Component JavaScript file
import { LightningElement, track } from 'lwc';

export default class RandomForest extends LightningElement {
    @track predictions = [];
    @track isTraining = false;
    @track trainingData = [];
    @track modelInfo = {
        trees: 10,
        maxDepth: 5,
        features: 3
    };

    // Sample training data
    sampleData = [
        { feature1: 1.2, feature2: 3.4, feature3: 5.6, target: 0 },
        { feature1: 2.1, feature2: 4.3, feature3: 6.7, target: 1 },
        { feature1: 3.5, feature2: 5.2, feature3: 7.8, target: 0 },
        { feature1: 4.1, feature2: 6.3, feature3: 8.9, target: 1 },
        { feature1: 5.2, feature2: 7.4, feature3: 9.1, target: 0 },
        { feature1: 6.3, feature2: 8.5, feature3: 10.2, target: 1 },
        { feature1: 7.4, feature2: 9.6, feature3: 11.3, target: 0 },
        { feature1: 8.5, feature2: 10.7, feature3: 12.4, target: 1 }
    ];

    // Decision Tree node class
    class TreeNode {
        constructor() {
            this.featureIndex = null;
            this.threshold = null;
            this.left = null;
            this.right = null;
            this.value = null;
        }
    }

    // Decision Tree class
    class DecisionTree {
        constructor(maxDepth = 5) {
            this.maxDepth = maxDepth;
            this.root = null;
        }

        // Train the decision tree
        train(X, y) {
            this.root = this.buildTree(X, y, 0);
        }

        // Build tree recursively
        buildTree(X, y, depth) {
            const node = new TreeNode();
            
            // Base cases
            if (depth >= this.maxDepth || y.every(val => val === y[0])) {
                node.value = this.majorityClass(y);
                return node;
            }

            // Find best split
            const { featureIndex, threshold } = this.findBestSplit(X, y);
            
            if (featureIndex === null) {
                node.value = this.majorityClass(y);
                return node;
            }

            node.featureIndex = featureIndex;
            node.threshold = threshold;

            // Split data
            const { leftIndices, rightIndices } = this.splitData(X, featureIndex, threshold);

            // Recursively build left and right subtrees
            if (leftIndices.length > 0) {
                const leftX = leftIndices.map(i => X[i]);
                const leftY = leftIndices.map(i => y[i]);
                node.left = this.buildTree(leftX, leftY, depth + 1);
            }

            if (rightIndices.length > 0) {
                const rightX = rightIndices.map(i => X[i]);
                const rightY = rightIndices.map(i => y[i]);
                node.right = this.buildTree(rightX, rightY, depth + 1);
            }

            return node;
        }

        // Find best split point
        findBestSplit(X, y) {
            let bestGini = Infinity;
            let bestFeature = null;
            let bestThreshold = null;

            for (let featureIndex = 0; featureIndex < X[0].length; featureIndex++) {
                const featureValues = X.map(row => row[featureIndex]);
                const thresholds = [...new Set(featureValues)].sort((a, b) => a - b);

                for (let i = 0; i < thresholds.length - 1; i++) {
                    const threshold = (thresholds[i] + thresholds[i + 1]) / 2;
                    const { leftIndices, rightIndices } = this.splitData(X, featureIndex, threshold);
                    
                    const gini = this.calculateGini(y, leftIndices, rightIndices);
                    
                    if (gini < bestGini) {
                        bestGini = gini;
                        bestFeature = featureIndex;
                        bestThreshold = threshold;
                    }
                }
            }

            return { featureIndex: bestFeature, threshold: bestThreshold };
        }

        // Split data based on threshold
        splitData(X, featureIndex, threshold) {
            const leftIndices = [];
            const rightIndices = [];

            X.forEach((row, index) => {
                if (row[featureIndex] <= threshold) {
                    leftIndices.push(index);
                } else {
                    rightIndices.push(index);
                }
            });

            return { leftIndices, rightIndices };
        }

        // Calculate Gini impurity
        calculateGini(y, leftIndices, rightIndices) {
            const total = y.length;
            const leftY = leftIndices.map(i => y[i]);
            const rightY = rightIndices.map(i => y[i]);

            const leftGini = this.giniImpurity(leftY);
            const rightGini = this.giniImpurity(rightY);

            const leftWeight = leftIndices.length / total;
            const rightWeight = rightIndices.length / total;

            return leftWeight * leftGini + rightWeight * rightGini;
        }

        // Gini impurity calculation
        giniImpurity(y) {
            if (y.length === 0) return 0;
            
            const counts = {};
            y.forEach(val => {
                counts[val] = (counts[val] || 0) + 1;
            });

            let gini = 1;
            Object.values(counts).forEach(count => {
                const probability = count / y.length;
                gini -= probability * probability;
            });

            return gini;
        }

        // Get majority class
        majorityClass(y) {
            const counts = {};
            y.forEach(val => {
                counts[val] = (counts[val] || 0) + 1;
            });

            return parseInt(Object.keys(counts).reduce((a, b) => 
                counts[a] > counts[b] ? a : b));
        }

        // Make prediction
        predict(sample) {
            return this.predictRecursive(this.root, sample);
        }

        predictRecursive(node, sample) {
            if (node.value !== null) {
                return node.value;
            }

            if (sample[node.featureIndex] <= node.threshold) {
                return this.predictRecursive(node.left, sample);
            } else {
                return this.predictRecursive(node.right, sample);
            }
        }
    }

    // Random Forest class
    class RandomForest {
        constructor(nTrees = 10, maxDepth = 5) {
            this.nTrees = nTrees;
            this.maxDepth = maxDepth;
            this.trees = [];
        }

        // Train the random forest
        train(X, y) {
            this.trees = [];
            
            for (let i = 0; i < this.nTrees; i++) {
                // Bootstrap sampling
                const bootstrapIndices = this.bootstrapSample(X.length);
                const bootstrapX = bootstrapIndices.map(i => X[i]);
                const bootstrapY = bootstrapIndices.map(i => y[i]);
                
                const tree = new DecisionTree(this.maxDepth);
                tree.train(bootstrapX, bootstrapY);
                this.trees.push(tree);
            }
        }

        // Bootstrap sampling
        bootstrapSample(n) {
            const indices = [];
            for (let i = 0; i < n; i++) {
                indices.push(Math.floor(Math.random() * n));
            }
            return indices;
        }

        // Make prediction using majority voting
        predict(X) {
            const predictions = [];
            
            for (let i = 0; i < X.length; i++) {
                const sample = X[i];
                const votes = this.trees.map(tree => tree.predict(sample));
                
                // Majority voting
                const voteCounts = {};
                votes.forEach(vote => {
                    voteCounts[vote] = (voteCounts[vote] || 0) + 1;
                });
                
                const prediction = parseInt(Object.keys(voteCounts).reduce((a, b) => 
                    voteCounts[a] > voteCounts[b] ? a : b));
                
                predictions.push(prediction);
            }
            
            return predictions;
        }
    }

    // Initialize and train the model
    handleTrain() {
        this.isTraining = true;
        
        // Prepare training data
        const X = this.sampleData.map(item => [
            item.feature1, 
            item.feature2, 
            item.feature3
        ]);
        const y = this.sampleData.map(item => item.target);

        // Create and train random forest
        const rf = new RandomForest(this.modelInfo.trees, this.modelInfo.maxDepth);
        rf.train(X, y);

        // Make predictions
        const testSamples = [
            [2.0, 4.0, 6.0],
            [5.0, 7.0, 9.0],
            [8.0, 10.0, 12.0]
        ];

        const predictions = rf.predict(testSamples);
        
        this.predictions = predictions.map((pred, index) => ({
            sample: testSamples[index],
            prediction: pred
        }));

        this.isTraining = false;
    }

    // Handle model configuration changes
    handleTreeChange(event) {
        this.modelInfo.trees = parseInt(event.target.value);
    }

    handleDepthChange(event) {
        this.modelInfo.maxDepth = parseInt(event.target.value);
    }

    // Render the component
    render() {
        return super.render();
    }
}
```

```html
<!-- randomForest.html - Lightning Web Component HTML template -->
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <h2 class="slds-text-heading_small slds-m-bottom_small">Random Forest Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                <lightning-card title="Model Configuration">
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Number of Trees</label>
                        <div class="slds-form-element__control">
                            <lightning-input 
                                type="number" 
                                value={modelInfo.trees}
                                min="1"
                                max="100"
                                onchange={handleTreeChange}>
                            </lightning-input>
                        </div>
                    </div>
                    
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Max Depth</label>
                        <div class="slds-form-element__control">
                            <lightning-input 
                                type="number" 
                                value={modelInfo.maxDepth}
                                min="1"
                                max="20"
                                onchange={handleDepthChange}>
                            </lightning-input>
                        </div>
                    </div>
                    
                    <lightning-button 
                        label="Train Model" 
                        variant="brand" 
                        onclick={handleTrain}
                        disabled={isTraining}>
                    </lightning-button>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                <lightning-card title="Training Data">
                    <p class="slds-text-body_small">Sample dataset used for training:</p>
                    <ul class="slds-list_dotted">
                        <template for:each={sampleData} for:item="item">
                            <li key={item.feature1}>{item.feature1}, {item.feature2}, {item.feature3} → {item.target}</li>
                        </template>
                    </ul>
                </lightning-card>
            </div>
        </div>
        
        <div class="slds-m-top_medium">
            <lightning-card title="Predictions">
                <template if:true={predictions.length > 0}>
                    <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                        <thead>
                            <tr class="slds-line-height_reset">
                                <th scope="col">Sample</th>
                                <th scope="col">Prediction</th>
                            </tr>
                        </thead>
                        <tbody>
                            <template for:each={predictions} for:item="pred">
                                <tr key={pred.sample}>
                                    <td>{pred.sample}</td>
                                    <td>{pred.prediction}</td>
                                </tr>
                            </template>
                        </tbody>
                    </table>
                </template>
                <template if:false={predictions.length > 0}>
                    <p>No predictions available. Train the model to see results.</p>
                </template>
            </lightning-card>
        </div>
        
        <div class="slds-m-top_medium">
            <lightning-card title="Algorithm Explanation">
                <p>Random Forest is an ensemble learning method that operates by constructing multiple decision trees during training and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees.</p>
                <ul class="slds-list_dotted">
                    <li>Uses bootstrap sampling to create diverse training sets</li>
                    <li>Each tree is trained on a subset of features</li>
                    <li>Predictions are made by majority voting</li>
                    <li>Reduces overfitting compared to single decision trees</li>
                </ul>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
/* randomForest.css - Lightning Web Component CSS */
.slds-box_x-small {
    padding: 0.75rem;
}

.slds-m-around_medium {
    margin: 1rem;
}

.slds-m-top_medium {
    margin-top: 1rem;
}

.slds-text-heading_small {
    font-size: 1.125rem;
    font-weight: 600;
    line-height: 1.25;
}
```

## Key Features of this Implementation:

1. **Complete Random Forest Algorithm**: Implements both Decision Tree and Random Forest classes
2. **Bootstrap Sampling**: Uses bootstrap sampling to create diverse training sets
3. **Majority Voting**: Makes predictions using majority voting among trees
4. **Interactive Configuration**: Allows users to adjust number of trees and max depth
5. **Training Data Display**: Shows sample training data
6. **Prediction Results**: Displays prediction results with sample inputs
7. **Algorithm Explanation**: Provides educational information about Random Forest

## How to Use:

1. Create a new Lightning Web Component with this code
2. Configure the number of trees and max depth
3. Click "Train Model" to run the algorithm
4. View predictions for sample data points

This implementation demonstrates core Random Forest concepts including ensemble methods, bootstrap sampling, and decision tree construction within the Lightning Web Component framework.

