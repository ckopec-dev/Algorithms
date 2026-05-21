# Reservoir Sampling in Lightning Web Component

Here's an example implementation of the Reservoir Sampling algorithm in Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class ReservoirSampling extends LightningElement {
    // Sample data array
    sampleData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    
    // Reservoir sampling result
    reservoirResult = [];
    
    // Size of reservoir
    k = 3;
    
    // Current index being processed
    currentIndex = 0;
    
    // Process data with reservoir sampling
    handleReservoirSampling() {
        this.reservoirResult = this.reservoirSampling(this.sampleData, this.k);
        this.currentIndex = this.sampleData.length;
    }
    
    // Reservoir Sampling Algorithm Implementation
    reservoirSampling(data, k) {
        // Initialize reservoir array
        let reservoir = [];
        
        // Fill reservoir with first k elements
        for (let i = 0; i < k && i < data.length; i++) {
            reservoir.push(data[i]);
        }
        
        // Process remaining elements
        for (let i = k; i < data.length; i++) {
            // Generate random index between 0 and i (inclusive)
            const randomIndex = Math.floor(Math.random() * (i + 1));
            
            // If random index is less than k, replace element in reservoir
            if (randomIndex < k) {
                reservoir[randomIndex] = data[i];
            }
        }
        
        return reservoir;
    }
    
    // Process data step by step (for demonstration)
    handleStepByStep() {
        if (this.currentIndex < this.sampleData.length) {
            // Simulate processing one element at a time
            const currentElement = this.sampleData[this.currentIndex];
            this.currentIndex++;
            
            // Update UI to show current element
            console.log(`Processing element: ${currentElement}`);
        } else {
            // Complete sampling
            this.reservoirResult = this.reservoirSampling(this.sampleData, this.k);
        }
    }
    
    // Reset the sampling
    handleReset() {
        this.reservoirResult = [];
        this.currentIndex = 0;
    }
    
    // Get sample data for display
    get displayData() {
        return this.sampleData.join(', ');
    }
    
    // Get reservoir result for display
    get displayResult() {
        return this.reservoirResult.length > 0 
            ? this.reservoirResult.join(', ') 
            : 'No result yet';
    }
}
```

```html
<!-- reservoirSampling.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Reservoir Sampling Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_12-of-12">
                <p><strong>Sample Data:</strong> {displayData}</p>
                <p><strong>Reservoir Size (k):</strong> {k}</p>
                <p><strong>Current Index:</strong> {currentIndex}</p>
            </div>
            
            <div class="slds-col slds-size_12-of-12">
                <lightning-button 
                    label="Run Reservoir Sampling" 
                    variant="brand" 
                    onclick={handleReservoirSampling}>
                </lightning-button>
                
                <lightning-button 
                    label="Step by Step" 
                    variant="neutral" 
                    onclick={handleStepByStep}
                    style="margin-left: 10px;">
                </lightning-button>
                
                <lightning-button 
                    label="Reset" 
                    variant="destructive" 
                    onclick={handleReset}
                    style="margin-left: 10px;">
                </lightning-button>
            </div>
            
            <div class="slds-col slds-size_12-of-12 slds-m-top_medium">
                <h3>Reservoir Sampling Result:</h3>
                <p>{displayResult}</p>
            </div>
        </div>
    </div>
</template>
```

```css
/* reservoirSampling.css */
.slds-box {
    margin: 20px;
    padding: 20px;
}

.slds-button {
    margin: 5px;
}
```

## How the Algorithm Works:

1. **Initialization**: Fill the reservoir array with the first k elements from the input data
2. **Processing**: For each subsequent element (from index k onwards):
   - Generate a random index between 0 and current index (inclusive)
   - If the random index is less than k, replace the element at that random index in the reservoir
3. **Result**: After processing all elements, the reservoir contains a random sample of k elements

## Key Features:

- **Memory Efficient**: Only stores k elements regardless of input size
- **Uniform Probability**: Each element has equal probability of being selected
- **Single Pass**: Processes data in one pass
- **Streaming Data**: Can handle data streams of unknown length

This implementation demonstrates the core reservoir sampling algorithm that can be used in Lightning Web Components for random sampling of large datasets.

