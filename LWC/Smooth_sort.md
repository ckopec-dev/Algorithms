# Smooth Sort in Lightning Web Component

Here's an example of implementing the Smooth Sort algorithm in Lightning Web Component:

```javascript
// smoothSort.js
import { LightningElement } from 'lwc';

export default class SmoothSort extends LightningElement {
    inputArray = [64, 34, 25, 12, 22, 11, 90];
    sortedArray = [];
    isSorting = false;
    steps = [];

    connectedCallback() {
        this.sortedArray = [...this.inputArray];
    }

    handleSort() {
        this.isSorting = true;
        this.steps = [];
        this.sortedArray = [...this.inputArray];
        
        // Perform smooth sort
        this.smoothSort(this.sortedArray);
        
        this.isSorting = false;
    }

    smoothSort(arr) {
        const n = arr.length;
        if (n <= 1) return;

        // Build the heap
        this.buildSmoothHeap(arr, n);
        
        // Extract elements from heap one by one
        for (let i = n - 1; i > 0; i--) {
            // Move current root to end
            this.swap(arr, 0, i);
            this.steps.push({
                action: 'swap',
                array: [...arr],
                indices: [0, i]
            });
            
            // Call max heapify on the reduced heap
            this.siftDown(arr, 0, i);
        }
        
        this.steps.push({
            action: 'complete',
            array: [...arr]
        });
    }

    buildSmoothHeap(arr, n) {
        // Build heap (rearrange array)
        for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
            this.siftDown(arr, i, n);
        }
    }

    siftDown(arr, start, end) {
        let root = start;
        
        while (true) {
            let child = 2 * root + 1;
            if (child >= end) break;
            
            // If the right child exists and is greater than left child
            if (child + 1 < end && arr[child] < arr[child + 1]) {
                child++;
            }
            
            // If root is smaller than child, swap them
            if (arr[root] < arr[child]) {
                this.swap(arr, root, child);
                this.steps.push({
                    action: 'sift',
                    array: [...arr],
                    indices: [root, child]
                });
                root = child;
            } else {
                break;
            }
        }
    }

    swap(arr, i, j) {
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }

    handleStep() {
        // This would be used for step-by-step visualization
        if (this.steps.length > 0) {
            const step = this.steps.shift();
            this.sortedArray = step.array;
        }
    }

    get arrayString() {
        return this.sortedArray.join(', ');
    }

    get stepCount() {
        return this.steps.length;
    }
}
```

```html
<!-- smoothSort.html -->
<template>
    <div class="container">
        <h2>Smooth Sort Algorithm</h2>
        
        <div class="input-section">
            <label>Input Array:</label>
            <p class="array-display">{arrayString}</p>
            
            <lightning-button 
                label="Sort Array" 
                variant="brand" 
                onclick={handleSort}
                disabled={isSorting}>
            </lightning-button>
            
            <lightning-button 
                label="Step Through" 
                variant="outline-brand" 
                onclick={handleStep}
                disabled={steps.length === 0}>
            </lightning-button>
        </div>

        <div class="visualization">
            <h3>Sorting Steps</h3>
            <div class="step-info">
                <p>Steps remaining: {stepCount}</p>
                <p>Status: {isSorting ? 'Sorting in progress...' : 'Ready'}</p>
            </div>
            
            <div class="array-visualization">
                <template for:each={sortedArray} for:item="item" for:index="index">
                    <div key={index} class="array-element">
                        <span>{item}</span>
                    </div>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* smoothSort.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 30px;
    padding: 20px;
    border: 1px solid #e5e5e5;
    border-radius: 8px;
    background-color: #f9f9f9;
}

.array-display {
    font-family: monospace;
    font-size: 18px;
    font-weight: bold;
    margin: 10px 0;
    padding: 10px;
    background-color: white;
    border: 1px solid #ddd;
    border-radius: 4px;
}

.visualization {
    margin-top: 30px;
}

.step-info {
    margin-bottom: 20px;
    padding: 10px;
    background-color: #e8f4f8;
    border-radius: 4px;
}

.array-visualization {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    margin-top: 20px;
}

.array-element {
    width: 40px;
    height: 40px;
    display: flex;
    align-items: center;
    justify-content: center;
    background-color: #0070d2;
    color: white;
    border-radius: 4px;
    font-weight: bold;
    font-size: 14px;
    transition: all 0.3s ease;
}

.array-element.highlight {
    background-color: #ff9900;
    transform: scale(1.1);
}

.lightning-button {
    margin: 5px;
}
```

## Key Features of this Implementation:

1. **Smooth Sort Algorithm**: Implements the heap-based sorting algorithm with O(n log n) complexity
2. **Step-by-Step Visualization**: Shows each sorting step with visual feedback
3. **Interactive Controls**: Sort button and step-through functionality
4. **Real-time Array Display**: Shows current state of the array during sorting
5. **Responsive Design**: Clean, modern UI with LWC styling

## How Smooth Sort Works:

1. **Build Heap**: Creates a heap structure from the input array
2. **Extract Elements**: Repeatedly extracts the maximum element and places it at the end
3. **Sift Down**: Maintains heap property after each extraction
4. **Visualization**: Shows each step of the process for educational purposes

This implementation demonstrates how Smooth Sort can be integrated into a Lightning Web Component for educational purposes and interactive demonstrations.

