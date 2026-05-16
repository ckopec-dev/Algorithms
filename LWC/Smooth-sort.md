# Smooth Sort in Lightning Web Component

Here's an example implementation of the Smooth Sort algorithm in Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class SmoothSortExample extends LightningElement {
    array = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42];
    sortedArray = [];
    isSorting = false;
    steps = [];

    connectedCallback() {
        this.sortedArray = [...this.array];
        this.steps = [];
    }

    handleSort() {
        this.isSorting = true;
        this.steps = [];
        
        // Create a copy of the array to sort
        const arr = [...this.sortedArray];
        
        // Perform smooth sort
        this.smoothSort(arr);
        
        this.sortedArray = arr;
        this.isSorting = false;
    }

    smoothSort(arr) {
        const heapSize = arr.length;
        
        // Build max heap
        for (let i = Math.floor(heapSize / 2) - 1; i >= 0; i--) {
            this.heapify(arr, heapSize, i);
        }
        
        // Extract elements from heap one by one
        for (let i = heapSize - 1; i > 0; i--) {
            // Move current root to end
            [arr[0], arr[i]] = [arr[i], arr[0]];
            
            // Call heapify on the reduced heap
            this.heapify(arr, i, 0);
        }
    }

    heapify(arr, heapSize, rootIndex) {
        let largest = rootIndex;
        let left = 2 * rootIndex + 1;
        let right = 2 * rootIndex + 2;

        // If left child exists and is greater than root
        if (left < heapSize && arr[left] > arr[largest]) {
            largest = left;
        }

        // If right child exists and is greater than largest so far
        if (right < heapSize && arr[right] > arr[largest]) {
            largest = right;
        }

        // If largest is not root
        if (largest !== rootIndex) {
            [arr[rootIndex], arr[largest]] = [arr[largest], arr[rootIndex]];
            
            // Recursively heapify the affected sub-tree
            this.heapify(arr, heapSize, largest);
        }
    }

    handleReset() {
        this.sortedArray = [...this.array];
        this.steps = [];
    }

    get arrayString() {
        return this.sortedArray.join(', ');
    }
}
```

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Smooth Sort Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_12-of-12">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-card__header-title slds-truncate" title="Array">
                            <h3>Original Array</h3>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <p class="slds-text-body_regular">{arrayString}</p>
                    </div>
                </div>
            </div>
            
            <div class="slds-col slds-size_12-of-12">
                <lightning-button 
                    label="Sort Array" 
                    variant="brand" 
                    onclick={handleSort}
                    disabled={isSorting}>
                </lightning-button>
                <lightning-button 
                    label="Reset" 
                    variant="neutral" 
                    onclick={handleReset}
                    class="slds-m-left_x-small">
                </lightning-button>
            </div>
            
            <div class="slds-col slds-size_12-of-12 slds-m-top_medium">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-card__header-title slds-truncate" title="Result">
                            <h3>Sorted Array</h3>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <p class="slds-text-body_regular">{arrayString}</p>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>
```

```css
.slds-box {
    margin: 1rem;
}

.slds-card {
    margin-bottom: 1rem;
}

.slds-text-body_regular {
    font-size: 1rem;
    line-height: 1.5;
}
```

## Algorithm Explanation

Smooth Sort is a variant of heap sort that is optimized for partially sorted arrays. It works by:

1. **Building a max heap** from the input array
2. **Extracting elements** one by one from the heap
3. **Maintaining the heap property** after each extraction

## Key Features

- **Time Complexity**: O(n log n) in worst case, but better performance on partially sorted data
- **Space Complexity**: O(1) - sorts in-place
- **Adaptive**: Performs better on nearly sorted arrays
- **Stable**: Maintains relative order of equal elements

## Usage

1. Create a new Lightning Web Component
2. Copy the JavaScript code into the `.js` file
3. Copy the HTML template into the `.html` file
4. Copy the CSS into the `.css` file
5. Use the component in your Lightning application

The component will display the original array, allow you to sort it using Smooth Sort, and show the sorted result.

