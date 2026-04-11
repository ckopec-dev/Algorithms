# Heap Sort in Lightning Web Component

Here's an example of implementing Heap Sort algorithm in a Lightning Web Component:

```javascript
// heapSort.js
import { LightningElement } from 'lwc';

export default class HeapSort extends LightningElement {
    array = [64, 34, 25, 12, 22, 11, 90];
    sortedArray = [];
    steps = [];
    isSorting = false;

    connectedCallback() {
        this.sortedArray = [...this.array];
        this.steps = [];
        this.steps.push({
            array: [...this.array],
            message: 'Original Array'
        });
    }

    handleSort() {
        if (this.isSorting) return;
        
        this.isSorting = true;
        this.sortedArray = [...this.array];
        this.steps = [];
        
        // Add initial state
        this.steps.push({
            array: [...this.sortedArray],
            message: 'Starting Heap Sort'
        });
        
        this.heapSort(this.sortedArray);
        
        this.steps.push({
            array: [...this.sortedArray],
            message: 'Sorting Complete'
        });
        
        this.isSorting = false;
    }

    heapSort(arr) {
        const n = arr.length;
        
        // Build max heap
        for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
            this.heapify(arr, n, i);
            this.steps.push({
                array: [...arr],
                message: `Heapifying at index ${i}`
            });
        }
        
        // Extract elements from heap one by one
        for (let i = n - 1; i > 0; i--) {
            // Move current root to end
            [arr[0], arr[i]] = [arr[i], arr[0]];
            
            this.steps.push({
                array: [...arr],
                message: `Swapping ${arr[i]} to end, heapifying remaining`
            });
            
            // Call heapify on the reduced heap
            this.heapify(arr, i, 0);
            
            this.steps.push({
                array: [...arr],
                message: `Heapified root at index 0`
            });
        }
    }

    heapify(arr, n, i) {
        let largest = i;
        let left = 2 * i + 1;
        let right = 2 * i + 2;

        // If left child exists and is greater than root
        if (left < n && arr[left] > arr[largest]) {
            largest = left;
        }

        // If right child exists and is greater than largest so far
        if (right < n && arr[right] > arr[largest]) {
            largest = right;
        }

        // If largest is not root
        if (largest !== i) {
            [arr[i], arr[largest]] = [arr[largest], arr[i]];
            
            // Recursively heapify the affected sub-tree
            this.heapify(arr, n, largest);
        }
    }

    get stepCount() {
        return this.steps.length;
    }

    get currentStep() {
        return this.steps.length > 0 ? this.steps[this.steps.length - 1] : null;
    }

    handleReset() {
        this.sortedArray = [...this.array];
        this.steps = [];
        this.isSorting = false;
    }
}
```

```html
<!-- heapSort.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="Heap Sort">
                <h2 class="slds-text-heading_small">Heap Sort Algorithm</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-box slds-theme_default">
                        <h3>Original Array</h3>
                        <p>{array}</p>
                    </div>
                </div>
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-box slds-theme_default">
                        <h3>Sorted Array</h3>
                        <p>{sortedArray}</p>
                    </div>
                </div>
            </div>

            <div class="slds-m-top_medium">
                <lightning-button 
                    label="Start Sorting" 
                    variant="brand" 
                    onclick={handleSort}
                    disabled={isSorting}>
                </lightning-button>
                <lightning-button 
                    label="Reset" 
                    variant="neutral" 
                    onclick={handleReset}
                    class="slds-m-left_small">
                </lightning-button>
            </div>

            <div class="slds-m-top_medium">
                <h3>Sorting Steps ({stepCount})</h3>
                <template if:true={steps.length}>
                    <div class="slds-box slds-theme_default">
                        <template for:each={steps} for:item="step">
                            <div key={step.message} class="slds-m-bottom_small">
                                <p><strong>{step.message}</strong></p>
                                <p>{step.array}</p>
                            </div>
                        </template>
                    </div>
                </template>
                <template if:false={steps.length}>
                    <p>No sorting steps yet. Click "Start Sorting" to begin.</p>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* heapSort.css */
.slds-card__body {
    padding: 1rem;
}

.slds-box {
    padding: 1rem;
    border: 1px solid #e5e5e5;
    border-radius: 0.25rem;
}

.slds-button {
    margin: 0.25rem;
}
```

## How it works:

1. **Heap Sort Algorithm**: The implementation follows the standard heap sort algorithm with:
   - Building a max heap from the input array
   - Repeatedly extracting the maximum element and heapifying the remaining elements

2. **Step-by-step Visualization**: The component shows each step of the sorting process:
   - Initial heap building
   - Element swapping
   - Heapification of remaining elements

3. **Interactive Features**:
   - Start Sorting button to begin the process
   - Reset button to restore original array
   - Visual display of each sorting step

4. **Key Methods**:
   - `heapSort()`: Main sorting function that builds heap and sorts
   - `heapify()`: Maintains heap property
   - `handleSort()`: Triggers the sorting process
   - `handleReset()`: Resets the component state

This implementation demonstrates how to use LWC to create an interactive visualization of the heap sort algorithm with step-by-step execution tracking.

