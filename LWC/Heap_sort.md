# Heap Sort in Lightning Web Component

Here's a complete example of implementing Heap Sort algorithm in a Lightning Web Component:

```javascript
// heapSort.js
import { LightningElement } from 'lwc';

export default class HeapSort extends LightningElement {
    inputArray = [64, 34, 25, 12, 22, 11, 90];
    sortedArray = [];
    steps = [];
    isSorting = false;

    connectedCallback() {
        this.sortedArray = [...this.inputArray];
    }

    handleSort() {
        if (this.isSorting) return;
        
        this.isSorting = true;
        this.steps = [];
        this.sortedArray = [...this.inputArray];
        
        // Perform heap sort
        this.heapSort(this.sortedArray);
        
        this.isSorting = false;
    }

    heapSort(arr) {
        const n = arr.length;
        
        // Build max heap
        for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
            this.heapify(arr, n, i);
        }
        
        // Extract elements from heap one by one
        for (let i = n - 1; i > 0; i--) {
            // Move current root to end
            [arr[0], arr[i]] = [arr[i], arr[0]];
            
            // Call heapify on the reduced heap
            this.heapify(arr, i, 0);
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

    handleInputChange(event) {
        const value = event.target.value;
        if (value.trim() === '') {
            this.inputArray = [];
        } else {
            this.inputArray = value.split(',').map(num => parseInt(num.trim())).filter(num => !isNaN(num));
        }
        this.sortedArray = [...this.inputArray];
    }

    get arrayString() {
        return this.inputArray.join(', ');
    }

    get sortedString() {
        return this.sortedArray.join(', ');
    }
}
```

```html
<!-- heapSort.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Heap Sort Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="arrayInput">Input Array (comma-separated)</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="arrayInput"
                    class="slds-input"
                    value={arrayString}
                    onchange={handleInputChange}
                    placeholder="e.g., 64, 34, 25, 12, 22, 11, 90"
                />
            </div>
        </div>

        <lightning-button 
            label="Sort Array" 
            variant="brand" 
            onclick={handleSort}
            disabled={isSorting}
            class="slds-m-top_small">
        </lightning-button>

        <div class="slds-m-top_small">
            <p><strong>Original Array:</strong> {arrayString}</p>
            <p><strong>Sorted Array:</strong> {sortedString}</p>
        </div>

        <div

