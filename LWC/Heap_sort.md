# Heap Sort in Lightning Web Component

Here's a complete example of implementing Heap Sort algorithm in a Lightning Web Component:

## heapSort.js (JavaScript Controller)

```javascript
import { LightningElement } from 'lwc';

export default class HeapSort extends LightningElement {
    inputArray = [64, 34, 25, 12, 22, 11, 90];
    sortedArray = [];
    steps = [];
    isSorting = false;

    get arrayString() {
        return this.inputArray.join(', ');
    }

    get sortedString() {
        return this.sortedArray.join(', ');
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

    handleReset() {
        this.sortedArray = [...this.inputArray];
        this.steps = [];
    }
}
```

## heapSort.html (HTML Template)

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Heap Sort Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Input Array">
                    <p class="slds-text-body_regular">{arrayString}</p>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Sorted Array">
                    <p class="slds-text-body_regular">{sortedString}</p>
                </lightning-card>
            </div>
        </div>

        <div class="slds-m-top_medium">
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
                class="slds-m-left_small">
            </lightning-button>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Algorithm Steps">
                <template if:true={steps.length}>
                    <ul class="slds-list_dotted">
                        <template for:each={steps} for:item="step">
                            <li key={step.index}>{step.description}</li>
                        </template>
                    </ul>
                </template>
                <template if:false={steps.length}>
                    <p>No sorting steps yet. Click "Sort Array" to see the process.</p>
                </template>
            </lightning-card>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="How Heap Sort Works">
                <ul class="slds-list_dotted">
                    <li>Build a max heap from the input data</li>
                    <li>Extract the maximum element and place it at the end</li>
                    <li>Reduce the heap size and heapify the root</li>
                    <li>Repeat until all elements are sorted</li>
                </ul>
            </lightning-card>
        </div>
    </div>
</template>
```

## heapSort.css (CSS Styles)

```css
.slds-box {
    padding: 1rem;
}

.slds-text-body_regular {
    font-size: 0.875rem;
    line-height: 1.4;
}

.slds-list_dotted {
    padding-left: 1rem;
}

.slds-list_dotted li {
    margin-bottom: 0.25rem;
}
```

## Key Features of this Implementation:

1. **Complete Heap Sort Algorithm**: Implements the full heap sort with heapify function
2. **Interactive UI**: Users can sort and reset arrays
3. **Visual Feedback**: Shows input and output arrays
4. **Educational Value**: Includes algorithm explanation
5. **Responsive Design**: Uses Lightning Web Component styling

## How it Works:

1. **Build Max Heap**: Convert input array into a max heap
2. **Extract Elements**: Repeatedly extract the maximum element
3. **Heapify**: Maintain heap property after each extraction
4. **Result**: Sorted array in ascending order

The algorithm has O(n log n) time complexity and O(1) space complexity, making it efficient for sorting large datasets.

