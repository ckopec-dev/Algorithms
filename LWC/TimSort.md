# TimSort Algorithm in Lightning Web Component

```javascript
// timSort.js - Lightning Web Component JavaScript file
import { LightningElement } from 'lwc';

export default class TimSort extends LightningElement {
    array = [5, 2, 8, 1, 9, 3, 7, 4, 6];
    sortedArray = [];
    isSorting = false;

    connectedCallback() {
        this.sortedArray = [...this.array];
    }

    handleSort() {
        this.isSorting = true;
        // Use setTimeout to allow UI to update
        setTimeout(() => {
            this.sortedArray = this.timSort([...this.array]);
            this.isSorting = false;
        }, 0);
    }

    // TimSort implementation
    timSort(arr) {
        const MIN_MERGE = 32;
        
        const n = arr.length;
        
        // Sort individual subarrays of size MIN_MERGE
        for (let i = 0; i < n; i += MIN_MERGE) {
            this.insertionSort(arr, i, Math.min((i + MIN_MERGE - 1), (n - 1)));
        }
        
        // Merge subarrays in bottom-up manner
        for (let size = MIN_MERGE; size < n; size = 2 * size) {
            for (let left = 0; left < n - size; left += 2 * size) {
                const mid = left + size - 1;
                const right = Math.min((left + size * 2 - 1), (n - 1));
                
                if (mid < right) {
                    this.merge(arr, left, mid, right);
                }
            }
        }
        
        return arr;
    }

    // Insertion sort for small arrays
    insertionSort(arr, left, right) {
        for (let i = left + 1; i <= right; i++) {
            const key = arr[i];
            let j = i - 1;
            
            while (j >= left && arr[j] > key) {
                arr[j + 1] = arr[j];
                j--;
            }
            arr[j + 1] = key;
        }
    }

    // Merge two sorted subarrays
    merge(arr, left, mid, right) {
        const leftArr = arr.slice(left, mid + 1);
        const rightArr = arr.slice(mid + 1, right + 1);
        
        let i = 0, j = 0, k = left;
        
        while (i < leftArr.length && j < rightArr.length) {
            if (leftArr[i] <= rightArr[j]) {
                arr[k] = leftArr[i];
                i++;
            } else {
                arr[k] = rightArr[j];
                j++;
            }
            k++;
        }
        
        while (i < leftArr.length) {
            arr[k] = leftArr[i];
            i++;
            k++;
        }
        
        while (j < rightArr.length) {
            arr[k] = rightArr[j];
            j++;
            k++;
        }
    }

    // Get array as string for display
    get arrayString() {
        return this.array.join(', ');
    }

    get sortedArrayString() {
        return this.sortedArray.join(', ');
    }
}
```

```html
<!-- timSort.html - Lightning Web Component HTML template -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                <div class="slds-media__body">
                    <h2 class="slds-card__header-title slds-truncate" title="TimSort Algorithm">
                        TimSort Algorithm Demo
                    </h2>
                </div>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <h3>Original Array</h3>
                    <p>{arrayString}</p>
                </div>
                <div class="slds-col slds-size_1-of-2">
                    <h3>Sorted Array</h3>
                    <p>{sortedArrayString}</p>
                </div>
            </div>
            
            <div class="slds-m-top_medium">
                <lightning-button 
                    label="Sort Array" 
                    variant="brand"
                    onclick={handleSort}
                    disabled={isSorting}>
                </lightning-button>
                
                <div class="slds-m-top_small">
                    <lightning-spinner 
                        if:true={isSorting} 
                        size="small" 
                        variant="brand">
                    </lightning-spinner>
                    <p if:true={isSorting}>Sorting in progress...</p>
                </div>
            </div>
            
            <div class="slds-m-top_medium">
                <h3>How TimSort Works:</h3>
                <ul class="slds-list_dotted">
                    <li>Divides array into small subarrays (min run size)</li>
                    <li>Sorts each subarray using insertion sort</li>
                    <li>Merges sorted subarrays using merge sort technique</li>
                    <li>Optimizes for partially sorted data</li>
                    <li>Time complexity: O(n log n)</li>
                    <li>Space complexity: O(n)</li>
                </ul>
            </div>
        </div>
    </div>
</template>
```

```css
/* timSort.css - Lightning Web Component CSS */
.slds-card__body {
    background-color: #f4f6f9;
}

.slds-card__header {
    background-color: #0070d2;
    color: white;
}

.slds-card__header-title {
    color: white;
}
```

## Key Features of this TimSort Implementation:

1. **MIN_MERGE Constant**: Uses 32 as the minimum run size for optimal performance
2. **Hybrid Approach**: Combines insertion sort for small arrays with merge sort for larger ones
3. **Optimized for Real-world Data**: Handles partially sorted arrays efficiently
4. **Interactive UI**: Shows before/after sorting with a sort button
5. **Performance Monitoring**: Includes loading state during sorting
6. **Educational Component**: Includes explanation of TimSort algorithm

## How TimSort Works:
- **Divide**: Breaks array into small runs (min 32 elements)
- **Conquer**: Sorts each run using insertion sort (efficient for small data)
- **Combine**: Merges runs together using merge sort technique
- **Optimize**: Takes advantage of existing order in data

This implementation demonstrates the practical application of TimSort in a Lightning Web Component, showing both the algorithm's functionality and its integration with Salesforce's UI framework.

