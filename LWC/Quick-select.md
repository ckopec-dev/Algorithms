# Quick Select Algorithm in Lightning Web Component

```javascript
// quickSelect.js
import { LightningElement } from 'lwc';

export default class QuickSelect extends LightningElement {
    numbers = [3, 6, 8, 10, 1, 2, 1];
    k = 3;
    result = null;

    handleQuickSelect() {
        const arr = [...this.numbers];
        const kth = this.k;
        this.result = this.quickSelect(arr, 0, arr.length - 1, kth - 1);
    }

    quickSelect(arr, low, high, k) {
        if (low === high) {
            return arr[low];
        }

        // Partition the array and get pivot index
        const pivotIndex = this.partition(arr, low, high);

        if (k === pivotIndex) {
            return arr[k];
        } else if (k < pivotIndex) {
            return this.quickSelect(arr, low, pivotIndex - 1, k);
        } else {
            return this.quickSelect(arr, pivotIndex + 1, high, k);
        }
    }

    partition(arr, low, high) {
        // Choose the rightmost element as pivot
        const pivot = arr[high];
        let i = low - 1; // Index of smaller element

        for (let j = low; j < high; j++) {
            // If current element is smaller than or equal to pivot
            if (arr[j] <= pivot) {
                i++;
                this.swap(arr, i, j);
            }
        }

        // Place pivot in its correct position
        this.swap(arr, i + 1, high);
        return i + 1;
    }

    swap(arr, i, j) {
        const temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    get displayNumbers() {
        return this.numbers.join(', ');
    }

    get displayResult() {
        return this.result !== null ? `The ${this.k}th smallest element is: ${this.result}` : '';
    }
}
```

```html
<!-- quickSelect.html -->
<template>
    <div class="container">
        <h2>Quick Select Algorithm</h2>
        
        <div class="input-section">
            <p>Array: {displayNumbers}</p>
            <p>Find the Kth smallest element (K):</p>
            <lightning-input 
                type="number" 
                value={k} 
                onchange={handleKChange}
                min="1"
                max="7">
            </lightning-input>
        </div>

        <lightning-button 
            label="Find Kth Smallest" 
            onclick={handleQuickSelect}
            variant="brand">
        </lightning-button>

        <div class="result-section">
            <p if:true={result}>{displayResult}</p>
        </div>
    </div>
</template>
```

```css
/* quickSelect.css */
.container {
    padding: 20px;
    max-width: 600px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 8px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #e8f5e8;
    border-radius: 8px;
    border-left: 4px solid #4caf50;
}

.lightning-button {
    margin-top: 15px;
}
```

## How it works:

1. **Quick Select Algorithm**: Finds the Kth smallest element in an unsorted array
2. **Time Complexity**: O(n) average case, O(n²) worst case
3. **Space Complexity**: O(log n) due to recursion
4. **Key Features**:
   - Uses partitioning similar to QuickSort
   - Only recurses into the part containing the desired element
   - More efficient than sorting the entire array

## Example Usage:
- Input array: [3, 6, 8, 10, 1, 2, 1]
- K = 3 (find 3rd smallest)
- Output: 2 (the 3rd smallest element)

The algorithm efficiently finds the Kth smallest element without fully sorting the array, making it optimal for this specific problem.

