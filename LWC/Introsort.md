# Introsort Algorithm in Lightning Web Component

```javascript
// introsort.js
import { LightningElement } from 'lwc';

export default class IntrosortExample extends LightningElement {
    array = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42];
    sortedArray = [];
    isSorting = false;

    connectedCallback() {
        this.sortedArray = [...this.array];
    }

    handleSort() {
        this.isSorting = true;
        this.sortedArray = [...this.array];
        
        // Perform introsort
        this.introsort(this.sortedArray, 0, this.sortedArray.length - 1, 
                      Math.floor(Math.log2(this.sortedArray.length)));
        
        this.isSorting = false;
    }

    introsort(array, low, high, maxDepth) {
        if (high - low + 1 <= 16) {
            // Use insertion sort for small arrays
            this.insertionSort(array, low, high);
        } else if (maxDepth === 0) {
            // Use heapsort when max depth reached
            this.heapSort(array, low, high);
        } else {
            // Use quicksort and recursively call introsort
            const pivot = this.partition(array, low, high);
            this.introsort(array, low, pivot - 1, maxDepth - 1);
            this.introsort(array, pivot + 1, high, maxDepth - 1);
        }
    }

    partition(array, low, high) {
        const pivot = array[high];
        let i = low - 1;

        for (let j = low; j < high; j++) {
            if (array[j] <= pivot) {
                i++;
                [array[i], array[j]] = [array[j], array[i]]; // Swap elements
            }
        }
        [array[i + 1], array[high]] = [array[high], array[i + 1]]; // Swap pivot
        return i + 1;
    }

    insertionSort(array, low, high) {
        for (let i = low + 1; i <= high; i++) {
            const key = array[i];
            let j = i - 1;
            
            while (j >= low && array[j] > key) {
                array[j + 1] = array[j];
                j--;
            }
            array[j + 1] = key;
        }
    }

    heapSort(array, low, high) {
        const n = high - low + 1;
        const arr = array.slice(low, high + 1);

        // Build max heap
        for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
            this.heapify(arr, n, i);
        }

        // Extract elements from heap one by one
        for (let i = n - 1; i > 0; i--) {
            [arr[0], arr[i]] = [arr[i], arr[0]]; // Move current root to end
            this.heapify(arr, i, 0); // Call heapify on the reduced heap
        }

        // Copy back to original array
        for (let i = 0; i < n; i++) {
            array[low + i] = arr[i];
        }
    }

    heapify(array, n, i) {
        let largest = i;
        const left = 2 * i + 1;
        const right = 2 * i + 2;

        if (left < n && array[left] > array[largest]) {
            largest = left;
        }

        if (right < n && array[right] > array[largest]) {
            largest = right;
        }

        if (largest !== i) {
            [array[i], array[largest]] = [array[largest], array[i]];
            this.heapify(array, n, largest);
        }
    }

    get sortedArrayString() {
        return this.sortedArray.join(', ');
    }

    get arrayString() {
        return this.array.join(', ');
    }
}
```

```html
<!-- introsort.html -->
<template>
    <div class="container">
        <h2>Introsort Algorithm Demo</h2>
        
        <div class="array-display">
            <p><strong>Original Array:</strong> {arrayString}</p>
            <p><strong>Sorted Array:</strong> {sortedArrayString}</p>
        </div>

        <lightning-button 
            label="Sort Array" 
            variant="brand" 
            onclick={handleSort}
            disabled={isSorting}>
        </lightning-button>

        <div class="algorithm-info">
            <h3>How Introsort Works:</h3>
            <ul>
                <li>Uses quicksort for large arrays</li>
                <li>Switches to heapsort when recursion depth exceeds limit</li>
                <li>Uses insertion sort for small arrays (&lt;= 16 elements)</li>
                <li>Avoids quicksort's worst-case O(n²) performance</li>
                <li>Guarantees O(n log n) time complexity</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* introsort.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.array-display {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 8px;
    margin: 20px 0;
}

.array-display p {
    margin: 10px 0;
    font-family: monospace;
    font-size: 16px;
}

.algorithm-info {
    background-color: #e8f4f8;
    padding: 15px;
    border-radius: 8px;
    margin-top: 20px;
}

.algorithm-info h3 {
    margin-top: 0;
}

.algorithm-info ul {
    margin: 10px 0;
    padding-left: 20px;
}

.algorithm-info li {
    margin: 5px 0;
}
```

## Algorithm Explanation

The Introsort algorithm combines three sorting techniques:

1. **Quicksort**: Used for large arrays with good average performance
2. **Heapsort**: Switched to when recursion depth exceeds a threshold to avoid worst-case scenarios
3. **Insertion Sort**: Used for small arrays (≤ 16 elements) due to low overhead

**Key Features:**
- **Time Complexity**: O(n log n) guaranteed
- **Space Complexity**: O(log n) 
- **Adaptive**: Switches between algorithms based on conditions
- **Practical**: Combines the best properties of different sorting algorithms

This implementation demonstrates how Introsort can provide consistent performance while maintaining the efficiency of quicksort for most cases.

