# Cycle Sort in Lightning Web Component

Here's an example implementation of the Cycle Sort algorithm in a Lightning Web Component:

```javascript
// cycleSort.js
import { LightningElement } from 'lwc';

export default class CycleSort extends LightningElement {
    inputArray = [5, 2, 8, 1, 9, 3];
    sortedArray = [];
    steps = [];
    isSorting = false;

    connectedCallback() {
        this.sortedArray = [...this.inputArray];
        this.steps = [];
        this.steps.push({
            array: [...this.inputArray],
            message: "Initial array"
        });
    }

    handleSort() {
        this.isSorting = true;
        this.sortedArray = [...this.inputArray];
        this.steps = [];
        this.steps.push({
            array: [...this.inputArray],
            message: "Initial array"
        });
        
        this.cycleSort(this.sortedArray);
        this.steps.push({
            array: [...this.sortedArray],
            message: "Final sorted array"
        });
        this.isSorting = false;
    }

    cycleSort(array) {
        let writes = 0;
        const n = array.length;

        // Loop through the array elements
        for (let cycleStart = 0; cycleStart < n - 1; cycleStart++) {
            let item = array[cycleStart];
            let pos = cycleStart;

            // Find the position where we put the item
            for (let i = cycleStart + 1; i < n; i++) {
                if (array[i] < item) {
                    pos++;
                }
            }

            // If item is already in correct position
            if (pos === cycleStart) {
                continue;
            }

            // Skip duplicates
            while (item === array[pos]) {
                pos++;
            }

            // Put the item to its correct position
            if (pos !== cycleStart) {
                [array[pos], item] = [item, array[pos]];
                writes++;
                this.steps.push({
                    array: [...array],
                    message: `Moved ${item} to position ${pos}`
                });
            }

            // Rotate the rest of the cycle
            while (pos !== cycleStart) {
                pos = cycleStart;
                for (let i = cycleStart + 1; i < n; i++) {
                    if (array[i] < item) {
                        pos++;
                    }
                }

                while (item === array[pos]) {
                    pos++;
                }

                if (item !== array[pos]) {
                    [array[pos], item] = [item, array[pos]];
                    writes++;
                    this.steps.push({
                        array: [...array],
                        message: `Moved ${item} to position ${pos}`
                    });
                }
            }
        }
    }

    handleReset() {
        this.sortedArray = [...this.inputArray];
        this.steps = [];
        this.steps.push({
            array: [...this.inputArray],
            message: "Initial array"
        });
    }

    get stepCount() {
        return this.steps.length;
    }
}
```

```html
<!-- cycleSort.html -->
<template>
    <div class="container">
        <h2>Cycle Sort Algorithm</h2>
        
        <div class="controls">
            <lightning-button 
                label="Sort Array" 
                variant="brand" 
                onclick={handleSort}
                disabled={isSorting}>
            </lightning-button>
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <div class="array-display">
            <h3>Original Array: [{inputArray.join(', ')}]</h3>
            <h3>Sorted Array: [{sortedArray.join(', ')}]</h3>
        </div>

        <div class="steps">
            <h3>Sorting Steps ({stepCount})</h3>
            <template for:each={steps} for:item="step">
                <div key={step.message} class="step">
                    <p><strong>{step.message}</strong></p>
                    <p>Array: [{step.array.join(', ')}]</p>
                </div>
            </template>
        </div>

        <div class="algorithm-info">
            <h3>How Cycle Sort Works:</h3>
            <ul>
                <li>Find the correct position for each element</li>
                <li>Perform cyclic rotations to place elements in their correct positions</li>
                <li>Minimizes the number of writes to the array</li>
                <li>Time complexity: O(n²)</li>
                <li>Space complexity: O(1)</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* cycleSort.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.controls {
    margin: 20px 0;
    text-align: center;
}

.array-display {
    margin: 20px 0;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.step {
    margin: 10px 0;
    padding: 10px;
    background-color: #e8f4f8;
    border-radius: 3px;
    border-left: 4px solid #0070d2;
}

.algorithm-info {
    margin-top: 30px;
    padding: 15px;
    background-color: #fff3cd;
    border-radius: 5px;
    border-left: 4px solid #ffc107;
}

.algorithm-info ul {
    margin: 10px 0;
    padding-left: 20px;
}

.algorithm-info li {
    margin: 5px 0;
}
```

## How the Algorithm Works:

1. **Cycle Detection**: The algorithm identifies cycles in the array where elements need to be moved
2. **Position Finding**: For each element, it determines the correct position in the sorted array
3. **Cyclic Rotation**: Elements are moved through cyclic rotations to their correct positions
4. **Minimize Writes**: The algorithm minimizes the number of write operations compared to other sorting algorithms

## Key Features:

- **Visual Step-by-Step**: Shows each step of the sorting process
- **Interactive**: Users can sort and reset the array
- **Performance Info**: Displays algorithm complexity information
- **Responsive Design**: Works well in Lightning Experience

This implementation demonstrates how Cycle Sort works by showing the actual movements of elements through each cycle, making it easy to understand the algorithm's behavior.

