# Pancake Sorting in Lightning Web Component

Here's an example of implementing the Pancake Sorting algorithm in a Lightning Web Component:

```javascript
// pancakeSort.js
import { LightningElement } from 'lwc';

export default class PancakeSort extends LightningElement {
    array = [3, 1, 4, 1, 5, 9, 2, 6];
    sortedArray = [];
    steps = [];
    isSorting = false;

    connectedCallback() {
        this.sortedArray = [...this.array];
    }

    handleSort() {
        this.isSorting = true;
        this.steps = [];
        this.steps.push({
            array: [...this.sortedArray],
            message: "Starting Pancake Sort"
        });
        
        this.pancakeSort(this.sortedArray);
        this.isSorting = false;
    }

    pancakeSort(arr) {
        const n = arr.length;
        let flips = [];
        
        // Start from the end and work backwards
        for (let i = n - 1; i > 0; i--) {
            // Find the index of the maximum element in arr[0..i]
            let maxIndex = this.findMaxIndex(arr, i);
            
            // If the maximum element is not at the end, flip it to the beginning
            if (maxIndex !== 0) {
                this.flip(arr, maxIndex);
                flips.push({
                    array: [...arr],
                    message: `Flip at index ${maxIndex}: Bring max element to front`
                });
            }
            
            // Flip the entire array to move the maximum element to its correct position
            this.flip(arr, i);
            flips.push({
                array: [...arr],
                message: `Flip at index ${i}: Place max element at position ${i}`
            });
        }
        
        // Update steps with all the flip operations
        this.steps = [...this.steps, ...flips];
    }

    findMaxIndex(arr, end) {
        let maxIndex = 0;
        let maxValue = arr[0];
        
        for (let i = 1; i <= end; i++) {
            if (arr[i] > maxValue) {
                maxValue = arr[i];
                maxIndex = i;
            }
        }
        
        return maxIndex;
    }

    flip(arr, end) {
        let start = 0;
        while (start < end) {
            [arr[start], arr[end]] = [arr[end], arr[start]];
            start++;
            end--;
        }
    }

    handleReset() {
        this.sortedArray = [...this.array];
        this.steps = [];
    }
}
```

```html
<!-- pancakeSort.html -->
<template>
    <div class="container">
        <h2>Pancake Sorting Algorithm</h2>
        
        <div class="array-display">
            <h3>Original Array: [{array.join(', ')}]</h3>
            <h3>Current Array: [{sortedArray.join(', ')}]</h3>
        </div>

        <div class="controls">
            <lightning-button 
                label="Start Sorting" 
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

        <div class="steps" if:true={steps.length > 0}>
            <h3>Sorting Steps:</h3>
            <template for:each={steps} for:item="step">
                <div key={step.message} class="step-item">
                    <p><strong>{step.message}</strong></p>
                    <p>Array: [{step.array.join(', ')}]</p>
                </div>
            </template>
        </div>

        <div class="algorithm-info">
            <h3>How Pancake Sorting Works:</h3>
            <ul>
                <li>Find the largest unsorted element</li>
                <li>Flip it to the front if it's not already there</li>
                <li>Flip it to its correct position at the end</li>
                <li>Repeat for the remaining unsorted elements</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* pancakeSort.css */
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

.controls {
    margin: 20px 0;
    text-align: center;
}

.step-item {
    background-color: #e8f4f8;
    padding: 10px;
    margin: 10px 0;
    border-radius: 5px;
    border-left: 4px solid #0070d2;
}

.algorithm-info {
    background-color: #fff3cd;
    padding: 15px;
    border-radius: 8px;
    margin-top: 20px;
}

.algorithm-info ul {
    margin: 10px 0;
    padding-left: 20px;
}

.algorithm-info li {
    margin: 5px 0;
}
```

## How the Algorithm Works

1. **Find Maximum Element**: Locate the largest unsorted element in the array
2. **Flip to Front**: If the maximum element isn't already at the front, flip the array up to that element's position
3. **Flip to Correct Position**: Flip the entire unsorted portion to move the maximum element to its correct position at the end
4. **Repeat**: Continue this process for the remaining unsorted elements

## Key Features

- **Visual Step-by-Step Display**: Shows each flip operation and resulting array state
- **Interactive Controls**: Start sorting and reset functionality
- **Educational**: Clear explanation of the algorithm's working principles
- **Responsive Design**: Works well in Lightning experience

The algorithm has a time complexity of O(n²) and space complexity of O(n) in the worst case, where n is the number of elements in the array.

