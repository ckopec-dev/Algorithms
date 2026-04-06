# Counting Sort in Lightning Web Component

Here's an example of implementing the Counting Sort algorithm in a Lightning Web Component:

## HTML Template (countingSort.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Counting Sort Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Enter numbers (comma separated):</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input" 
                    value={inputNumbers} 
                    onchange={handleInputChange}
                    placeholder="e.g., 4, 2, 2, 8, 3, 3, 1">
            </div>
        </div>
        
        <button class="slds-button slds-button_brand" onclick={sortNumbers}>
            Sort Numbers
        </button>
        
        <div if:true={sortedNumbers.length > 0} class="slds-m-top_medium">
            <h3>Sorted Result:</h3>
            <p>{sortedNumbers}</p>
        </div>
        
        <div if:true={executionTime} class="slds-m-top_medium">
            <h3>Execution Time:</h3>
            <p>{executionTime} ms</p>
        </div>
    </div>
</template>
```

## JavaScript Controller (countingSort.js)
```javascript
import { LightningElement } from 'lwc';

export default class CountingSort extends LightningElement {
    inputNumbers = '';
    sortedNumbers = '';
    executionTime = null;

    handleInputChange(event) {
        this.inputNumbers = event.target.value;
    }

    sortNumbers() {
        if (!this.inputNumbers.trim()) {
            return;
        }

        // Parse input numbers
        const numbers = this.inputNumbers
            .split(',')
            .map(num => parseInt(num.trim()))
            .filter(num => !isNaN(num));

        if (numbers.length === 0) {
            return;
        }

        // Measure execution time
        const startTime = performance.now();

        // Perform counting sort
        const sorted = this.countingSort(numbers);

        const endTime = performance.now();
        this.executionTime = (endTime - startTime).toFixed(4);

        // Display result
        this.sortedNumbers = sorted.join(', ');
    }

    countingSort(arr) {
        if (arr.length <= 1) {
            return arr;
        }

        // Find the maximum and minimum values
        let max = Math.max(...arr);
        let min = Math.min(...arr);
        
        // Create counting array
        const range = max - min + 1;
        const count = new Array(range).fill(0);
        
        // Count occurrences of each element
        for (let i = 0; i < arr.length; i++) {
            count[arr[i] - min]++;
        }
        
        // Reconstruct sorted array
        const sorted = [];
        for (let i = 0; i < range; i++) {
            while (count[i] > 0) {
                sorted.push(i + min);
                count[i]--;
            }
        }
        
        return sorted;
    }
}
```

## CSS Styles (countingSort.css)
```css
.slds-box {
    padding: 1rem;
    margin: 1rem 0;
}

.slds-input {
    width: 100%;
    max-width: 400px;
}

.slds-button {
    margin-top: 1rem;
}

.slds-m-top_medium {
    margin-top: 1rem;
}
```

## How it works:

1. **Input**: User enters comma-separated numbers
2. **Parsing**: Numbers are converted to integers
3. **Counting Sort Algorithm**:
   - Find the range (max - min + 1)
   - Create a counting array of that size
   - Count occurrences of each element
   - Reconstruct the sorted array from counts
4. **Output**: Display sorted numbers and execution time

## Example Usage:
- Input: `4, 2, 2, 8, 3, 3, 1`
- Output: `1, 2, 2, 3, 3, 4, 8`
- Time Complexity: O(n + k) where n is array size and k is range
- Space Complexity: O(k)

This implementation handles positive and negative integers and provides performance measurement for educational purposes.

