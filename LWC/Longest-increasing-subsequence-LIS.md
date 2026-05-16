# Longest Increasing Subsequence (LIS) in Lightning Web Component

Here's a complete implementation of the LIS algorithm in a Lightning Web Component:

```javascript
// lis.js
import { LightningElement } from 'lwc';

export default class LisComponent extends LightningElement {
    inputArray = [10, 22, 9, 33, 21, 50, 41, 60, 80];
    result = '';
    lisLength = 0;
    lisSequence = [];

    handleCalculate() {
        const arr = this.inputArray;
        if (arr.length === 0) {
            this.result = 'Array is empty';
            return;
        }

        // Dynamic Programming approach for LIS
        const n = arr.length;
        const dp = new Array(n).fill(1); // dp[i] stores length of LIS ending at index i
        const parent = new Array(n).fill(-1); // To track the actual sequence

        // Fill dp array
        for (let i = 1; i < n; i++) {
            for (let j = 0; j < i; j++) {
                if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
                    dp[i] = dp[j] + 1;
                    parent[i] = j;
                }
            }
        }

        // Find the maximum length
        this.lisLength = Math.max(...dp);

        // Reconstruct the actual LIS sequence
        let maxLengthIndex = 0;
        for (let i = 1; i < n; i++) {
            if (dp[i] > dp[maxLengthIndex]) {
                maxLengthIndex = i;
            }
        }

        this.lisSequence = [];
        let current = maxLengthIndex;
        while (current !== -1) {
            this.lisSequence.unshift(arr[current]);
            current = parent[current];
        }

        this.result = `LIS Length: ${this.lisLength}, Sequence: [${this.lisSequence.join(', ')}]`;
    }

    handleInputChange(event) {
        const value = event.target.value;
        if (value.trim() === '') {
            this.inputArray = [];
        } else {
            this.inputArray = value.split(',').map(Number).filter(num => !isNaN(num));
        }
    }

    get displayArray() {
        return this.inputArray.join(', ');
    }
}
```

```html
<!-- lis.html -->
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <h2>Longest Increasing Subsequence (LIS)</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Input Array</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={displayArray}
                    onchange={handleInputChange}
                    placeholder="Enter comma-separated numbers (e.g., 10, 22, 9, 33)"
                />
            </div>
        </div>

        <lightning-button 
            label="Calculate LIS" 
            variant="brand" 
            onclick={handleCalculate}
            class="slds-m-top_small">
        </lightning-button>

        <div class="slds-m-top_small">
            <p><strong>Result:</strong> {result}</p>
        </div>

        <div class="slds-m-top_small">
            <h3>Algorithm Explanation:</h3>
            <ul class="slds-list_dotted">
                <li>Uses dynamic programming approach</li>
                <li>Time complexity: O(n²)</li>
                <li>Space complexity: O(n)</li>
                <li>Builds up solutions to subproblems</li>
                <li>For each element, finds the longest increasing subsequence ending at that element</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* lis.css */
.slds-box {
    background-color: #f4f6f9;
    border: 1px solid #d8dde6;
    border-radius: 0.25rem;
}

.slds-input {
    width: 100%;
}
```

## How it works:

1. **Input**: Takes a comma-separated list of numbers
2. **Algorithm**: Uses dynamic programming with O(n²) time complexity
3. **DP Array**: `dp[i]` stores the length of the longest increasing subsequence ending at index `i`
4. **Parent Tracking**: Keeps track of the actual sequence to reconstruct the LIS
5. **Reconstruction**: Traces back through parent array to build the actual subsequence

## Example:
- Input: `[10, 22, 9, 33, 21, 50, 41, 60, 80]`
- Output: `LIS Length: 6, Sequence: [10, 22, 33, 50, 60, 80]`

## Key Features:
- Interactive input field for custom arrays
- Clear algorithm explanation
- Visual feedback of results
- Handles edge cases (empty arrays)
- Reconstructs the actual subsequence, not just the length

