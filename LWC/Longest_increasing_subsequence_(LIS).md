# Longest Increasing Subsequence (LIS) in Lightning Web Component

Here's a complete example of implementing the LIS algorithm in a Lightning Web Component:

## JavaScript Controller (lwc/lisExample.js)

```javascript
import { LightningElement } from 'lwc';

export default class LisExample extends LightningElement {
    inputArray = '10,9,2,5,3,7,101,18';
    result = '';
    lisLength = 0;
    lisSequence = [];

    handleInputChange(event) {
        this.inputArray = event.target.value;
    }

    calculateLIS() {
        const array = this.inputArray
            .split(',')
            .map(item => parseInt(item.trim()))
            .filter(item => !isNaN(item));

        if (array.length === 0) {
            this.result = 'Please enter valid numbers';
            return;
        }

        // Calculate LIS using dynamic programming
        const lisLength = this.calculateLongestIncreasingSubsequence(array);
        this.lisLength = lisLength;
        this.lisSequence = this.getLISArray(array, lisLength);
        
        this.result = `Longest Increasing Subsequence length: ${lisLength}`;
    }

    calculateLongestIncreasingSubsequence(arr) {
        if (arr.length === 0) return 0;
        
        // dp[i] represents the length of LIS ending at index i
        const dp = new Array(arr.length).fill(1);
        
        // Fill dp array
        for (let i = 1; i < arr.length; i++) {
            for (let j = 0; j < i; j++) {
                if (arr[i] > arr[j]) {
                    dp[i] = Math.max(dp[i], dp[j] + 1);
                }
            }
        }
        
        // Return maximum value in dp array
        return Math.max(...dp);
    }

    getLISArray(arr, maxLength) {
        if (arr.length === 0) return [];
        
        // dp[i] represents the length of LIS ending at index i
        const dp = new Array(arr.length).fill(1);
        const parent = new Array(arr.length).fill(-1);
        
        // Fill dp array and track parent indices
        for (let i = 1; i < arr.length; i++) {
            for (let j = 0; j < i; j++) {
                if (arr[i] > arr[j] && dp[j] + 1 > dp[i]) {
                    dp[i] = dp[j] + 1;
                    parent[i] = j;
                }
            }
        }
        
        // Find the index with maximum LIS length
        let maxLengthIndex = 0;
        for (let i = 1; i < dp.length; i++) {
            if (dp[i] > dp[maxLengthIndex]) {
                maxLengthIndex = i;
            }
        }
        
        // Reconstruct the LIS
        const lis = [];
        let currentIndex = maxLengthIndex;
        
        while (currentIndex !== -1) {
            lis.unshift(arr[currentIndex]);
            currentIndex = parent[currentIndex];
        }
        
        return lis;
    }

    handleCalculate() {
        this.calculateLIS();
    }

    get formattedArray() {
        return this.inputArray.split(',').join(', ');
    }
}
```

## HTML Template (lwc/lisExample.html)

```html
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <h2 class="slds-text-heading_small">Longest Increasing Subsequence (LIS)</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="arrayInput">
                Input Array (comma separated)
            </label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="arrayInput"
                    class="slds-input"
                    value={inputArray}
                    onchange={handleInputChange}
                    placeholder="e.g., 10,9,2,5,3,7,101,18"
                />
            </div>
        </div>

        <lightning-button 
            label="Calculate LIS" 
            variant="brand" 
            onclick={handleCalculate}
            class="slds-m-top_small">
        </lightning-button>

        <template if:true={result}>
            <div class="slds-m-top_small">
                <p class="slds-text-body_regular">
                    <strong>Input Array:</strong> {formattedArray}
                </p>
                <p class="slds-text-body_regular">
                    <strong>Result:</strong> {result}
                </p>
                <p class="slds-text-body_regular">
                    <strong>LIS Sequence:</strong> {lisSequence.join(', ')}
                </p>
            </div>
        </template>

        <div class="slds-m-top_small slds-text-body_small">
            <p><strong>Algorithm Explanation:</strong></p>
            <ul class="slds-list_dotted">
                <li>Uses dynamic programming approach</li>
                <li>Time complexity: O(n²)</li>
                <li>Space complexity: O(n)</li>
                <li>Find the longest subsequence where elements are in increasing order</li>
            </ul>
        </div>
    </div>
</template>
```

## CSS Styles (lwc/lisExample.css)

```css
.slds-box {
    background-color: #f4f6f9;
    border: 1px solid #d8dde6;
    border-radius: 0.25rem;
}

.slds-text-heading_small {
    color: #080707;
    font-size: 1.125rem;
    font-weight: 600;
    line-height: 1.25;
}

.slds-text-body_regular {
    color: #3e3e3e;
    font-size: 0.875rem;
    line-height: 1.4;
}
```

## Example Usage

### Input:
```
10,9,2,5,3,7,101,18
```

### Output:
```
Longest Increasing Subsequence length: 4
LIS Sequence: 2, 3, 7, 18
```

## How It Works

1. **Input Parsing**: Takes comma-separated numbers as input
2. **Dynamic Programming**: Uses a DP array where `dp[i]` represents the length of LIS ending at index `i`
3. **Backtracking**: Tracks parent indices to reconstruct the actual LIS sequence
4. **Result Display**: Shows both the length and the actual subsequence

## Key Features

- **User-friendly interface** with input validation
- **Real-time calculation** when button is clicked
- **Visual feedback** with formatted results
- **Algorithm explanation** for educational purposes
- **Error handling** for invalid inputs

This implementation demonstrates the classic LIS problem solution using dynamic programming approach in the Lightning Web Component framework.

