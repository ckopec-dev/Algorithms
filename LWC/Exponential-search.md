# Exponential Search in Lightning Web Component

Here's an example implementation of the Exponential Search algorithm in Lightning Web Component:

```javascript
// exponentialSearch.js
import { LightningElement } from 'lwc';

export default class ExponentialSearch extends LightningElement {
    searchInput = '';
    arrayInput = '';
    result = '';
    searchResult = -1;

    handleArrayInput(event) {
        this.arrayInput = event.target.value;
    }

    handleSearchInput(event) {
        this.searchInput = event.target.value;
    }

    performExponentialSearch() {
        // Parse input arrays
        const array = this.arrayInput.split(',').map(Number).filter(num => !isNaN(num));
        const searchElement = parseInt(this.searchInput);

        // Validate inputs
        if (array.length === 0 || isNaN(searchElement)) {
            this.result = 'Please enter valid array and search element';
            return;
        }

        // Sort array for binary search (exponential search requires sorted array)
        array.sort((a, b) => a - b);

        // Perform exponential search
        const resultIndex = this.exponentialSearch(array, searchElement);

        if (resultIndex !== -1) {
            this.result = `Element found at index: ${resultIndex}`;
        } else {
            this.result = 'Element not found in array';
        }

        this.searchResult = resultIndex;
    }

    /**
     * Exponential Search Algorithm
     * @param {number[]} arr - Sorted array to search in
     * @param {number} target - Element to search for
     * @returns {number} - Index of element if found, -1 otherwise
     */
    exponentialSearch(arr, target) {
        // If target is first element
        if (arr[0] === target) {
            return 0;
        }

        // Find range where element is present
        let bound = 1;
        while (bound < arr.length && arr[bound] < target) {
            bound *= 2;
        }

        // Perform binary search on the found range
        return this.binarySearch(arr, target, bound / 2, Math.min(bound, arr.length - 1));
    }

    /**
     * Binary Search helper function
     * @param {number[]} arr - Sorted array
     * @param {number} target - Element to search for
     * @param {number} left - Left boundary
     * @param {number} right - Right boundary
     * @returns {number} - Index of element if found, -1 otherwise
     */
    binarySearch(arr, target, left, right) {
        while (left <= right) {
            const mid = Math.floor((left + right) / 2);

            if (arr[mid] === target) {
                return mid;
            } else if (arr[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return -1;
    }

    get formattedArray() {
        return this.arrayInput || 'Enter array elements separated by commas';
    }

    get formattedSearchElement() {
        return this.searchInput || 'Enter search element';
    }
}
```

```html
<!-- exponentialSearch.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Exponential Search Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="arrayInput">Array (comma-separated)</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="arrayInput"
                    class="slds-input"
                    value={arrayInput}
                    onchange={handleArrayInput}
                    placeholder="e.g., 1,3,5,7,9,11,13,15"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label" for="searchInput">Search Element</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    id="searchInput"
                    class="slds-input"
                    value={searchInput}
                    onchange={handleSearchInput}
                    placeholder="Enter number to search"
                />
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Search" 
                variant="brand" 
                onclick={performExponentialSearch}
                class="slds-m-bottom_medium"
            ></lightning-button>
        </div>

        <div class="slds-box slds-theme_info">
            <h3>Result:</h3>
            <p>{result}</p>
        </div>

        <div class="slds-m-top_medium">
            <h3>Algorithm Explanation:</h3>
            <ul class="slds-list_dotted">
                <li>Start with index 1</li>
                <li>Compare target with element at index 1</li>
                <li>If target is greater, double the index (1, 2, 4, 8, 16...)</li>
                <li>Continue until target is found or array boundary is reached</li>
                <li>Perform binary search in the identified range</li>
                <li>Time Complexity: O(log n)</li>
                <li>Space Complexity: O(1)</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* exponentialSearch.css */
.slds-box {
    padding: 1rem;
    margin: 1rem 0;
}

.slds-input {
    width: 100%;
    margin-bottom: 1rem;
}

.slds-button {
    margin-right: 0.5rem;
}

.slds-list_dotted {
    padding-left: 1rem;
}
```

## How it works:

1. **Input Handling**: Users enter a comma-separated array and a search element
2. **Validation**: Checks for valid inputs and sorts the array
3. **Exponential Search**: 
   - Starts with index 1
   - Doubles the index until finding a range where the target might be
   - Uses binary search within that range
4. **Result Display**: Shows whether element was found and its index

## Key Features:
- **Time Complexity**: O(log n) - more efficient than linear search for large sorted arrays
- **Space Complexity**: O(1) - constant space usage
- **User-Friendly**: Interactive input with clear result display
- **Error Handling**: Validates inputs and provides meaningful error messages

## Example Usage:
- Input Array: `1,3,5,7,9,11,13,15`
- Search Element: `7`
- Output: `Element found at index: 3`

The algorithm is particularly useful when dealing with unbounded or very large sorted arrays where the target element is likely to be near the beginning.

