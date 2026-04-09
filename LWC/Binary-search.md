# Binary Search in Lightning Web Component

Here's an example of implementing binary search algorithm in a Lightning Web Component:

## HTML Template (binarySearch.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Binary Search Example</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Enter numbers (comma separated)</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input" 
                    value={inputNumbers} 
                    onchange={handleNumbersChange}
                    placeholder="e.g., 1,3,5,7,9,11,13,15"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Search for number</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    class="slds-input" 
                    value={searchNumber} 
                    onchange={handleSearchChange}
                    placeholder="Enter number to search"
                />
            </div>
        </div>

        <lightning-button 
            label="Search" 
            variant="brand" 
            onclick={performBinarySearch}
            disabled={isSearchDisabled}>
        </lightning-button>

        <div class="slds-m-top_medium">
            <lightning-card title="Result">
                <div class="slds-text-body_regular">
                    <p><strong>Search Result:</strong> {searchResult}</p>
                    <p><strong>Index:</strong> {searchIndex}</p>
                    <p><strong>Iterations:</strong> {iterations}</p>
                </div>
            </lightning-card>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Array Visualization">
                <div class="slds-grid slds-gutters slds-wrap">
                    <template for:each={sortedNumbers} for:item="number">
                        <div key={number.value} class="slds-col slds-size_1-of-8">
                            <div class={number.cssClass}>
                                {number.value}
                            </div>
                        </div>
                    </template>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (binarySearch.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class BinarySearch extends LightningElement {
    @track inputNumbers = '';
    @track searchNumber = '';
    @track sortedNumbers = [];
    @track searchResult = '';
    @track searchIndex = -1;
    @track iterations = 0;
    @track isSearchDisabled = true;

    handleNumbersChange(event) {
        this.inputNumbers = event.target.value;
        this.validateInput();
    }

    handleSearchChange(event) {
        this.searchNumber = event.target.value;
        this.validateInput();
    }

    validateInput() {
        this.isSearchDisabled = !this.inputNumbers || !this.searchNumber;
    }

    performBinarySearch() {
        try {
            // Parse and sort the input numbers
            const numbers = this.parseNumbers(this.inputNumbers);
            this.sortedNumbers = this.sortNumbers(numbers);
            
            // Perform binary search
            const result = this.binarySearch(
                this.sortedNumbers, 
                parseInt(this.searchNumber)
            );
            
            this.searchResult = result.found ? 'Found' : 'Not Found';
            this.searchIndex = result.index;
            this.iterations = result.iterations;
            
        } catch (error) {
            this.searchResult = 'Error: ' + error.message;
            this.searchIndex = -1;
            this.iterations = 0;
        }
    }

    parseNumbers(input) {
        if (!input) return [];
        return input.split(',').map(num => parseInt(num.trim())).filter(num => !isNaN(num));
    }

    sortNumbers(numbers) {
        // Create a copy and sort
        const sorted = [...numbers].sort((a, b) => a - b);
        
        // Create array with CSS classes for visualization
        return sorted.map((value, index) => ({
            value: value,
            cssClass: 'slds-text-align_center slds-box slds-theme_default'
        }));
    }

    binarySearch(array, target) {
        let left = 0;
        let right = array.length - 1;
        let iterations = 0;
        let found = false;
        let index = -1;

        while (left <= right) {
            iterations++;
            const mid = Math.floor((left + right) / 2);
            
            if (array[mid].value === target) {
                found = true;
                index = mid;
                break;
            } else if (array[mid].value < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }

        return {
            found: found,
            index: index,
            iterations: iterations
        };
    }
}
```

## CSS Styles (binarySearch.css)
```css
.slds-box {
    border: 1px solid #e1e1e1;
    border-radius: 0.25rem;
    padding: 1rem;
}

.slds-text-align_center {
    text-align: center;
}

.slds-theme_default {
    background-color: #ffffff;
    border: 1px solid #e1e1e1;
}
```

## Usage Example

1. **Input Numbers**: Enter comma-separated numbers like: `1,3,5,7,9,11,13,15`
2. **Search Number**: Enter the number you want to find like: `7`
3. **Click Search**: The component will show:
   - Whether the number was found
   - The index position (0-based)
   - Number of iterations performed

## Key Features

- **Input Validation**: Ensures both input fields are filled
- **Array Sorting**: Automatically sorts input numbers
- **Visualization**: Shows the sorted array with visual indicators
- **Performance Metrics**: Displays number of iterations
- **Error Handling**: Gracefully handles invalid inputs
- **Responsive Design**: Uses Lightning Design System classes

## How Binary Search Works

The algorithm:
1. Compares target with middle element
2. If target equals middle, return index
3. If target is greater, search right half
4. If target is smaller, search left half
5. Repeat until found or search space is empty

Time Complexity: O(log n)  
Space Complexity: O(1)

