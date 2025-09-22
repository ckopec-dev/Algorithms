# Jump Search Algorithm in Lightning Web Component

Here's a complete example of implementing the Jump Search algorithm in a Lightning Web Component:

## HTML Template (jumpSearch.html)
```html
<template>
    <div class="slds-box slds-box_small slds-theme_default">
        <h2>Jump Search Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Enter array elements (comma separated)</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={inputArray}
                    onchange={handleArrayChange}
                    placeholder="e.g., 1,3,5,7,9,11,13,15,17,19"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Enter search element</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    class="slds-input"
                    value={searchElement}
                    onchange={handleSearchChange}
                    placeholder="Enter number to search"
                />
            </div>
        </div>

        <button class="slds-button slds-button_brand" onclick={performJumpSearch}>
            Perform Jump Search
        </button>

        <template if:true={result}>
            <div class="slds-alert slds-alert_success slds-m-top_medium">
                <span class="slds-alert_icon slds-icon slds-icon_check slds-icon_x-small">
                    <svg class="slds-icon slds-icon-text-success" aria-hidden="true">
                        <use href="/assets/icons/utility-sprite/svg/symbols.svg#check"></use>
                    </svg>
                </span>
                <div class="slds-alert__body">
                    <p>Element found at index: {result}</p>
                </div>
            </div>
        </template>

        <template if:true={error}>
            <div class="slds-alert slds-alert_error slds-m-top_medium">
                <span class="slds-alert_icon slds-icon slds-icon_error slds-icon_x-small">
                    <svg class="slds-icon slds-icon-text-error" aria-hidden="true">
                        <use href="/assets/icons/utility-sprite/svg/symbols.svg#error"></use>
                    </svg>
                </span>
                <div class="slds-alert__body">
                    <p>{error}</p>
                </div>
            </div>
        </template>

        <template if:true={searchSteps}>
            <div class="slds-box slds-box_small slds-theme_alert-texture slds-m-top_medium">
                <h3>Search Steps:</h3>
                <ul class="slds-list_dotted">
                    <template for:each={searchSteps} for:item="step">
                        <li key={step.index}>{step.description}</li>
                    </template>
                </ul>
            </div>
        </template>
    </div>
</template>
```

## JavaScript Controller (jumpSearch.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class JumpSearch extends LightningElement {
    @track inputArray = '';
    @track searchElement = '';
    @track result = null;
    @track error = null;
    @track searchSteps = [];

    // Handle array input change
    handleArrayChange(event) {
        this.inputArray = event.target.value;
        this.clearResults();
    }

    // Handle search element change
    handleSearchChange(event) {
        this.searchElement = event.target.value;
        this.clearResults();
    }

    // Clear previous results
    clearResults() {
        this.result = null;
        this.error = null;
        this.searchSteps = [];
    }

    // Perform jump search algorithm
    performJumpSearch() {
        try {
            // Parse input array
            if (!this.inputArray.trim()) {
                throw new Error('Please enter array elements');
            }

            const array = this.parseArray(this.inputArray);
            
            // Validate array is sorted
            if (!this.isSorted(array)) {
                throw new Error('Array must be sorted for jump search algorithm');
            }

            // Parse search element
            if (!this.searchElement && this.searchElement !== 0) {
                throw new Error('Please enter a search element');
            }

            const searchValue = parseInt(this.searchElement);
            
            // Perform jump search
            const result = this.jumpSearch(array, searchValue);
            
            this.result = result.index;
            this.searchSteps = result.steps;

        } catch (error) {
            this.error = error.message;
        }
    }

    // Parse comma-separated string to array
    parseArray(inputString) {
        return inputString.split(',').map(item => {
            const parsed = parseInt(item.trim());
            if (isNaN(parsed)) {
                throw new Error('Invalid number in array');
            }
            return parsed;
        });
    }

    // Check if array is sorted in ascending order
    isSorted(array) {
        for (let i = 1; i < array.length; i++) {
            if (array[i] < array[i - 1]) {
                return false;
            }
        }
        return true;
    }

    // Jump Search Algorithm Implementation
    jumpSearch(array, searchValue) {
        const n = array.length;
        let steps = [];
        
        // Handle edge cases
        if (n === 0) {
            return { index: -1, steps: [{ index: -1, description: 'Array is empty' }] };
        }

        // Calculate block size (jump size)
        const jumpSize = Math.floor(Math.sqrt(n));
        steps.push({ 
            index: -1, 
            description: `Jump size calculated as √${n} ≈ ${jumpSize}` 
        });

        let previousBlock = 0;
        let currentBlock = jumpSize;

        // Jump through blocks
        steps.push({ 
            index: -1, 
            description: `Starting from index 0, jumping by ${jumpSize} positions` 
        });

        while (currentBlock < n && array[currentBlock] < searchValue) {
            previousBlock = currentBlock;
            currentBlock += jumpSize;
            steps.push({ 
                index: currentBlock, 
                description: `Jumping to index ${currentBlock}` 
            });
        }

        // Linear search in the block
        steps.push({ 
            index: -1, 
            description: `Searching linearly in block [${previousBlock}, ${Math.min(currentBlock, n) - 1}]` 
        });

        for (let i = previousBlock; i < Math.min(currentBlock, n); i++) {
            steps.push({ 
                index: i, 
                description: `Checking element at index ${i}: ${array[i]}` 
            });
            
            if (array[i] === searchValue) {
                return { 
                    index: i, 
                    steps: steps 
                };
            }
        }

        // Element not found
        steps.push({ 
            index: -1, 
            description: `Element ${searchValue} not found in array` 
        });
        
        return { 
            index: -1, 
            steps: steps 
        };
    }
}
```

## CSS Styles (jumpSearch.css)
```css
.slds-box {
    background-color: #ffffff;
    border: 1px solid #e5e5e5;
    border-radius: 0.25rem;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    padding: 1rem;
}

.slds-form-element {
    margin-bottom: 1rem;
}

.slds-input {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #e5e5e5;
    border-radius: 0.25rem;
    font-size: 0.875rem;
}

.slds-button {
    margin-top: 1rem;
}

.slds-alert {
    margin-top: 1rem;
}

.slds-list_dotted {
    padding-left: 1rem;
}
```

## How it works:

1. **Input**: User enters a comma-separated sorted array and a search element
2. **Algorithm**: 
   - Calculates jump size as √n (square root of array length)
   - Jumps through blocks until finding the correct block
   - Performs linear search within that block
3. **Output**: Shows the index where element is found or indicates not found
4. **Visualization**: Displays step-by-step execution of the algorithm

## Example Usage:
- Input Array: `1,3,5,7,9,11,13,15,17,19`
- Search Element: `11`
- Output: Element found at index: `5`

This implementation demonstrates the jump search algorithm with clear visualization of each step in the Lightning Web Component framework.

