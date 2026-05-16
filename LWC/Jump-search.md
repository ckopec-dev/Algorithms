# Jump Search Algorithm in Lightning Web Component

```javascript
// jumpSearch.js - Lightning Web Component JavaScript file

import { LightningElement } from 'lwc';

export default class JumpSearch extends LightningElement {
    searchArray = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29];
    searchValue = '';
    result = '';
    searchSteps = [];

    handleSearch() {
        const value = parseInt(this.searchValue);
        if (isNaN(value)) {
            this.result = 'Please enter a valid number';
            return;
        }

        this.searchSteps = [];
        const index = this.jumpSearch(this.searchArray, value);
        
        if (index !== -1) {
            this.result = `Element found at index: ${index}`;
        } else {
            this.result = 'Element not found in array';
        }
    }

    jumpSearch(arr, target) {
        const n = arr.length;
        let step = Math.floor(Math.sqrt(n));
        let prev = 0;

        // Record the first step
        this.searchSteps.push({
            step: 'Initial jump',
            prev: prev,
            stepSize: step,
            current: step
        });

        // Jumping through the array
        while (arr[Math.min(step, n) - 1] < target) {
            prev = step;
            step += Math.floor(Math.sqrt(n));
            
            // Record each jump step
            this.searchSteps.push({
                step: 'Jump',
                prev: prev,
                stepSize: step,
                current: Math.min(step, n)
            });

            if (prev >= n) {
                return -1;
            }
        }

        // Linear search in the block
        this.searchSteps.push({
            step: 'Linear search in block',
            prev: prev,
            stepSize: step,
            current: prev
        });

        // Linear search in the identified block
        while (arr[prev] < target) {
            prev++;
            
            this.searchSteps.push({
                step: 'Checking element',
                prev: prev,
                stepSize: step,
                current: prev
            });

            if (prev === Math.min(step, n)) {
                return -1;
            }
        }

        if (arr[prev] === target) {
            return prev;
        }

        return -1;
    }

    handleInputChange(event) {
        this.searchValue = event.target.value;
    }

    get arrayString() {
        return this.searchArray.join(', ');
    }

    get stepsString() {
        return this.searchSteps.map(step => 
            `${step.step}: prev=${step.prev}, current=${step.current}`
        ).join('\n');
    }
}
```

```html
<!-- jumpSearch.html - Lightning Web Component HTML file -->

<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                <div class="slds-media__body">
                    <h2 class="slds-card__header-title slds-truncate" title="Jump Search Algorithm">
                        Jump Search Algorithm
                    </h2>
                </div>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Array to Search</label>
                        <div class="slds-form-element__control">
                            <p class="slds-text-body_small">{arrayString}</p>
                        </div>
                    </div>
                    
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Search Value</label>
                        <div class="slds-form-element__control">
                            <input 
                                type="number" 
                                class="slds-input" 
                                value={searchValue}
                                onchange={handleInputChange}
                                placeholder="Enter number to search"
                            />
                        </div>
                    </div>
                    
                    <lightning-button 
                        label="Search" 
                        variant="brand" 
                        onclick={handleSearch}
                        class="slds-m-top_medium">
                    </lightning-button>
                    
                    <div class="slds-form-element slds-m-top_medium">
                        <label class="slds-form-element__label">Result</label>
                        <div class="slds-form-element__control">
                            <p class="slds-text-body_small">{result}</p>
                        </div>
                    </div>
                </div>
                
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Search Steps</label>
                        <div class="slds-form-element__control">
                            <pre class="slds-text-body_small slds-scrollable">{stepsString}</pre>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>
```

```css
/* jumpSearch.css - Lightning Web Component CSS file */

.slds-scrollable {
    max-height: 300px;
    overflow-y: auto;
}
```

## How Jump Search Works

The jump search algorithm works by:

1. **Calculate Jump Size**: Jump size = √n (where n is array length)
2. **Jump Through Blocks**: Jump through the array in blocks of size √n
3. **Find the Block**: Identify the block where the target element might be
4. **Linear Search**: Perform linear search within that block

## Time Complexity
- **Best Case**: O(1) - Element found at first jump
- **Average Case**: O(√n) 
- **Worst Case**: O(√n)

## Space Complexity
- O(1) - Only uses a constant amount of extra space

## Key Features
- Works on sorted arrays only
- More efficient than linear search for large arrays
- Better than binary search for certain scenarios due to fewer comparisons
- Easy to implement and understand

The LWC component above demonstrates the jump search algorithm with visual step-by-step execution showing how the algorithm jumps through the array and performs linear search in the identified block.

