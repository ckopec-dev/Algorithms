# Fibonacci Search Technique in Lightning Web Component

Here's a complete example of implementing Fibonacci search technique in a Lightning Web Component:

```javascript
// fibonacciSearch.js
import { LightningElement } from 'lwc';

export default class FibonacciSearch extends LightningElement {
    searchInput = '';
    searchResult = '';
    searchArray = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29];
    searchSteps = [];

    handleSearch() {
        const target = parseInt(this.searchInput);
        if (isNaN(target)) {
            this.searchResult = 'Please enter a valid number';
            return;
        }

        this.searchSteps = [];
        const result = this.fibonacciSearch(this.searchArray, target);
        
        if (result !== -1) {
            this.searchResult = `Element found at index: ${result}`;
        } else {
            this.searchResult = 'Element not found in the array';
        }
    }

    fibonacciSearch(arr, target) {
        const n = arr.length;
        
        // Initialize Fibonacci numbers
        let fibM2 = 0; // (m-2)th Fibonacci number
        let fibM1 = 1; // (m-1)th Fibonacci number
        let fibM = fibM2 + fibM1; // mth Fibonacci number
        
        // Find the smallest Fibonacci number greater than or equal to n
        while (fibM < n) {
            fibM2 = fibM1;
            fibM1 = fibM;
            fibM = fibM2 + fibM1;
        }
        
        let offset = -1;
        
        // While there are elements to be checked
        while (fibM > 1) {
            // Check if fibM2 is a valid location
            let i = Math.min(offset + fibM2, n - 1);
            
            // Add step to visualization
            this.searchSteps.push({
                step: `Comparing with element at index ${i} (value: ${arr[i]})`,
                current: i,
                fibM: fibM,
                fibM2: fibM2,
                fibM1: fibM1
            });
            
            // If target is greater, cut the subarray from offset to i
            if (arr[i] < target) {
                fibM = fibM1;
                fibM1 = fibM2;
                fibM2 = fibM - fibM1;
                offset = i;
            }
            // If target is smaller, cut the subarray after i
            else if (arr[i] > target) {
                fibM = fibM2;
                fibM1 = fibM1 - fibM2;
                fibM2 = fibM - fibM1;
            }
            // Element found
            else {
                return i;
            }
        }
        
        // Compare the last element with target
        if (fibM1 && arr[offset + 1] === target) {
            return offset + 1;
        }
        
        // Element not found
        return -1;
    }

    handleInputChange(event) {
        this.searchInput = event.target.value;
    }

    get arrayDisplay() {
        return this.searchArray.map((value, index) => ({
            value: value,
            index: index
        }));
    }

    get stepsDisplay() {
        return this.searchSteps;
    }

    clearSearch() {
        this.searchInput = '';
        this.searchResult = '';
        this.searchSteps = [];
    }
}
```

```html
<!-- fibonacciSearch.html -->
<template>
    <div class="slds-box slds-box_large slds-m-around_medium">
        <h2 class="slds-text-heading_medium slds-m-bottom_small">Fibonacci Search Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-1 slds-medium-size_1-of-2">
                <lightning-input 
                    label="Enter number to search"
                    value={searchInput}
                    onchange={handleInputChange}
                    type="number">
                </lightning-input>
                
                <lightning-button 
                    label="Search"
                    onclick={handleSearch}
                    variant="brand"
                    class="slds-m-top_small">
                </lightning-button>
                
                <lightning-button 
                    label="Clear"
                    onclick={clearSearch}
                    variant="destructive"
                    class="slds-m-top_small slds-m-left_small">
                </lightning-button>
                
                <div class="slds-m-top_small">
                    <p class="slds-text-title">Result: {searchResult}</p>
                </div>
            </div>
            
            <div class="slds-col slds-size_1-of-1 slds-medium-size_1-of-2">
                <h3 class="slds-text-heading_small">Search Array</h3>
                <div class="slds-grid slds-gutters slds-wrap">
                    <template for:each={arrayDisplay} for:item="item">
                        <div class="slds-col slds-size_1-of-5">
                            <div class="slds-text-body_small slds-text-align_center">
                                <div class="slds-badge slds-badge_lightest">
                                    <span class="slds-badge__label">{item.value}</span>
                                </div>
                                <div class="slds-text-body_small slds-text-align_center slds-m-top_xxx-small">
                                    [{item.index}]
                                </div>
                            </div>
                        </div>
                    </template>
                </div>
            </div>
        </div>
        
        <template if:true={searchSteps.length > 0}>
            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Search Steps</h3>
                <div class="slds-grid slds-gutters slds-wrap">
                    <template for:each={stepsDisplay} for:item="step">
                        <div class="slds-col slds-size_1-of-1 slds-medium-size_1-of-2">
                            <div class="slds-card">
                                <div class="slds-card__header slds-grid">
                                    <header class="slds-media slds-media_center slds-has-flexi-truncate">
                                        <div class="slds-media__body">
                                            <h3 class="slds-card__header-title slds-truncate" title="Step {step.index}">Step {step.index}</h3>
                                        </div>
                                    </header>
                                </div>
                                <div class="slds-card__body">
                                    <p>{step.step}</p>
                                    <div class="slds-grid slds-gutters">
                                        <div class="slds-col slds-size_1-of-3">
                                            <p class="slds-text-body_small">Fib(M): {step.fibM}</p>
                                        </div>
                                        <div class="slds-col slds-size_1-of-3">
                                            <p class="slds-text-body_small">Fib(M-1): {step.fibM1}</p>
                                        </div>
                                        <div class="slds-col slds-size_1-of-3">
                                            <p class="slds-text-body_small">Fib(M-2): {step.fibM2}</p>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </template>
                </div>
            </div>
        </template>
    </div>
</template>
```

```css
/* fibonacciSearch.css */
.slds-badge {
    margin-bottom: 0.5rem;
}

.slds-card {
    margin-bottom: 1rem;
}

.slds-card__header-title {
    font-weight: 600;
}
```

## Key Features of this Implementation:

1. **Fibonacci Search Algorithm**: Implements the classic Fibonacci search technique
2. **Interactive UI**: Allows users to input values and see the search process
3. **Step-by-step Visualization**: Shows each step of the Fibonacci search process
4. **Real-time Feedback**: Displays current comparison and Fibonacci numbers used
5. **Responsive Design**: Works well on different screen sizes
6. **Error Handling**: Validates input and handles edge cases

## How Fibonacci Search Works:

1. **Preprocessing**: Find the smallest Fibonacci number greater than or equal to array length
2. **Comparison**: Compare target with element at position determined by Fibonacci numbers
3. **Elimination**: Based on comparison, eliminate portions of the array
4. **Iteration**: Continue until element is found or search space is exhausted

## Time Complexity:
- **Best Case**: O(1) - Element found at first comparison
- **Average Case**: O(log n)
- **Worst Case**: O(log n)

This implementation provides a visual and educational demonstration of how Fibonacci search works compared to binary search.

