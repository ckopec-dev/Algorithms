# Dynamic Programming Example in Lightning Web Component

Here's a complete example of a Dynamic Programming algorithm implemented in a Lightning Web Component - the classic Fibonacci sequence calculation with memoization:

## LWC Component Structure

### fibonacci.component.html
```html
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="Dynamic Programming - Fibonacci">
                <h2 class="slds-text-heading_small">Dynamic Programming - Fibonacci Calculator</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <lightning-input 
                        label="Enter Fibonacci number (n)" 
                        type="number" 
                        value={inputNumber}
                        onchange={handleInputChange}
                        min="0"
                        max="50">
                    </lightning-input>
                    <lightning-button 
                        label="Calculate" 
                        onclick={calculateFibonacci}
                        variant="brand"
                        disabled={isCalculating}>
                    </lightning-button>
                </div>
                <div class="slds-col slds-size_1-of-2">
                    <lightning-output-field 
                        label="Result" 
                        value={result}>
                    </lightning-output-field>
                    <lightning-output-field 
                        label="Calculation Time (ms)" 
                        value={calculationTime}>
                    </lightning-output-field>
                </div>
            </div>
            
            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Memoization Table</h3>
                <lightning-datatable
                    data={memoizationData}
                    columns={columns}
                    key-field="id"
                    hide-checkbox-column="true">
                </lightning-datatable>
            </div>
        </div>
    </div>
</template>
```

### fibonacci.component.js
```javascript
import { LightningElement, track } from 'lwc';

export default class Fibonacci extends LightningElement {
    @track inputNumber = 10;
    @track result = '';
    @track calculationTime = '';
    @track isCalculating = false;
    @track memoizationData = [];
    
    columns = [
        { label: 'n', fieldName: 'n', type: 'number' },
        { label: 'Fib(n)', fieldName: 'fibValue', type: 'number' },
        { label: 'Computed', fieldName: 'computed', type: 'boolean' }
    ];

    // Memoization cache for dynamic programming
    fibCache = new Map();
    
    handleInputChange(event) {
        this.inputNumber = event.target.value;
    }

    calculateFibonacci() {
        this.isCalculating = true;
        this.result = '';
        this.calculationTime = '';
        this.memoizationData = [];

        // Clear cache for fresh calculation
        this.fibCache.clear();

        const startTime = performance.now();
        
        const fibResult = this.fibonacciDP(this.inputNumber);
        
        const endTime = performance.now();
        const timeTaken = (endTime - startTime).toFixed(4);

        this.result = fibResult.toString();
        this.calculationTime = timeTaken;

        // Prepare data for display
        this.prepareMemoizationData();
        
        this.isCalculating = false;
    }

    /**
     * Dynamic Programming approach to calculate Fibonacci
     * Uses memoization to avoid redundant calculations
     */
    fibonacciDP(n) {
        // Base cases
        if (n <= 1) {
            return n;
        }

        // Check if already computed
        if (this.fibCache.has(n)) {
            return this.fibCache.get(n);
        }

        // Compute and store result
        const result = this.fibonacciDP(n - 1) + this.fibonacciDP(n - 2);
        this.fibCache.set(n, result);
        
        return result;
    }

    /**
     * Prepare data for visualization table
     */
    prepareMemoizationData() {
        const data = [];
        
        // Show all computed values from 0 to input number
        for (let i = 0; i <= this.inputNumber; i++) {
            if (this.fibCache.has(i)) {
                data.push({
                    id: i,
                    n: i,
                    fibValue: this.fibCache.get(i),
                    computed: true
                });
            } else {
                data.push({
                    id: i,
                    n: i,
                    fibValue: 0,
                    computed: false
                });
            }
        }
        
        this.memoizationData = data;
    }

    /**
     * Alternative recursive approach without DP (for comparison)
     */
    fibonacciNaive(n) {
        if (n <= 1) {
            return n;
        }
        return this.fibonacciNaive(n - 1) + this.fibonacciNaive(n - 2);
    }
}
```

### fibonacci.component.css
```css
.slds-card__body {
    background-color: #f4f6f9;
}

.slds-card {
    border: 1px solid #e1e8f0;
    border-radius: 0.25rem;
}

.slds-text-heading_small {
    font-size: 1rem;
    font-weight: 600;
    line-height: 1.25;
}
```

## Key Dynamic Programming Concepts Demonstrated

1. **Memoization**: Using a Map to cache previously computed Fibonacci values
2. **Overlapping Subproblems**: The same Fibonacci numbers are computed multiple times in naive recursion
3. **Optimal Substructure**: Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
4. **Bottom-up Approach**: Building up solutions from base cases

## How It Works

1. **Input**: User enters a number n
2. **DP Calculation**: Uses memoization to avoid recomputing values
3. **Visualization**: Shows which values were computed and their results
4. **Performance**: Demonstrates significant speed improvement over naive recursion

## Benefits of This Approach

- **Time Complexity**: Reduced from O(2^n) to O(n)
- **Space Complexity**: O(n) for the cache
- **Reusability**: Cached values can be reused for future calculations
- **User Experience**: Fast responses even for large numbers

This example clearly demonstrates how dynamic programming can transform exponential time complexity into linear time complexity through the use of memoization and avoiding redundant calculations.

