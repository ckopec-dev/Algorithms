# Knapsack Problem Algorithm in Lightning Web Component

Here's a complete example of implementing the Knapsack problem algorithm in a Lightning Web Component:

## JavaScript Controller (knapsack.js)

```javascript
import { LightningElement } from 'lwc';

export default class Knapsack extends LightningElement {
    // Input data
    capacity = 50;
    weights = [10, 20, 30];
    values = [60, 100, 120];
    items = [];
    
    // Results
    maxProfit = 0;
    selectedItems = [];
    
    // Initialize items
    connectedCallback() {
        this.items = this.weights.map((weight, index) => ({
            id: index,
            weight: weight,
            value: this.values[index],
            valuePerWeight: this.values[index] / weight
        }));
    }
    
    // Knapsack algorithm implementation
    solveKnapsack() {
        const n = this.items.length;
        const dp = Array(n + 1).fill().map(() => Array(this.capacity + 1).fill(0));
        
        // Fill the dp table
        for (let i = 1; i <= n; i++) {
            for (let w = 0; w <= this.capacity; w++) {
                // If current item's weight is more than the current capacity
                if (this.items[i - 1].weight > w) {
                    dp[i][w] = dp[i - 1][w];
                } else {
                    // Max of including or excluding the current item
                    dp[i][w] = Math.max(
                        dp[i - 1][w],
                        dp[i - 1][w - this.items[i - 1].weight] + this.items[i - 1].value
                    );
                }
            }
        }
        
        this.maxProfit = dp[n][this.capacity];
        this.selectedItems = this.getSelectedItems(dp);
    }
    
    // Backtrack to find selected items
    getSelectedItems(dp) {
        const selected = [];
        let w = this.capacity;
        const n = this.items.length;
        
        for (let i = n; i > 0 && w > 0; i--) {
            // If value is different from previous row, item was included
            if (dp[i][w] !== dp[i - 1][w]) {
                selected.push({
                    id: i - 1,
                    ...this.items[i - 1]
                });
                w -= this.items[i - 1].weight;
            }
        }
        
        return selected.reverse();
    }
    
    // Handle form submission
    handleSolve() {
        this.solveKnapsack();
    }
    
    // Handle input changes
    handleCapacityChange(event) {
        this.capacity = parseInt(event.target.value) || 0;
    }
    
    handleWeightChange(event) {
        const index = event.target.dataset.index;
        this.weights[index] = parseInt(event.target.value) || 0;
        this.items[index].weight = this.weights[index];
    }
    
    handleValueChange(event) {
        const index = event.target.dataset.index;
        this.values[index] = parseInt(event.target.value) || 0;
        this.items[index].value = this.values[index];
    }
}
```

## HTML Template (knapsack.html)

```html
<template>
    <div class="slds-box slds-box_small slds-theme_default">
        <h2 class="slds-text-heading_small">Knapsack Problem Solver</h2>
        
        <!-- Input Section -->
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                <lightning-input 
                    label="Knapsack Capacity" 
                    type="number" 
                    value={capacity}
                    onchange={handleCapacityChange}
                    min="0">
                </lightning-input>
            </div>
        </div>
        
        <!-- Items Input -->
        <div class="slds-section slds-section_compact">
            <header class="slds-section__title">
                <h3 class="slds-text-heading_small">Items</h3>
            </header>
            <div class="slds-section__content">
                <template for:each={items} for:item="item" for:index="index">
                    <div key={item.id} class="slds-grid slds-gutters slds-wrap slds-m-bottom_small">
                        <div class="slds-col slds-size_12-of-12 slds-medium-size_4-of-12">
                            <lightning-input 
                                label="Weight" 
                                type="number" 
                                value={item.weight}
                                data-index={index}
                                onchange={handleWeightChange}
                                min="0">
                            </lightning-input>
                        </div>
                        <div class="slds-col slds-size_12-of-12 slds-medium-size_4-of-12">
                            <lightning-input 
                                label="Value" 
                                type="number" 
                                value={item.value}
                                data-index={index}
                                onchange={handleValueChange}
                                min="0">
                            </lightning-input>
                        </div>
                        <div class="slds-col slds-size_12-of-12 slds-medium-size_4-of-12">
                            <lightning-input 
                                label="Value/Weight Ratio" 
                                type="number" 
                                value={item.valuePerWeight}
                                disabled>
                            </lightning-input>
                        </div>
                    </div>
                </template>
            </div>
        </div>
        
        <!-- Action Button -->
        <lightning-button 
            label="Solve Knapsack Problem" 
            variant="brand" 
            onclick={handleSolve}
            class="slds-m-top_small">
        </lightning-button>
        
        <!-- Results Section -->
        <template if:true={maxProfit}>
            <div class="slds-section slds-section_compact slds-m-top_small">
                <header class="slds-section__title">
                    <h3 class="slds-text-heading_small">Results</h3>
                </header>
                <div class="slds-section__content">
                    <div class="slds-grid slds-gutters">
                        <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                            <lightning-output-field 
                                label="Maximum Profit" 
                                value={maxProfit}>
                            </lightning-output-field>
                        </div>
                        <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                            <lightning-output-field 
                                label="Total Weight" 
                                value={selectedItems.reduce((sum, item) => sum + item.weight, 0)}>
                            </lightning-output-field>
                        </div>
                    </div>
                    
                    <div class="slds-m-top_small">
                        <h4 class="slds-text-heading_small">Selected Items:</h4>
                        <template for:each={selectedItems} for:item="item">
                            <div key={item.id} class="slds-box slds-box_x-small slds-m-bottom_x-small">
                                <div class="slds-grid slds-gutters slds-wrap">
                                    <div class="slds-col slds-size_12-of-12 slds-medium-size_4-of-12">
                                        <strong>Item {item.id + 1}</strong>
                                    </div>
                                    <div class="slds-col slds-size_12-of-12 slds-medium-size_4-of-12">
                                        Weight: {item.weight}
                                    </div>
                                    <div class="slds-col slds-size_12-of-12 slds-medium-size_4-of-12">
                                        Value: {item.value}
                                    </div>
                                </div>
                            </div>
                        </template>
                    </div>
                </div>
            </div>
        </template>
    </div>
</template>
```

## CSS Styles (knapsack.css)

```css
.slds-box_x-small {
    padding: 0.5rem;
}

.slds-section_compact {
    margin-top: 1rem;
}

.slds-m-bottom_x-small {
    margin-bottom: 0.25rem;
}
```

## Key Features of this Implementation:

1. **Dynamic Input**: Users can modify knapsack capacity and item properties
2. **Dynamic Programming**: Uses 2D DP table for optimal solution
3. **Backtracking**: Identifies which items were selected to achieve maximum profit
4. **Real-time Updates**: Shows value/weight ratios and results
5. **User-Friendly Interface**: Clean, responsive design using Lightning Design System
6. **Error Handling**: Basic input validation and error prevention

## How it Works:

1. The algorithm builds a 2D table `dp[i][w]` where:
   - `i` represents items considered (0 to n)
   - `w` represents capacity (0 to capacity)
   - `dp[i][w]` stores maximum value achievable with first `i` items and capacity `w`

2. **Recurrence Relation**:
   ```
   dp[i][w] = max(dp[i-1][w], dp[i-1][w-weight[i]] + value[i])
   ```

3. **Time Complexity**: O(n × W) where n is number of items and W is capacity
4. **Space Complexity**: O(n × W)

This implementation demonstrates a classic dynamic programming solution to the 0/1 Knapsack problem in a Lightning Web Component framework.

