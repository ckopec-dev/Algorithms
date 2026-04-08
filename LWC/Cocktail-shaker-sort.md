# Cocktail Shaker Sort in Lightning Web Component

```javascript
// cocktailShakerSort.js
import { LightningElement } from 'lwc';

export default class CocktailShakerSort extends LightningElement {
    array = [64, 34, 25, 12, 22, 11, 90];
    sortedArray = [...this.array];
    isSorting = false;
    steps = [];
    currentStep = 0;

    handleSort() {
        this.isSorting = true;
        this.steps = [];
        this.sortedArray = [...this.array];
        
        // Perform cocktail shaker sort and store steps
        this.cocktailShakerSortWithSteps();
        this.currentStep = 0;
        
        // Start animation
        this.animateSteps();
    }

    cocktailShakerSortWithSteps() {
        let arr = [...this.sortedArray];
        let n = arr.length;
        let start = 0;
        let end = n - 1;
        let swapped = true;

        // Add initial state
        this.steps.push({
            array: [...arr],
            start: -1,
            end: -1,
            direction: 'both'
        });

        while (swapped) {
            swapped = false;

            // Forward pass (left to right)
            for (let i = start; i < end; i++) {
                if (arr[i] > arr[i + 1]) {
                    [arr[i], arr[i + 1]] = [arr[i + 1], arr[i]];
                    swapped = true;
                    
                    this.steps.push({
                        array: [...arr],
                        start: i,
                        end: i + 1,
                        direction: 'forward'
                    });
                }
            }

            if (!swapped) break;

            end--;
            
            // Backward pass (right to left)
            for (let i = end; i > start; i--) {
                if (arr[i] < arr[i - 1]) {
                    [arr[i], arr[i - 1]] = [arr[i - 1], arr[i]];
                    swapped = true;
                    
                    this.steps.push({
                        array: [...arr],
                        start: i,
                        end: i - 1,
                        direction: 'backward'
                    });
                }
            }

            start++;
        }

        this.steps.push({
            array: [...arr],
            start: -1,
            end: -1,
            direction: 'done'
        });
    }

    animateSteps() {
        if (this.currentStep < this.steps.length) {
            const step = this.steps[this.currentStep];
            this.sortedArray = step.array;
            this.currentStep++;
            
            // Continue animation after delay
            setTimeout(() => {
                this.animateSteps();
            }, 500);
        } else {
            this.isSorting = false;
        }
    }

    handleReset() {
        this.sortedArray = [...this.array];
        this.steps = [];
        this.currentStep = 0;
        this.isSorting = false;
    }

    get stepInfo() {
        if (this.steps.length > 0 && this.currentStep > 0) {
            const step = this.steps[this.currentStep - 1];
            return `Step ${this.currentStep}: ${step.direction} pass`;
        }
        return 'Ready to sort';
    }
}
```

```html
<!-- cocktailShakerSort.html -->
<template>
    <div class="container">
        <h2>Cocktail Shaker Sort</h2>
        
        <div class="array-display">
            <h3>Original Array:</h3>
            <div class="array-items">
                <template for:each={array} for:item="item">
                    <span key={item} class="array-item">{item}</span>
                </template>
            </div>
        </div>

        <div class="array-display">
            <h3>Current Array:</h3>
            <div class="array-items">
                <template for:each={sortedArray} for:item="item">
                    <span key={item} class="array-item">{item}</span>
                </template>
            </div>
        </div>

        <div class="controls">
            <lightning-button 
                label="Sort Array" 
                variant="brand" 
                onclick={handleSort}
                disabled={isSorting}>
            </lightning-button>
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <div class="status">
            <p>{stepInfo}</p>
        </div>

        <div class="algorithm-info">
            <h3>How Cocktail Shaker Sort Works:</h3>
            <ul>
                <li>It's a bidirectional bubble sort</li>
                <li>Alternates between forward and backward passes</li>
                <li>Each pass "bubbles" the largest/smallest elements to their correct positions</li>
                <li>More efficient than regular bubble sort</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* cocktailShakerSort.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.array-display {
    margin: 20px 0;
    padding: 15px;
    border: 1px solid #e1e1e1;
    border-radius: 8px;
    background-color: #f9f9f9;
}

.array-items {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    margin-top: 10px;
}

.array-item {
    padding: 10px 15px;
    background-color: #0070d2;
    color: white;
    border-radius: 5px;
    font-weight: bold;
    min-width: 40px;
    text-align: center;
}

.array-item.comparing {
    background-color: #ff9900;
    transform: scale(1.1);
}

.array-item.swapped {
    background-color: #4caf50;
}

.controls {
    margin: 20px 0;
    text-align: center;
}

.status {
    text-align: center;
    padding: 10px;
    margin: 15px 0;
    background-color: #e8f5e8;
    border-radius: 5px;
}

.algorithm-info {
    margin-top: 30px;
    padding: 15px;
    background-color: #f0f8ff;
    border-radius: 8px;
}

.algorithm-info ul {
    margin: 10px 0;
    padding-left: 20px;
}

.algorithm-info li {
    margin: 5px 0;
}
```

## How It Works

This Lightning Web Component implements the Cocktail Shaker Sort algorithm with the following features:

1. **Bidirectional Sorting**: The algorithm alternates between forward and backward passes
2. **Visual Feedback**: Shows the current step and which elements are being compared/swapped
3. **Animation**: Steps through the sorting process with delays for visualization
4. **Interactive Controls**: Start sorting and reset functionality

## Algorithm Steps

1. **Forward Pass**: Left to right, bubble largest element to the end
2. **Backward Pass**: Right to left, bubble smallest element to the beginning
3. **Repeat**: Continue until no swaps are needed
4. **Optimization**: Reduce the range of comparison after each pass

The algorithm is more efficient than regular bubble sort because it eliminates elements from both ends of the array in each pass.

