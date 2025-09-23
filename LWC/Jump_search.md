# Jump Search Algorithm in Lightning Web Component

```javascript
// jumpSearch.js
import { LightningElement } from 'lwc';

export default class JumpSearch extends LightningElement {
    inputArray = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
    targetValue = 13;
    result = '';
    searchSteps = [];

    handleSearch() {
        const array = this.inputArray;
        const target = parseInt(this.targetValue);
        
        if (isNaN(target)) {
            this.result = 'Please enter a valid number';
            return;
        }

        this.searchSteps = [];
        const index = this.jumpSearch(array, target);
        
        if (index !== -1) {
            this.result = `Element found at index: ${index}`;
        } else {
            this.result = 'Element not found in array';
        }
    }

    jumpSearch(array, target) {
        const n = array.length;
        let step = Math.floor(Math.sqrt(n));
        let prev = 0;

        // Add step to search steps
        this.searchSteps.push({
            message: `Starting jump search with step size: ${step}`,
            array: [...array],
            currentStep: 'start'
        });

        // Jump through the array
        while (array[Math.min(step, n) - 1] < target) {
            prev = step;
            step += Math.floor(Math.sqrt(n));
            
            this.searchSteps.push({
                message: `Jumping from index ${prev} to index ${step}`,
                array: [...array],
                currentStep: 'jump',
                prevIndex: prev,
                currentIndex: step
            });

            if (prev >= n) {
                return -1;
            }
        }

        // Linear search in the block
        this.searchSteps.push({
            message: `Performing linear search from index ${prev} to ${Math.min(step, n) - 1}`,
            array: [...array],
            currentStep: 'linear_search',
            startIndex: prev,
            endIndex: Math.min(step, n) - 1
        });

        for (let i = prev; i < Math.min(step, n); i++) {
            this.searchSteps.push({
                message: `Checking element at index ${i}`,
                array: [...array],
                currentStep: 'check',
                currentIndex: i
            });

            if (array[i] === target) {
                return i;
            }
        }

        return -1;
    }

    handleInputChange(event) {
        const { name, value } = event.target;
        if (name === 'target') {
            this.targetValue = value;
        }
    }

    get formattedArray() {
        return this.inputArray.join(', ');
    }

    get searchStepsHtml() {
        return this.searchSteps.map((step, index) => `
            <div class="slds-p-around_small slds-border_bottom">
                <p><strong>Step ${index + 1}:</strong> ${step.message}</p>
                ${step.currentStep === 'jump' ? 
                    `<p>Jumping from index ${step.prevIndex} to index ${step.currentIndex}</p>` : ''}
                ${step.currentStep === 'linear_search' ? 
                    `<p>Searching in range [${step.startIndex}, ${step.endIndex}]</p>` : ''}
                ${step.currentStep === 'check' ? 
                    `<p>Checking element: ${step.array[step.currentIndex]}</p>` : ''}
            </div>
        `).join('');
    }
}
```

```html
<!-- jumpSearch.html -->
<template>
    <div class="slds-box slds-p-around_medium">
        <h2>Jump Search Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Input Array</label>
            <p class="slds-form-element__help">{formattedArray}</p>
        </div>

        <div class="slds-form-element slds-m-top_medium">
            <label class="slds-form-element__label">Target Value</label>
            <input 
                type="number" 
                name="target"
                value={targetValue}
                onchange={handleInputChange}
                class="slds-input"
            />
        </div>

        <button 
            onclick={handleSearch}
            class="slds-button slds-button_brand slds-m-top_medium"
        >
            Search
        </button>

        <div class="slds-m-top_medium">
            <p><strong>Result:</strong> {result}</p>
        </div>

        <div class="slds-m-top_medium">
            <h3>Search Steps</h3>
            <template if:true={searchSteps.length}>
                <div class="slds-card">
                    <div class="slds-card__body">
                        <template for:each={searchSteps} for:item="step" for:index="index">
                            <div key={index} class="slds-p-around_small slds-border_bottom">
                                <p><strong>Step {index + 1}:</strong> {step.message}</p>
                                <template if:true={step.prevIndex}>
                                    <p>Jumping from index {step.prevIndex} to index {step.currentIndex}</p>
                                </template>
                                <template if:true={step.startIndex}>
                                    <p>Searching in range [{step.startIndex}, {step.endIndex}]</p>
                                </template>
                                <template if:true={step.currentIndex}>
                                    <p>Checking element: {step.array[step.currentIndex]}</p>
                                </template>
                            </div>
                        </template>
                    </div>
                </div>
            </template>
        </div>
    </div>
</template>
```

```css
/* jumpSearch.css */
.slds-card__body {
    max-height: 400px;
    overflow-y: auto;
}
```

## How Jump Search Works

1. **Calculate Step Size**: Take square root of array length as step size
2. **Jump Through Array**: Jump through the array in steps
3. **Find Block**: Identify the block where target might be
4. **Linear Search**: Perform linear search within that block

## Time Complexity
- **Best Case**: O(1)
- **Average Case**: O(√n)
- **Worst Case**: O(√n)

## Space Complexity
- O(1) - Only uses a constant amount of extra space

This implementation shows the step-by-step process of jump search with visual feedback in the Lightning Web Component.

