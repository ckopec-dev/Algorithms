# Damerau-Levenshtein Distance in Lightning Web Component

Here's a complete example of implementing the Damerau-Levenshtein distance algorithm in a Lightning Web Component:

```javascript
// damerauLevenshtein.js
import { LightningElement } from 'lwc';

export default class DamerauLevenshtein extends LightningElement {
    input1 = '';
    input2 = '';
    distance = 0;
    isCalculated = false;

    handleInput1Change(event) {
        this.input1 = event.target.value;
        this.calculateDistance();
    }

    handleInput2Change(event) {
        this.input2 = event.target.value;
        this.calculateDistance();
    }

    calculateDistance() {
        if (this.input1 && this.input2) {
            this.distance = this.damerauLevenshteinDistance(this.input1, this.input2);
            this.isCalculated = true;
        } else {
            this.isCalculated = false;
        }
    }

    damerauLevenshteinDistance(str1, str2) {
        const len1 = str1.length;
        const len2 = str2.length;
        
        // Create a matrix to store distances
        const matrix = Array(len1 + 1)
            .fill()
            .map(() => Array(len2 + 1).fill(0));

        // Initialize first row and column
        for (let i = 0; i <= len1; i++) {
            matrix[i][0] = i;
        }
        for (let j = 0; j <= len2; j++) {
            matrix[0][j] = j;
        }

        // Fill the matrix
        for (let i = 1; i <= len1; i++) {
            for (let j = 1; j <= len2; j++) {
                const cost = str1[i - 1] === str2[j - 1] ? 0 : 1;
                
                // Calculate minimum operations
                matrix[i][j] = Math.min(
                    matrix[i - 1][j] + 1,        // deletion
                    matrix[i][j - 1] + 1,        // insertion
                    matrix[i - 1][j - 1] + cost  // substitution
                );

                // Check for transposition (Damerau-Levenshtein specific)
                if (i > 1 && j > 1 && 
                    str1[i - 1] === str2[j - 2] && 
                    str1[i - 2] === str2[j - 1]) {
                    matrix[i][j] = Math.min(
                        matrix[i][j],
                        matrix[i - 2][j - 2] + 1  // transposition
                    );
                }
            }
        }

        return matrix[len1][len2];
    }

    get distanceClass() {
        return this.isCalculated ? 'slds-text-title slds-text-color_success' : 'slds-text-title';
    }

    get distanceText() {
        return this.isCalculated ? `Distance: ${this.distance}` : '';
    }
}
```

```html
<!-- damerauLevenshtein.html -->
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <h2 class="slds-text-heading_small">Damerau-Levenshtein Distance Calculator</h2>
        
        <div class="slds-form-element slds-m-bottom_medium">
            <label class="slds-form-element__label">First String</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={input1}
                    onchange={handleInput1Change}
                    placeholder="Enter first string"
                />
            </div>
        </div>

        <div class="slds-form-element slds-m-bottom_medium">
            <label class="slds-form-element__label">Second String</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={input2}
                    onchange={handleInput2Change}
                    placeholder="Enter second string"
                />
            </div>
        </div>

        <template if:true={isCalculated}>
            <div class={distanceClass}>
                {distanceText}
            </div>
            <div class="slds-text-body_small slds-m-top_small">
                <p>Operations: Insertion, Deletion, Substitution, Transposition</p>
            </div>
        </template>

        <template if:false={isCalculated}>
            <p class="slds-text-body_small slds-text-color_weak">Enter two strings to calculate distance</p>
        </template>
    </div>
</template>
```

```css
/* damerauLevenshtein.css */
.slds-box {
    background-color: #ffffff;
    border: 1px solid #e5e5e5;
    border-radius: 0.25rem;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
}

.slds-text-title {
    font-weight: 600;
    font-size: 1rem;
    line-height: 1.25;
}
```

## How it works:

1. **Algorithm Implementation**: The `damerauLevenshteinDistance` function implements the classic dynamic programming approach with the additional transposition operation.

2. **Operations Supported**:
   - **Insertion**: Add a character
   - **Deletion**: Remove a character  
   - **Substitution**: Replace a character
   - **Transposition**: Swap two adjacent characters (Damerau-Levenshtein specific)

3. **Key Features**:
   - Real-time calculation as users type
   - Responsive UI with Salesforce Lightning design
   - Clear visual feedback
   - Handles edge cases (empty strings)

## Example Usage:

- Input: "kitten" and "sitting" → Distance: 3
- Input: "hello" and "world" → Distance: 5  
- Input: "abc" and "acb" → Distance: 1 (transposition)

The component provides an interactive way to visualize how the Damerau-Levenshtein distance algorithm works by comparing two strings and calculating the minimum number of operations needed to transform one string into another.

