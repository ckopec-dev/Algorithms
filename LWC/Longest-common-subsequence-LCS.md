# Longest Common Subsequence (LCS) in Lightning Web Component

Here's a complete example of implementing the LCS algorithm in a Lightning Web Component:

## HTML Template (lcsCalculator.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_small">Longest Common Subsequence Calculator</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">String 1</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={string1}
                    onchange={handleString1Change}
                    placeholder="Enter first string"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">String 2</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={string2}
                    onchange={handleString2Change}
                    placeholder="Enter second string"
                />
            </div>
        </div>

        <lightning-button 
            label="Calculate LCS" 
            variant="brand" 
            onclick={calculateLCS}
            class="slds-m-top_small"
        ></lightning-button>

        <template if:true={showResult}>
            <div class="slds-box slds-theme_info slds-m-top_small">
                <h3 class="slds-text-heading_small">Result</h3>
                <p><strong>LCS Length:</strong> {lcsLength}</p>
                <p><strong>LCS String:</strong> {lcsString}</p>
                <p><strong>Matrix:</strong></p>
                <div class="slds-scrollable_x" style="max-height: 200px;">
                    <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                        <thead>
                            <tr class="slds-line-height_reset">
                                <th scope="col"></th>
                                <template for:each={matrixHeaders} for:item="header">
                                    <th scope="col" key={header}>{header}</th>
                                </template>
                            </tr>
                        </thead>
                        <tbody>
                            <template for:each={lcsMatrix} for:item="row" for:index="index">
                                <tr key={index}>
                                    <th scope="row">{matrixHeaders[index]}</th>
                                    <template for:each={row} for:item="cell">
                                        <td key={cell}>{cell}</td>
                                    </template>
                                </tr>
                            </template>
                        </tbody>
                    </table>
                </div>
            </div>
        </template>
    </div>
</template>
```

## JavaScript Controller (lcsCalculator.js)
```javascript
import { LightningElement } from 'lwc';

export default class LcsCalculator extends LightningElement {
    string1 = '';
    string2 = '';
    lcsLength = 0;
    lcsString = '';
    lcsMatrix = [];
    matrixHeaders = [];
    showResult = false;

    handleString1Change(event) {
        this.string1 = event.target.value;
    }

    handleString2Change(event) {
        this.string2 = event.target.value;
    }

    calculateLCS() {
        if (!this.string1 || !this.string2) {
            return;
        }

        // Calculate LCS using dynamic programming
        const result = this.calculateLCSMatrix(this.string1, this.string2);
        this.lcsLength = result.length;
        this.lcsString = result.lcsString;
        this.lcsMatrix = result.matrix;
        this.matrixHeaders = result.headers;
        this.showResult = true;
    }

    calculateLCSMatrix(str1, str2) {
        const m = str1.length;
        const n = str2.length;
        
        // Create matrix
        const matrix = Array(m + 1).fill().map(() => Array(n + 1).fill(0));
        const headers = [' ', ...str2.split('')];
        
        // Fill the matrix
        for (let i = 1; i <= m; i++) {
            for (let j = 1; j <= n; j++) {
                if (str1[i - 1] === str2[j - 1]) {
                    matrix[i][j] = matrix[i - 1][j - 1] + 1;
                } else {
                    matrix[i][j] = Math.max(matrix[i - 1][j], matrix[i][j - 1]);
                }
            }
        }

        // Backtrack to find the actual LCS string
        let lcsString = '';
        let i = m;
        let j = n;
        
        while (i > 0 && j > 0) {
            if (str1[i - 1] === str2[j - 1]) {
                lcsString = str1[i - 1] + lcsString;
                i--;
                j--;
            } else if (matrix[i - 1][j] > matrix[i][j - 1]) {
                i--;
            } else {
                j--;
            }
        }

        return {
            length: matrix[m][n],
            lcsString: lcsString,
            matrix: matrix,
            headers: headers
        };
    }
}
```

## CSS Styles (lcsCalculator.css)
```css
.slds-table_cell-buffer td,
.slds-table_cell-buffer th {
    padding: 0.5rem;
}

.slds-scrollable_x {
    overflow-x: auto;
}
```

## Usage Example

When you use this component with the following inputs:
- **String 1**: "ABCDGH"
- **String 2**: "AEDFHR"

The output will be:
- **LCS Length**: 3
- **LCS String**: "ADH"
- **Matrix**: A 7x7 matrix showing the dynamic programming calculation

## Key Features

1. **Dynamic Programming Implementation**: Uses the classic LCS algorithm with O(m×n) time complexity
2. **Interactive UI**: Real-time input fields with calculation button
3. **Visual Matrix Display**: Shows the complete calculation matrix
4. **Backtracking**: Extracts the actual LCS string from the matrix
5. **Responsive Design**: Uses Salesforce Lightning Design System components

## Algorithm Explanation

The LCS algorithm works by:
1. Creating a 2D matrix where each cell [i][j] represents the length of LCS for substrings str1[0...i-1] and str2[0...j-1]
2. Filling the matrix using the recurrence relation:
   - If characters match: `matrix[i][j] = matrix[i-1][j-1] + 1`
   - If they don't match: `matrix[i][j] = max(matrix[i-1][j], matrix[i][j-1])`
3. Backtracking through the matrix to reconstruct the actual LCS string

This implementation demonstrates a practical application of the LCS algorithm in a modern Salesforce Lightning Web Component.

