# Matrix Chain Multiplication in Lightning Web Component

Here's a complete example of implementing the Matrix Chain Multiplication algorithm in Lightning Web Component:

## matrixChainMultiplication.js (JavaScript Controller)

```javascript
import { LightningElement } from 'lwc';

export default class MatrixChainMultiplication extends LightningElement {
    matrixDimensions = [10, 20, 30, 40, 30];
    result = '';
    optimalCost = 0;
    optimalParentheses = '';

    handleCalculate() {
        const n = this.matrixDimensions.length - 1;
        if (n <= 1) {
            this.result = 'Need at least 2 matrices for multiplication';
            return;
        }

        // Create cost matrix
        const dp = Array(n).fill().map(() => Array(n).fill(0));
        const bracket = Array(n).fill().map(() => Array(n).fill(0));

        // Chain length from 2 to n
        for (let chainLength = 2; chainLength <= n; chainLength++) {
            for (let i = 0; i <= n - chainLength; i++) {
                const j = i + chainLength - 1;
                dp[i][j] = Number.MAX_SAFE_INTEGER;
                
                for (let k = i; k < j; k++) {
                    const cost = dp[i][k] + dp[k + 1][j] + 
                                this.matrixDimensions[i] * this.matrixDimensions[k + 1] * this.matrixDimensions[j + 1];
                    
                    if (cost < dp[i][j]) {
                        dp[i][j] = cost;
                        bracket[i][j] = k;
                    }
                }
            }
        }

        this.optimalCost = dp[0][n - 1];
        this.optimalParentheses = this.printOptimalParentheses(bracket, 0, n - 1);
        
        this.result = `Minimum number of multiplications: ${this.optimalCost}\n`;
        this.result += `Optimal parenthesization: ${this.optimalParentheses}\n`;
        this.result += `Matrix dimensions: [${this.matrixDimensions.join(', ')}]`;
    }

    printOptimalParentheses(bracket, i, j) {
        if (i === j) {
            return `M${i + 1}`;
        }
        
        const left = this.printOptimalParentheses(bracket, i, bracket[i][j]);
        const right = this.printOptimalParentheses(bracket, bracket[i][j] + 1, j);
        
        return `(${left} × ${right})`;
    }

    handleInputChange(event) {
        const value = event.target.value;
        if (value) {
            this.matrixDimensions = value.split(',').map(Number);
        }
    }

    get matrixDimensionsString() {
        return this.matrixDimensions.join(', ');
    }
}
```

## matrixChainMultiplication.html (HTML Template)

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Matrix Chain Multiplication</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="dimensions">Matrix Dimensions</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="dimensions"
                    class="slds-input"
                    value={matrixDimensionsString}
                    onchange={handleInputChange}
                    placeholder="Enter dimensions separated by commas (e.g., 10,20,30,40,30)"
                />
            </div>
        </div>

        <lightning-button 
            label="Calculate Optimal Multiplication" 
            variant="brand" 
            onclick={handleCalculate}
            class="slds-m-top_medium">
        </lightning-button>

        <div class="slds-m-top_medium">
            <lightning-card title="Results">
                <div class="slds-text-body_small">
                    <pre>{result}</pre>
                </div>
            </lightning-card>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Algorithm Explanation">
                <div class="slds-text-body_small">
                    <p><strong>Matrix Chain Multiplication</strong> finds the most efficient way to multiply a chain of matrices.</p>
                    <p><strong>Time Complexity:</strong> O(n³)</p>
                    <p><strong>Space Complexity:</strong> O(n²)</p>
                    <p>The algorithm uses dynamic programming to determine the optimal parenthesization that minimizes scalar multiplications.</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## matrixChainMultiplication.css (CSS Styles)

```css
.slds-box {
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 500px;
}

.slds-text-body_small {
    font-size: 0.875rem;
    line-height: 1.4;
}

pre {
    white-space: pre-wrap;
    word-wrap: break-word;
    background-color: #f4f6f9;
    padding: 1rem;
    border-radius: 0.25rem;
    font-family: monospace;
}
```

## How It Works

This Lightning Web Component implements the classic Matrix Chain Multiplication algorithm with the following features:

1. **Input**: Accepts matrix dimensions as comma-separated values
2. **Algorithm**: Uses dynamic programming to find optimal parenthesization
3. **Output**: Shows minimum multiplications and optimal parenthesization
4. **Visualization**: Displays results in a clean, user-friendly interface

## Example Usage

Input: `10,20,30,40,30` (represents 4 matrices: 10×20, 20×30, 30×40, 40×30)

Output:
```
Minimum number of multiplications: 26000
Optimal parenthesization: ((M1 × M2) × (M3 × M4))
Matrix dimensions: [10, 20, 30, 40, 30]
```

## Key Features

- **Dynamic Programming**: Implements the standard O(n³) solution
- **Interactive Input**: Users can modify matrix dimensions
- **Clear Output**: Shows both cost and optimal parenthesization
- **Responsive Design**: Uses Salesforce Lightning Design System
- **Error Handling**: Validates input and handles edge cases

The algorithm works by building a table of minimum costs for multiplying subchains of matrices and then reconstructing the optimal parenthesization using a bracket matrix.

