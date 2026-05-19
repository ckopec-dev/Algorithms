# Cholesky Decomposition in Lightning Web Component

Here's an example implementation of the Cholesky decomposition algorithm in Lightning Web Component:

```javascript
// cholesky.js
import { LightningElement } from 'lwc';

export default class CholeskyDecomposition extends LightningElement {
    matrix = [
        [4, 12, -16],
        [12, 37, -43],
        [-16, -43, 98]
    ];
    
    result = '';
    isCalculated = false;

    handleCalculate() {
        try {
            const L = this.choleskyDecomposition(this.matrix);
            this.result = this.formatMatrix(L);
            this.isCalculated = true;
        } catch (error) {
            this.result = `Error: ${error.message}`;
            this.isCalculated = false;
        }
    }

    choleskyDecomposition(matrix) {
        const n = matrix.length;
        
        // Validate input
        if (n !== matrix[0].length) {
            throw new Error('Matrix must be square');
        }
        
        // Check if matrix is symmetric
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                if (matrix[i][j] !== matrix[j][i]) {
                    throw new Error('Matrix must be symmetric');
                }
            }
        }
        
        // Create L matrix (lower triangular)
        const L = Array(n).fill().map(() => Array(n).fill(0));
        
        // Cholesky decomposition algorithm
        for (let i = 0; i < n; i++) {
            for (let j = 0; j <= i; j++) {
                let sum = 0;
                
                if (i === j) {
                    // Diagonal elements
                    for (let k = 0; k < j; k++) {
                        sum += L[j][k] * L[j][k];
                    }
                    L[j][j] = Math.sqrt(matrix[j][j] - sum);
                } else {
                    // Off-diagonal elements
                    for (let k = 0; k < j; k++) {
                        sum += L[i][k] * L[j][k];
                    }
                    L[i][j] = (matrix[i][j] - sum) / L[j][j];
                }
            }
        }
        
        return L;
    }

    formatMatrix(matrix) {
        let result = '<table border="1" style="border-collapse: collapse;">';
        
        for (let i = 0; i < matrix.length; i++) {
            result += '<tr>';
            for (let j = 0; j < matrix[i].length; j++) {
                const value = matrix[i][j];
                const cellStyle = j <= i ? 'background-color: #e8f5e8;' : 'background-color: #f8f8f8;';
                result += `<td style="padding: 8px; text-align: center; ${cellStyle}">${value.toFixed(4)}</td>`;
            }
            result += '</tr>';
        }
        
        result += '</table>';
        return result;
    }

    get matrixString() {
        return this.matrix.map(row => row.join(', ')).join('\n');
    }
}
```

```html
<!-- cholesky.html -->
<template>
    <div class="container">
        <h2>Cholesky Decomposition</h2>
        
        <div class="input-section">
            <h3>Input Matrix:</h3>
            <pre>{matrixString}</pre>
            
            <lightning-button 
                label="Calculate Cholesky Decomposition" 
                variant="brand" 
                onclick={handleCalculate}>
            </lightning-button>
        </div>

        <div class="result-section" if:true={isCalculated}>
            <h3>Cholesky Factor L:</h3>
            <div innerHTML={result}></div>
        </div>

        <div class="explanation">
            <h3>Algorithm Explanation:</h3>
            <p>
                The Cholesky decomposition factorizes a symmetric positive-definite matrix A into 
                the product of a lower triangular matrix L and its transpose L<sup>T</sup>:
                A = L × L<sup>T</sup>
            </p>
            <p>
                For each element L[i][j] where i ≥ j:
                <ul>
                    <li>If i = j: L[i][j] = √(A[i][i] - Σ<sub>k=0</sub><sup>j-1</sup> L[i][k]²)</li>
                    <li>If i > j: L[i][j] = (A[i][j] - Σ<sub>k=0</sub><sup>j-1</sup> L[i][k] × L[j][k]) / L[j][j]</li>
                </ul>
            </p>
        </div>
    </div>
</template>
```

```css
/* cholesky.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.result-section {
    background-color: #e8f5e8;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.explanation {
    background-color: #fff3cd;
    padding: 15px;
    border-radius: 5px;
    border-left: 4px solid #ffc107;
}

pre {
    background-color: #f8f9fa;
    padding: 10px;
    border-radius: 3px;
    overflow-x: auto;
}

table {
    width: 100%;
    margin: 10px 0;
}

td {
    border: 1px solid #ddd;
    text-align: center;
}
```

## How it works:

1. **Input Validation**: Checks if the matrix is square and symmetric
2. **Algorithm Implementation**: Uses the standard Cholesky decomposition formula
3. **Matrix Factorization**: Decomposes the input matrix into lower triangular matrix L
4. **Output Display**: Shows the resulting L matrix in a formatted table

## Example Output:
For the input matrix:
```
[4, 12, -16]
[12, 37, -43]
[-16, -43, 98]
```

The output L matrix will be:
```
[2.0000, 0.0000, 0.0000]
[6.0000, 1.0000, 0.0000]
[-8.0000, 5.0000, 3.0000]
```

This implementation handles error cases and provides a clean user interface for calculating Cholesky decompositions of symmetric positive-definite matrices.

