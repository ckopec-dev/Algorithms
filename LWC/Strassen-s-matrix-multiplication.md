# Strassen's Matrix Multiplication in Lightning Web Component

Here's a complete example of implementing Strassen's matrix multiplication algorithm in a Lightning Web Component:

## JavaScript Controller (matrixMultiplication.js)

```javascript
import { LightningElement } from 'lwc';

export default class MatrixMultiplication extends LightningElement {
    matrixA = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
    ];
    
    matrixB = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
    ];
    
    resultMatrix = [];
    isCalculated = false;

    get matrixAData() {
        return this.matrixA.map(row => row.join(', ')).join('\n');
    }

    get matrixBData() {
        return this.matrixB.map(row => row.join(', ')).join('\n');
    }

    get resultData() {
        if (!this.isCalculated) return '';
        return this.resultMatrix.map(row => row.join(', ')).join('\n');
    }

    handleCalculate() {
        try {
            this.resultMatrix = this.strassenMultiply(this.matrixA, this.matrixB);
            this.isCalculated = true;
        } catch (error) {
            console.error('Error in matrix multiplication:', error);
        }
    }

    strassenMultiply(A, B) {
        const n = A.length;
        
        // Base case: if matrix is 1x1
        if (n === 1) {
            return [[A[0][0] * B[0][0]]];
        }

        // Check if matrix size is power of 2
        if (n % 2 !== 0) {
            throw new Error('Matrix size must be a power of 2 for Strassen\'s algorithm');
        }

        // Divide matrices into quadrants
        const half = n / 2;
        
        // Initialize submatrices
        const A11 = this.getSubMatrix(A, 0, 0, half);
        const A12 = this.getSubMatrix(A, 0, half, half);
        const A21 = this.getSubMatrix(A, half, 0, half);
        const A22 = this.getSubMatrix(A, half, half, half);
        
        const B11 = this.getSubMatrix(B, 0, 0, half);
        const B12 = this.getSubMatrix(B, 0, half, half);
        const B21 = this.getSubMatrix(B, half, 0, half);
        const B22 = this.getSubMatrix(B, half, half, half);

        // Calculate Strassen's seven products
        const M1 = this.strassenMultiply(this.matrixAdd(A11, A22), this.matrixAdd(B11, B22));
        const M2 = this.strassenMultiply(this.matrixAdd(A21, A22), B11);
        const M3 = this.strassenMultiply(A11, this.matrixSubtract(B12, B22));
        const M4 = this.strassenMultiply(A22, this.matrixSubtract(B21, B11));
        const M5 = this.strassenMultiply(this.matrixAdd(A11, A12), B22);
        const M6 = this.strassenMultiply(this.matrixSubtract(A21, A11), this.matrixAdd(B11, B12));
        const M7 = this.strassenMultiply(this.matrixSubtract(A12, A22), this.matrixAdd(B21, B22));

        // Calculate result quadrants
        const C11 = this.matrixAdd(this.matrixSubtract(this.matrixAdd(M1, M4), M5), M7);
        const C12 = this.matrixAdd(M3, M5);
        const C21 = this.matrixAdd(M2, M4);
        const C22 = this.matrixSubtract(this.matrixAdd(M1, M3), M2);

        // Combine quadrants into result matrix
        return this.combineMatrices(C11, C12, C21, C22);
    }

    matrixAdd(A, B) {
        const n = A.length;
        const result = [];
        
        for (let i = 0; i < n; i++) {
            result[i] = [];
            for (let j = 0; j < n; j++) {
                result[i][j] = A[i][j] + B[i][j];
            }
        }
        
        return result;
    }

    matrixSubtract(A, B) {
        const n = A.length;
        const result = [];
        
        for (let i = 0; i < n; i++) {
            result[i] = [];
            for (let j = 0; j < n; j++) {
                result[i][j] = A[i][j] - B[i][j];
            }
        }
        
        return result;
    }

    getSubMatrix(matrix, startRow, startCol, size) {
        const result = [];
        
        for (let i = 0; i < size; i++) {
            result[i] = [];
            for (let j = 0; j < size; j++) {
                result[i][j] = matrix[startRow + i][startCol + j];
            }
        }
        
        return result;
    }

    combineMatrices(C11, C12, C21, C22) {
        const n = C11.length * 2;
        const result = [];
        
        // Initialize result matrix
        for (let i = 0; i < n; i++) {
            result[i] = [];
        }
        
        // Fill in the quadrants
        for (let i = 0; i < C11.length; i++) {
            for (let j = 0; j < C11.length; j++) {
                result[i][j] = C11[i][j];
                result[i][j + C11.length] = C12[i][j];
                result[i + C11.length][j] = C21[i][j];
                result[i + C11.length][j + C11.length] = C22[i][j];
            }
        }
        
        return result;
    }

    handleMatrixAChange(event) {
        const input = event.target.value;
        this.matrixA = this.parseMatrix(input);
    }

    handleMatrixBChange(event) {
        const input = event.target.value;
        this.matrixB = this.parseMatrix(input);
    }

    parseMatrix(input) {
        const rows = input.trim().split('\n');
        return rows.map(row => 
            row.trim().split(',').map(num => parseInt(num.trim()) || 0)
        );
    }
}
```

## HTML Template (matrixMultiplication.html)

```html
<template>
    <div class="container">
        <h2>Strassen's Matrix Multiplication</h2>
        
        <lightning-card title="Matrix A">
            <div class="matrix-input">
                <lightning-textarea 
                    label="Enter Matrix A (rows separated by newline, columns by comma)"
                    value={matrixAData}
                    onchange={handleMatrixAChange}
                    rows="4">
                </lightning-textarea>
            </div>
        </lightning-card>

        <lightning-card title="Matrix B">
            <div class="matrix-input">
                <lightning-textarea 
                    label="Enter Matrix B (rows separated by newline, columns by comma)"
                    value={matrixBData}
                    onchange={handleMatrixBChange}
                    rows="4">
                </lightning-textarea>
            </div>
        </lightning-card>

        <lightning-button 
            label="Calculate Product" 
            variant="brand" 
            onclick={handleCalculate}
            class="slds-m-top_small">
        </lightning-button>

        <lightning-card title="Result Matrix" if:true={isCalculated}>
            <div class="matrix-output">
                <lightning-textarea 
                    label="Result of A × B"
                    value={resultData}
                    readonly
                    rows="8">
                </lightning-textarea>
            </div>
        </lightning-card>

        <lightning-card title="Algorithm Information">
            <p>Strassen's algorithm is a divide-and-conquer approach to matrix multiplication.</p>
            <p>Time complexity: O(n^2.807) vs O(n^3) for standard multiplication.</p>
            <p>Works best for large matrices where the overhead of recursion is worth the improvement.</p>
        </lightning-card>
    </div>
</template>
```

## CSS Styling (matrixMultiplication.css)

```css
.container {
    padding: 1rem;
    max-width: 800px;
    margin: 0 auto;
}

.matrix-input {
    margin-bottom: 1rem;
}

.matrix-output {
    margin-top: 1rem;
}

.slds-m-top_small {
    margin-top: 1rem !important;
}

.lightning-card {
    margin-bottom: 1rem;
}
```

## Key Features of this Implementation:

1. **Complete Strassen's Algorithm**: Implements the full 7-matrix multiplication approach
2. **Recursive Division**: Properly divides matrices into quadrants
3. **Input Validation**: Checks for valid matrix sizes
4. **User Interface**: Interactive input fields for matrices
5. **Error Handling**: Graceful error handling for invalid inputs
6. **Responsive Design**: Uses Lightning Design System components

## How to Use:

1. Enter two 4x4 matrices in the input fields (format: rows separated by newlines, columns by commas)
2. Click "Calculate Product" 
3. View the result matrix in the output section
4. The algorithm automatically handles the recursive division and combination

## Notes:

- This implementation assumes square matrices of size that's a power of 2
- For smaller matrices, standard multiplication would be more efficient
- The algorithm demonstrates the theoretical advantage of O(n^2.807) complexity over O(n^3)

