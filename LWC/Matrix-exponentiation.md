# Matrix Exponentiation in Lightning Web Component

Here's an example of a matrix exponentiation algorithm implemented in Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class MatrixExponentiation extends LightningElement {
    matrix = [
        [1, 2],
        [3, 4]
    ];
    power = 3;
    result = [];

    // Matrix multiplication
    multiplyMatrices(a, b) {
        const rowsA = a.length;
        const colsA = a[0].length;
        const colsB = b[0].length;
        
        const result = Array(rowsA).fill().map(() => Array(colsB).fill(0));
        
        for (let i = 0; i < rowsA; i++) {
            for (let j = 0; j < colsB; j++) {
                for (let k = 0; k < colsA; k++) {
                    result[i][j] += a[i][k] * b[k][j];
                }
            }
        }
        
        return result;
    }

    // Matrix exponentiation using fast exponentiation
    matrixPower(matrix, power) {
        if (power === 0) {
            // Return identity matrix
            const size = matrix.length;
            const identity = Array(size).fill().map(() => Array(size).fill(0));
            for (let i = 0; i < size; i++) {
                identity[i][i] = 1;
            }
            return identity;
        }
        
        if (power === 1) {
            return matrix;
        }
        
        if (power % 2 === 0) {
            // If power is even, use: A^n = (A^(n/2))^2
            const halfPower = this.matrixPower(matrix, Math.floor(power / 2));
            return this.multiplyMatrices(halfPower, halfPower);
        } else {
            // If power is odd, use: A^n = A * A^(n-1)
            return this.multiplyMatrices(matrix, this.matrixPower(matrix, power - 1));
        }
    }

    // Fast matrix exponentiation using binary exponentiation
    fastMatrixPower(matrix, power) {
        if (power === 0) {
            const size = matrix.length;
            const identity = Array(size).fill().map(() => Array(size).fill(0));
            for (let i = 0; i < size; i++) {
                identity[i][i] = 1;
            }
            return identity;
        }
        
        let result = [];
        let base = matrix;
        let exp = power;
        
        // Initialize result as identity matrix
        const size = matrix.length;
        result = Array(size).fill().map(() => Array(size).fill(0));
        for (let i = 0; i < size; i++) {
            result[i][i] = 1;
        }
        
        while (exp > 0) {
            if (exp % 2 === 1) {
                result = this.multiplyMatrices(result, base);
            }
            base = this.multiplyMatrices(base, base);
            exp = Math.floor(exp / 2);
        }
        
        return result;
    }

    handleCalculate() {
        try {
            const result = this.fastMatrixPower(this.matrix, this.power);
            this.result = result;
        } catch (error) {
            console.error('Error in matrix exponentiation:', error);
        }
    }

    handlePowerChange(event) {
        this.power = parseInt(event.target.value) || 0;
    }

    handleMatrixChange(event) {
        const value = event.target.value;
        const rows = value.split('\n');
        this.matrix = rows.map(row => 
            row.trim().split(' ').map(Number)
        );
    }

    // Example usage
    connectedCallback() {
        // Example: Calculate [[1,2],[3,4]]^3
        this.result = this.fastMatrixPower(this.matrix, this.power);
    }
}
```

```html
<template>
    <div class="matrix-exponentiation">
        <h2>Matrix Exponentiation</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Matrix (rows separated by newlines, elements by spaces)" 
                type="textarea"
                value="1 2
3 4"
                onchange={handleMatrixChange}>
            </lightning-input>
            
            <lightning-input 
                label="Power" 
                type="number"
                value={power}
                onchange={handlePowerChange}>
            </lightning-input>
            
            <lightning-button 
                label="Calculate" 
                onclick={handleCalculate}>
            </lightning-button>
        </div>
        
        <div class="result-section">
            <h3>Result:</h3>
            <template if:true={result.length}>
                <table class="result-table">
                    <tbody>
                        <template for:each={result} for:item="row">
                            <tr key={row}>
                                <template for:each={row} for:item="cell">
                                    <td key={cell}>{cell}</td>
                                </template>
                            </tr>
                        </template>
                    </tbody>
                </table>
            </template>
        </div>
    </div>
</template>
```

```css
.matrix-exponentiation {
    padding: 20px;
    max-width: 600px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.result-table {
    border-collapse: collapse;
    margin-top: 10px;
}

.result-table td {
    border: 1px solid #ccc;
    padding: 8px;
    text-align: center;
}
```

## Key Features:

1. **Fast Exponentiation**: Uses binary exponentiation algorithm for O(log n) complexity
2. **Matrix Multiplication**: Implements standard matrix multiplication
3. **Identity Matrix**: Handles base case for power = 0
4. **Error Handling**: Includes basic error handling for invalid inputs
5. **Interactive UI**: Allows users to input custom matrices and exponents

## Algorithm Complexity:
- **Time Complexity**: O(log n × m³) where n is the power and m is matrix dimension
- **Space Complexity**: O(log n) for recursive calls or O(1) for iterative approach

This implementation demonstrates how to perform matrix exponentiation efficiently in Lightning Web Components, which is commonly used in competitive programming and mathematical applications.

