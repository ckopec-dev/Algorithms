# Gauss-Jordan Elimination in Lightning Web Component

Here's a complete example of implementing the Gauss-Jordan elimination algorithm in a Lightning Web Component:

## JavaScript Controller (gaussJordan.js)

```javascript
import { LightningElement } from 'lwc';

export default class GaussJordan extends LightningElement {
    matrix = [
        [2, 1, -1, 8],
        [-3, -1, 2, -11],
        [-2, 1, 2, -3]
    ];
    
    solution = [];
    steps = [];
    showSteps = false;

    get matrixRows() {
        return this.matrix.map((row, index) => ({
            id: index,
            values: row
        }));
    }

    handleMatrixChange(event) {
        const rowId = event.target.dataset.row;
        const colId = event.target.dataset.col;
        const value = parseFloat(event.target.value) || 0;
        
        this.matrix[rowId][colId] = value;
        this.requestUpdate();
    }

    handleSolve() {
        this.steps = [];
        this.solution = this.gaussJordanElimination(this.matrix);
        this.showSteps = true;
    }

    gaussJordanElimination(matrix) {
        const augmentedMatrix = matrix.map(row => [...row]);
        const rows = augmentedMatrix.length;
        const cols = augmentedMatrix[0].length;
        
        this.steps.push({
            title: "Initial Matrix",
            matrix: this.cloneMatrix(augmentedMatrix)
        });

        // Forward elimination
        for (let i = 0; i < rows; i++) {
            // Find pivot
            let maxRow = i;
            for (let k = i + 1; k < rows; k++) {
                if (Math.abs(augmentedMatrix[k][i]) > Math.abs(augmentedMatrix[maxRow][i])) {
                    maxRow = k;
                }
            }
            
            // Swap rows
            if (maxRow !== i) {
                [augmentedMatrix[i], augmentedMatrix[maxRow]] = [augmentedMatrix[maxRow], augmentedMatrix[i]];
                this.steps.push({
                    title: `Swap rows ${i} and ${maxRow}`,
                    matrix: this.cloneMatrix(augmentedMatrix)
                });
            }

            // Make all rows below this one 0 in current column
            for (let k = i + 1; k < rows; k++) {
                const factor = augmentedMatrix[k][i] / augmentedMatrix[i][i];
                for (let j = i; j < cols; j++) {
                    augmentedMatrix[k][j] -= factor * augmentedMatrix[i][j];
                }
                this.steps.push({
                    title: `Eliminate element at row ${k}, column ${i}`,
                    matrix: this.cloneMatrix(augmentedMatrix)
                });
            }
        }

        // Backward elimination
        for (let i = rows - 1; i >= 0; i--) {
            // Make diagonal element 1
            const factor = augmentedMatrix[i][i];
            for (let j = i; j < cols; j++) {
                augmentedMatrix[i][j] /= factor;
            }
            this.steps.push({
                title: `Make diagonal element 1 in row ${i}`,
                matrix: this.cloneMatrix(augmentedMatrix)
            });

            // Make all elements above this one 0
            for (let k = i - 1; k >= 0; k--) {
                const factor = augmentedMatrix[k][i];
                for (let j = i; j < cols; j++) {
                    augmentedMatrix[k][j] -= factor * augmentedMatrix[i][j];
                }
                this.steps.push({
                    title: `Eliminate element at row ${k}, column ${i}`,
                    matrix: this.cloneMatrix(augmentedMatrix)
                });
            }
        }

        // Extract solution
        const solution = [];
        for (let i = 0; i < rows; i++) {
            solution.push(augmentedMatrix[i][cols - 1]);
        }

        return solution;
    }

    cloneMatrix(matrix) {
        return matrix.map(row => [...row]);
    }

    handleReset() {
        this.solution = [];
        this.steps = [];
        this.showSteps = false;
    }
}
```

## HTML Template (gaussJordan.html)

```html
<template>
    <div class="slds-box slds-box_small slds-m-around_medium">
        <h2 class="slds-text-heading_small slds-m-bottom_medium">Gauss-Jordan Elimination</h2>
        
        <div class="slds-grid slds-gutters slds-m-bottom_medium">
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-media slds-media_center slds-has-flexi-truncate">
                            <div class="slds-media__body">
                                <h3 class="slds-card__header-title slds-truncate" title="Matrix Input">
                                    Matrix Input
                                </h3>
                            </div>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                            <thead>
                                <tr class="slds-line-height_reset">
                                    <th scope="col" class="slds-text-title_caps">Column</th>
                                    <template for:each={matrix[0]} for:item="col" for:index="index">
                                        <th key={index} scope="col" class="slds-text-title_caps">
                                            {index}
                                        </th>
                                    </template>
                                </tr>
                            </thead>
                            <tbody>
                                <template for:each={matrixRows} for:item="row" for:index="index">
                                    <tr key={row.id}>
                                        <th scope="row" class="slds-text-title_caps">
                                            Row {index}
                                        </th>
                                        <template for:each={row.values} for:item="value" for:index="colIndex">
                                            <td key={colIndex}>
                                                <lightning-input 
                                                    type="number"
                                                    value={value}
                                                    data-row={index}
                                                    data-col={colIndex}
                                                    onchange={handleMatrixChange}
                                                    variant="label-hidden"
                                                    class="slds-m-around_xxx-small">
                                                </lightning-input>
                                            </td>
                                        </template>
                                    </tr>
                                </template>
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-media slds-media_center slds-has-flexi-truncate">
                            <div class="slds-media__body">
                                <h3 class="slds-card__header-title slds-truncate" title="Results">
                                    Results
                                </h3>
                            </div>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <template if:true={solution.length > 0}>
                            <div class="slds-m-bottom_medium">
                                <p class="slds-text-body_small">Solution:</p>
                                <template for:each={solution} for:item="value" for:index="index">
                                    <div key={index} class="slds-grid slds-gutters slds-m-bottom_xxx-small">
                                        <div class="slds-col slds-size_1-of-3">
                                            <span class="slds-text-body_small">x{index} =</span>
                                        </div>
                                        <div class="slds-col slds-size_2-of-3">
                                            <lightning-input 
                                                type="number"
                                                value={value}
                                                disabled="true"
                                                variant="label-hidden">
                                            </lightning-input>
                                        </div>
                                    </div>
                                </template>
                            </div>
                        </template>
                        
                        <lightning-button 
                            label="Solve" 
                            variant="brand" 
                            onclick={handleSolve}
                            class="slds-m-bottom_medium">
                        </lightning-button>
                        
                        <lightning-button 
                            label="Reset" 
                            variant="neutral" 
                            onclick={handleReset}
                            class="slds-m-left_xxx-small">
                        </lightning-button>
                    </div>
                </div>
            </div>
        </div>
        
        <template if:true={showSteps}>
            <div class="slds-card">
                <div class="slds-card__header slds-grid">
                    <header class="slds-media slds-media_center slds-has-flexi-truncate">
                        <div class="slds-media__body">
                            <h3 class="slds-card__header-title slds-truncate" title="Solution Steps">
                                Solution Steps
                            </h3>
                        </div>
                    </header>
                </div>
                <div class="slds-card__body">
                    <template for:each={steps} for:item="step">
                        <div class="slds-m-bottom_medium">
                            <h4 class="slds-text-heading_small">{step.title}</h4>
                            <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                                <tbody>
                                    <template for:each={step.matrix} for:item="row">
                                        <tr>
                                            <template for:each={row} for:item="value">
                                                <td class="slds-text-body_small slds-p-around_xxx-small">
                                                    {value}
                                                </td>
                                            </template>
                                        </tr>
                                    </template>
                                </tbody>
                            </table>
                        </div>
                    </template>
                </div>
            </div>
        </template>
    </div>
</template>
```

## CSS Styles (gaussJordan.css)

```css
.slds-table_cell-buffer td,
.slds-table_cell-buffer th {
    padding: 0.25rem;
}

.slds-text-title_caps {
    text-transform: capitalize;
}

.slds-m-around_xxx-small {
    margin: 0.125rem;
}

.slds-m-bottom_xxx-small {
    margin-bottom: 0.125rem;
}
```

## Usage Example

This component demonstrates the Gauss-Jordan elimination method for solving systems of linear equations. The example uses the following system:

```
2x + y - z = 8
-3x - y + 2z = -11
-2x + y + 2z = -3
```

The algorithm performs:
1. Forward elimination to create an upper triangular matrix
2. Backward elimination to create a reduced row echelon form
3. Extracts the solution vector

The component provides:
- Interactive matrix input
- Step-by-step solution visualization
- Clear solution display
- Reset functionality

The Gauss-Jordan elimination process transforms the augmented matrix into reduced row echelon form, where the solution can be directly read from the last column.

