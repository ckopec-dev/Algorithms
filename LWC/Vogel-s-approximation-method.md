# Vogel's Approximation Method in Lightning Web Component

Here's a complete implementation of Vogel's Approximation Method (VAM) for solving transportation problems in Lightning Web Component:

```javascript
// vamAlgorithm.js
import { LightningElement, track } from 'lwc';

export default class VamAlgorithm extends LightningElement {
    @track supply = [30, 20, 10];
    @track demand = [15, 25, 10];
    @track costMatrix = [
        [8, 6, 10],
        [9, 12, 13],
        [14, 9, 16]
    ];
    
    @track resultMatrix = [];
    @track totalCost = 0;
    @track steps = [];
    
    connectedCallback() {
        this.runVAM();
    }
    
    runVAM() {
        // Reset results
        this.resultMatrix = JSON.parse(JSON.stringify(this.costMatrix));
        this.totalCost = 0;
        this.steps = [];
        
        // Create supply and demand arrays with indices
        let supplyCopy = this.supply.map((val, index) => ({ 
            value: val, 
            index: index,
            allocated: 0 
        }));
        
        let demandCopy = this.demand.map((val, index) => ({ 
            value: val, 
            index: index,
            allocated: 0 
        }));
        
        // Initialize result matrix with zeros
        let result = Array(this.supply.length).fill().map(() => Array(this.demand.length).fill(0));
        
        // Step-by-step process
        let stepCount = 0;
        
        while (supplyCopy.some(s => s.value > 0) || demandCopy.some(d => d.value > 0)) {
            stepCount++;
            this.steps.push(`Step ${stepCount}:`);
            
            // Calculate row penalties
            let rowPenalties = [];
            for (let i = 0; i < supplyCopy.length; i++) {
                if (supplyCopy[i].value > 0) {
                    let costs = [];
                    for (let j = 0; j < demandCopy.length; j++) {
                        if (demandCopy[j].value > 0) {
                            costs.push(this.costMatrix[i][j]);
                        }
                    }
                    if (costs.length >= 2) {
                        costs.sort((a, b) => a - b);
                        rowPenalties.push(costs[1] - costs[0]);
                    } else {
                        rowPenalties.push(0);
                    }
                } else {
                    rowPenalties.push(0);
                }
            }
            
            // Calculate column penalties
            let colPenalties = [];
            for (let j = 0; j < demandCopy.length; j++) {
                if (demandCopy[j].value > 0) {
                    let costs = [];
                    for (let i = 0; i < supplyCopy.length; i++) {
                        if (supplyCopy[i].value > 0) {
                            costs.push(this.costMatrix[i][j]);
                        }
                    }
                    if (costs.length >= 2) {
                        costs.sort((a, b) => a - b);
                        colPenalties.push(costs[1] - costs[0]);
                    } else {
                        colPenalties.push(0);
                    }
                } else {
                    colPenalties.push(0);
                }
            }
            
            this.steps.push(`Row penalties: [${rowPenalties.join(', ')}]`);
            this.steps.push(`Column penalties: [${colPenalties.join(', ')}]`);
            
            // Find maximum penalty
            let maxRowPenalty = Math.max(...rowPenalties);
            let maxColPenalty = Math.max(...colPenalties);
            
            this.steps.push(`Max row penalty: ${maxRowPenalty}, Max column penalty: ${maxColPenalty}`);
            
            let maxPenalty = Math.max(maxRowPenalty, maxColPenalty);
            let selectedRow = -1;
            let selectedCol = -1;
            
            if (maxPenalty === maxRowPenalty && maxRowPenalty > 0) {
                // Select row with maximum penalty
                selectedRow = rowPenalties.indexOf(maxRowPenalty);
                this.steps.push(`Selected row ${selectedRow} with max penalty`);
                
                // Find minimum cost in this row
                let minCost = Infinity;
                for (let j = 0; j < demandCopy.length; j++) {
                    if (demandCopy[j].value > 0 && this.costMatrix[selectedRow][j] < minCost) {
                        minCost = this.costMatrix[selectedRow][j];
                        selectedCol = j;
                    }
                }
            } else if (maxPenalty === maxColPenalty && maxColPenalty > 0) {
                // Select column with maximum penalty
                selectedCol = colPenalties.indexOf(maxColPenalty);
                this.steps.push(`Selected column ${selectedCol} with max penalty`);
                
                // Find minimum cost in this column
                let minCost = Infinity;
                for (let i = 0; i < supplyCopy.length; i++) {
                    if (supplyCopy[i].value > 0 && this.costMatrix[i][selectedCol] < minCost) {
                        minCost = this.costMatrix[i][selectedCol];
                        selectedRow = i;
                    }
                }
            }
            
            // Allocate
            if (selectedRow !== -1 && selectedCol !== -1) {
                let allocation = Math.min(supplyCopy[selectedRow].value, demandCopy[selectedCol].value);
                result[selectedRow][selectedCol] = allocation;
                
                this.totalCost += allocation * this.costMatrix[selectedRow][selectedCol];
                
                supplyCopy[selectedRow].value -= allocation;
                demandCopy[selectedCol].value -= allocation;
                
                this.steps.push(`Allocated ${allocation} units from supply ${selectedRow} to demand ${selectedCol}`);
                this.steps.push(`Cost: ${allocation} × ${this.costMatrix[selectedRow][selectedCol]} = ${allocation * this.costMatrix[selectedRow][selectedCol]}`);
            } else {
                break;
            }
        }
        
        this.resultMatrix = result;
        this.steps.push(`Total transportation cost: ${this.totalCost}`);
    }
    
    get supplyTotal() {
        return this.supply.reduce((sum, val) => sum + val, 0);
    }
    
    get demandTotal() {
        return this.demand.reduce((sum, val) => sum + val, 0);
    }
    
    get isBalanced() {
        return this.supplyTotal === this.demandTotal;
    }
}
```

```html
<!-- vamAlgorithm.html -->
<template>
    <div class="container">
        <h2>Vogel's Approximation Method</h2>
        
        <div class="info-section">
            <p><strong>Supply:</strong> {supply}</p>
            <p><strong>Demand:</strong> {demand}</p>
            <p><strong>Supply Total:</strong> {supplyTotal}</p>
            <p><strong>Demand Total:</strong> {demandTotal}</p>
            <p><strong>Balanced:</strong> {isBalanced}</p>
        </div>
        
        <div class="matrix-section">
            <h3>Cost Matrix</h3>
            <table class="cost-table">
                <thead>
                    <tr>
                        <th></th>
                        <template for:each={demand} for:item="demandVal">
                            <th key={demandVal}>Demand {demandVal.index}</th>
                        </template>
                    </tr>
                </thead>
                <tbody>
                    <template for:each={costMatrix} for:item="row">
                        <tr key={row.index}>
                            <td>Supply {row.index}</td>
                            <template for:each={row.value} for:item="cost">
                                <td key={cost.index}>{cost.value}</td>
                            </template>
                        </tr>
                    </template>
                </tbody>
            </table>
        </div>
        
        <div class="result-section">
            <h3>Allocation Result</h3>
            <table class="result-table">
                <thead>
                    <tr>
                        <th></th>
                        <template for:each={demand} for:item="demandVal">
                            <th key={demandVal.index}>Demand {demandVal.index}</th>
                        </template>
                        <th>Total</th>
                    </tr>
                </thead>
                <tbody>
                    <template for:each={resultMatrix} for:item="row">
                        <tr key={row.index}>
                            <td>Supply {row.index}</td>
                            <template for:each={row.value} for:item="allocation">
                                <td key={allocation.index}>{allocation.value}</td>
                            </template>
                            <td>{getSupplyTotal(row.value)}</td>
                        </tr>
                    </template>
                    <tr>
                        <td><strong>Total</strong></td>
                        <template for:each={demand} for:item="demandVal">
                            <td key={demandVal.index}>{getDemandTotal(demandVal.index)}</td>
                        </template>
                        <td><strong>{totalCost}</strong></td>
                    </tr>
                </tbody>
            </table>
        </div>
        
        <div class="steps-section">
            <h3>Algorithm Steps</h3>
            <ul class="steps-list">
                <template for:each={steps} for:item="step">
                    <li key={step}>{step}</li>
                </template>
            </ul>
        </div>
    </div>
</template>
```

```css
/* vamAlgorithm.css */
.container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.info-section {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.matrix-section {
    margin-bottom: 20px;
}

.cost-table, .result-table {
    border-collapse: collapse;
    width: 100%;
    margin-bottom: 20px;
}

.cost-table th, .cost-table td, 
.result-table th, .result-table td {
    border: 1px solid #ddd;
    padding: 12px;
    text-align: center;
}

.cost-table th, .result-table th {
    background-color: #4CAF50;
    color: white;
}

.cost-table tr:nth-child(even), .result-table tr:nth-child(even) {
    background-color: #f2f2f2;
}

.steps-section {
    background-color: #e8f4f8;
    padding: 15px;
    border-radius: 5px;
}

.steps-list {
    list-style-type: decimal;
    padding-left: 20px;
}

.steps-list li {
    margin-bottom: 5px;
}
```

## Key Features of this Implementation:

1. **Complete VAM Algorithm**: Implements all steps of Vogel's Approximation Method
2. **Step-by-Step Execution**: Shows each step of the algorithm in the UI
3. **Visual Matrix Display**: Displays cost matrix and allocation results
4. **Cost Calculation**: Automatically calculates total transportation cost
5. **Balanced Check**: Verifies if supply equals demand
6. **Responsive Design**: Clean, readable output format

## How VAM Works:

1. **Calculate penalties** for each row and column
2. **Select maximum penalty** row or column
3. **Allocate minimum cost** in selected row/column
4. **Update supply/demand** and repeat until all allocations are made
5. **Calculate total cost** of the solution

This implementation provides a clear visualization of how VAM works and produces an optimal initial solution for transportation problems.

