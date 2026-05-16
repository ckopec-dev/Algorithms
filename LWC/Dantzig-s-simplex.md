# Dantzig's Simplex Algorithm in Lightning Web Component

Here's a complete implementation of Dantzig's Simplex algorithm in a Lightning Web Component:

```javascript
// simplex.js
import { LightningElement } from 'lwc';

export default class SimplexAlgorithm extends LightningElement {
    // Input data for the linear programming problem
    input = {
        objective: [3, 2], // Coefficients of objective function
        constraints: [
            [1, 2], // Constraint 1: x1 + 2x2 <= 2
            [2, 1], // Constraint 2: 2x1 + x2 <= 3
            [1, 0], // Constraint 3: x1 <= 1
        ],
        rhs: [2, 3, 1], // Right-hand side values
        isMaximize: true // True for maximization, false for minimization
    };

    // Results and steps
    result = {
        optimalValue: 0,
        optimalSolution: [],
        steps: []
    };

    // Run the simplex algorithm
    handleRunSimplex() {
        const { objective, constraints, rhs, isMaximize } = this.input;
        const solution = this.runSimplex(objective, constraints, rhs, isMaximize);
        
        this.result = {
            optimalValue: solution.value,
            optimalSolution: solution.solution,
            steps: solution.steps
        };
    }

    runSimplex(objective, constraints, rhs, isMaximize) {
        const steps = [];
        let tableau = this.initializeTableau(objective, constraints, rhs, isMaximize);
        
        steps.push({
            step: 0,
            tableau: JSON.parse(JSON.stringify(tableau)),
            message: "Initial tableau created"
        });

        let iteration = 1;
        let isOptimal = false;

        while (!isOptimal) {
            // Find entering variable (most negative coefficient in objective row)
            const enteringVar = this.findEnteringVariable(tableau);
            
            if (enteringVar === -1) {
                isOptimal = true;
                steps.push({
                    step: iteration,
                    tableau: JSON.parse(JSON.stringify(tableau)),
                    message: "Optimal solution found"
                });
                break;
            }

            // Find leaving variable (minimum ratio test)
            const leavingVar = this.findLeavingVariable(tableau, enteringVar);
            
            if (leavingVar === -1) {
                steps.push({
                    step: iteration,
                    tableau: JSON.parse(JSON.stringify(tableau)),
                    message: "Unbounded solution"
                });
                return {
                    value: Infinity,
                    solution: [],
                    steps: steps
                };
            }

            // Pivot operation
            tableau = this.pivot(tableau, enteringVar, leavingVar);
            
            steps.push({
                step: iteration,
                tableau: JSON.parse(JSON.stringify(tableau)),
                message: `Pivoting on column ${enteringVar}, row ${leavingVar}`
            });

            iteration++;
        }

        const solution = this.extractSolution(tableau, constraints.length);
        
        return {
            value: isMaximize ? tableau[tableau.length - 1][tableau[0].length - 1] : -tableau[tableau.length - 1][tableau[0].length - 1],
            solution: solution,
            steps: steps
        };
    }

    initializeTableau(objective, constraints, rhs, isMaximize) {
        const numVars = objective.length;
        const numConstraints = constraints.length;
        const tableau = [];

        // Create tableau with slack variables
        for (let i = 0; i < numConstraints; i++) {
            const row = [...constraints[i], 0, rhs[i]]; // Add slack variable (0) and RHS
            tableau.push(row);
        }

        // Add objective row (negated for maximization)
        const objectiveRow = [...objective, 0, 0];
        if (isMaximize) {
            for (let i = 0; i < objectiveRow.length; i++) {
                objectiveRow[i] = -objectiveRow[i];
            }
        }
        tableau.push(objectiveRow);

        return tableau;
    }

    findEnteringVariable(tableau) {
        const lastRow = tableau[tableau.length - 1];
        let min = 0;
        let minIndex = -1;

        for (let i = 0; i < lastRow.length - 1; i++) {
            if (lastRow[i] < min) {
                min = lastRow[i];
                minIndex = i;
            }
        }

        return minIndex;
    }

    findLeavingVariable(tableau, enteringVar) {
        let minRatio = Infinity;
        let leavingVar = -1;

        for (let i = 0; i < tableau.length - 1; i++) {
            const coefficient = tableau[i][enteringVar];
            
            if (coefficient > 0) {
                const ratio = tableau[i][tableau[0].length - 1] / coefficient;
                
                if (ratio < minRatio) {
                    minRatio = ratio;
                    leavingVar = i;
                }
            }
        }

        return leavingVar;
    }

    pivot(tableau, enteringVar, leavingVar) {
        const newTableau = JSON.parse(JSON.stringify(tableau));
        const pivotElement = newTableau[leavingVar][enteringVar];

        // Normalize pivot row
        for (let i = 0; i < newTableau[0].length; i++) {
            newTableau[leavingVar][i] /= pivotElement;
        }

        // Eliminate other entries in entering variable column
        for (let i = 0; i < newTableau.length; i++) {
            if (i !== leavingVar) {
                const factor = newTableau[i][enteringVar];
                for (let j = 0; j < newTableau[0].length; j++) {
                    newTableau[i][j] -= factor * newTableau[leavingVar][j];
                }
            }
        }

        return newTableau;
    }

    extractSolution(tableau, numConstraints) {
        const solution = [];
        const numVars = tableau[0].length - numConstraints - 1;
        
        for (let i = 0; i < numVars; i++) {
            solution.push(0);
        }

        for (let i = 0; i < numConstraints; i++) {
            for (let j = 0; j < numVars; j++) {
                if (Math.abs(tableau[i][j] - 1) < 0.0001) {
                    solution[j] = tableau[i][tableau[0].length - 1];
                    break;
                }
            }
        }

        return solution;
    }

    // Format tableau for display
    formatTableau(tableau) {
        return tableau.map(row => 
            row.map(val => val.toFixed(3)).join(' ')
        ).join('\n');
    }

    // Handle button click
    handleRun() {
        this.handleRunSimplex();
    }
}
```

```html
<!-- simplex.html -->
<template>
    <div class="simplex-container">
        <h2>Dantzig's Simplex Algorithm</h2>
        
        <div class="input-section">
            <h3>Problem Setup</h3>
            <p>Maximize: 3x₁ + 2x₂</p>
            <p>Subject to:</p>
            <ul>
                <li>x₁ + 2x₂ ≤ 2</li>
                <li>2x₁ + x₂ ≤ 3</li>
                <li>x₁ ≤ 1</li>
            </ul>
            <lightning-button 
                label="Run Simplex Algorithm" 
                variant="brand" 
                onclick={handleRun}>
            </lightning-button>
        </div>

        <div class="result-section" if:true={result.steps.length > 0}>
            <h3>Results</h3>
            <p>Optimal Value: {result.optimalValue}</p>
            <p>Optimal Solution: [{result.optimalSolution.join(', ')}]</p>
            
            <h4>Simplex Steps</h4>
            <div class="steps">
                <template for:each={result.steps} for:item="step">
                    <div key={step.step} class="step">
                        <h5>Step {step.step}: {step.message}</h5>
                        <pre class="tableau">{formatTableau(step.tableau)}</pre>
                    </div>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* simplex.css */
.simplex-container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.result-section {
    background-color: #fff;
    padding: 15px;
    border-radius: 5px;
    border: 1px solid #ddd;
}

.steps {
    margin-top: 15px;
}

.step {
    margin-bottom: 20px;
    padding: 10px;
    background-color: #f9f9f9;
    border-radius: 3px;
}

.tableau {
    background-color: #000;
    color: #0f0;
    padding: 10px;
    border-radius: 3px;
    font-family: monospace;
    white-space: pre;
    overflow-x: auto;
}

.lightning-button {
    margin-top: 10px;
}
```

## How It Works

This implementation demonstrates Dantzig's Simplex algorithm with the following key components:

1. **Initialization**: Creates the initial simplex tableau with slack variables
2. **Entering Variable Selection**: Finds the most negative coefficient in the objective row
3. **Leaving Variable Selection**: Performs minimum ratio test to determine pivot row
4. **Pivot Operation**: Updates the tableau using elementary row operations
5. **Optimality Check**: Continues until no negative coefficients remain

## Key Features

- **Step-by-step execution** showing each iteration
- **Visual tableau display** at each step
- **Optimal solution extraction** from final tableau
- **Error handling** for unbounded solutions
- **Flexible problem setup** for different LP problems

The algorithm solves the linear programming problem:
- Maximize: 3x₁ + 2x₂
- Subject to: x₁ + 2x₂ ≤ 2, 2x₁ + x₂ ≤ 3, x₁ ≤ 1

This implementation provides a complete, working example of the simplex method within a Lightning Web Component framework.

