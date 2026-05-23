# Egyptian Multiplication in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class EgyptianMultiplication extends LightningElement {
    multiplicand = '';
    multiplier = '';
    result = null;
    steps = [];

    handleCalculate() {
        const a = parseInt(this.multiplicand);
        const b = parseInt(this.multiplier);
        
        if (isNaN(a) || isNaN(b)) {
            this.result = 'Please enter valid numbers';
            return;
        }
        
        this.result = this.egyptianMultiply(a, b);
    }

    egyptianMultiply(a, b) {
        // Reset steps
        this.steps = [];
        
        // Handle negative numbers
        const isNegative = (a < 0) !== (b < 0);
        a = Math.abs(a);
        b = Math.abs(b);
        
        let result = 0;
        let stepCount = 0;
        
        // Store initial values
        this.steps.push({
            step: ++stepCount,
            left: a,
            right: b,
            isDouble: false,
            isAdd: false,
            explanation: `Initial values: ${a} × ${b}`
        });
        
        // Main algorithm
        while (b > 0) {
            if (b % 2 === 1) {
                result += a;
                this.steps.push({
                    step: ++stepCount,
                    left: a,
                    right: b,
                    isDouble: false,
                    isAdd: true,
                    explanation: `Add ${a} to result (since ${b} is odd)`
                });
            } else {
                this.steps.push({
                    step: ++stepCount,
                    left: a,
                    right: b,
                    isDouble: false,
                    isAdd: false,
                    explanation: `Skip ${a} (since ${b} is even)`
                });
            }
            
            a *= 2;
            b = Math.floor(b / 2);
            
            this.steps.push({
                step: ++stepCount,
                left: a,
                right: b,
                isDouble: true,
                isAdd: false,
                explanation: `Double left value: ${a/2} → ${a}, halve right value: ${b*2} → ${b}`
            });
        }
        
        return isNegative ? -result : result;
    }

    handleReset() {
        this.multiplicand = '';
        this.multiplier = '';
        this.result = null;
        this.steps = [];
    }

    get formattedSteps() {
        return this.steps.map((step, index) => ({
            ...step,
            index: index + 1
        }));
    }
}
```

```html
<template>
    <div class="container">
        <h2>Egyptian Multiplication</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Multiplicand" 
                value={multiplicand}
                onchange={handleMultiplicandChange}
                type="number">
            </lightning-input>
            
            <lightning-input 
                label="Multiplier" 
                value={multiplier}
                onchange={handleMultiplierChange}
                type="number">
            </lightning-input>
            
            <lightning-button 
                label="Calculate" 
                onclick={handleCalculate}
                variant="brand">
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                onclick={handleReset}
                variant="neutral">
            </lightning-button>
        </div>

        <template if:true={result !== null}>
            <div class="result-section">
                <p><strong>Result:</strong> {result}</p>
            </div>
        </template>

        <template if:true={steps.length > 0}>
            <div class="steps-section">
                <h3>Calculation Steps:</h3>
                <lightning-datatable
                    data={formattedSteps}
                    columns={columns}
                    key-field="index"
                    hide-checkbox-column="true">
                </lightning-datatable>
            </div>
        </template>
    </div>
</template>
```

```css
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    display: flex;
    flex-direction: column;
    gap: 15px;
    margin-bottom: 20px;
}

.result-section {
    background-color: #f0f8ff;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.steps-section {
    margin-top: 20px;
}

.steps-section lightning-datatable {
    margin-top: 10px;
}

.lightning-button {
    margin-right: 10px;
}
```

## How the Algorithm Works

The Egyptian multiplication algorithm (also known as binary multiplication or Russian peasant multiplication) works by:

1. **Halving and Doubling**: Keep doubling one number while halving the other
2. **Selection**: When the halved number is odd, add the corresponding doubled value to the result
3. **Repeat**: Continue until the halved number becomes 0

## Example: 13 × 9

```
13 × 9
13 × 9 → 13 (odd) → add 13 to result
26 × 4 → 26 (even) → skip
52 × 2 → 52 (even) → skip
104 × 1 → 104 (odd) → add 104 to result
Result: 13 + 104 = 117
```

This implementation shows the step-by-step process of the Egyptian multiplication algorithm in a Lightning Web Component.

