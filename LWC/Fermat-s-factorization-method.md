# Fermat's Factorization Method in Lightning Web Component

Here's an example implementation of Fermat's factorization method in a Lightning Web Component:

```javascript
// fermatFactorization.js
import { LightningElement } from 'lwc';

export default class FermatFactorization extends LightningElement {
    number = '';
    factors = '';
    isCalculating = false;

    handleInputChange(event) {
        this.number = event.target.value;
        this.factors = '';
    }

    handleFactorize() {
        if (!this.number || isNaN(this.number) || this.number <= 1) {
            this.factors = 'Please enter a valid number greater than 1';
            return;
        }

        this.isCalculating = true;
        this.factors = 'Calculating...';

        // Simulate async calculation
        setTimeout(() => {
            const result = this.fermatFactorization(parseInt(this.number));
            this.factors = result;
            this.isCalculating = false;
        }, 100);
    }

    fermatFactorization(n) {
        // Check if number is even
        if (n % 2 === 0) {
            return `2 and ${n / 2}`;
        }

        // Find the smallest integer greater than or equal to sqrt(n)
        let a = Math.ceil(Math.sqrt(n));
        let b2 = a * a - n;

        // Continue until b2 is a perfect square
        while (!this.isPerfectSquare(b2)) {
            a++;
            b2 = a * a - n;
        }

        const b = Math.sqrt(b2);
        const factor1 = a - b;
        const factor2 = a + b;

        return `${factor1} and ${factor2}`;
    }

    isPerfectSquare(num) {
        if (num < 0) return false;
        const sqrt = Math.sqrt(num);
        return sqrt === Math.floor(sqrt);
    }

    handleReset() {
        this.number = '';
        this.factors = '';
    }
}
```

```html
<!-- fermatFactorization.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Fermat's Factorization Method</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="numberInput">
                Enter a number to factorize
            </label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    id="numberInput"
                    class="slds-input"
                    value={number}
                    onchange={handleInputChange}
                    min="2"
                    placeholder="Enter a number greater than 1"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <div class="slds-form-element__control">
                <lightning-button 
                    label="Factorize" 
                    variant="brand"
                    onclick={handleFactorize}
                    disabled={isCalculating}
                    class="slds-m-top_small"
                >
                </lightning-button>
                <lightning-button 
                    label="Reset" 
                    variant="neutral"
                    onclick={handleReset}
                    class="slds-m-top_small slds-m-left_small"
                >
                </lightning-button>
            </div>
        </div>

        <div class="slds-form-element slds-m-top_small">
            <div class="slds-form-element__label">Result</div>
            <div class="slds-form-element__control">
                <p class="slds-text-body_regular">
                    <template if:true={factors}>
                        <span class="slds-text-title">Factors of {number}: {factors}</span>
                    </template>
                    <template if:false={factors}>
                        <span class="slds-text-title">Enter a number and click Factorize</span>
                    </template>
                </p>
            </div>
        </div>

        <div class="slds-m-top_small">
            <p class="slds-text-body_small">
                <strong>How Fermat's Method Works:</strong>
                <ul class="slds-list_dotted">
                    <li>For odd number n, find smallest a such that a² - n is a perfect square</li>
                    <li>Then n = (a - b)(a + b) where b = √(a² - n)</li>
                    <li>Special case: if n is even, factor out 2 first</li>
                </ul>
            </p>
        </div>
    </div>
</template>
```

```css
/* fermatFactorization.css */
.slds-box {
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 300px;
}

.slds-text-title {
    font-weight: bold;
}
```

## How the Algorithm Works

1. **Input Validation**: Checks if the input is a valid number greater than 1
2. **Even Number Check**: If the number is even, immediately returns 2 and n/2
3. **Finding Square**: 
   - Starts with a = ceil(√n)
   - Calculates b² = a² - n
   - Continues incrementing a until b² is a perfect square
4. **Factor Calculation**: 
   - Once perfect square found: b = √b²
   - Factors are (a - b) and (a + b)

## Example Usage

- Input: `15` → Output: `3 and 5` (since 15 = 3 × 5)
- Input: `21` → Output: `3 and 7` (since 21 = 3 × 7)
- Input: `35` → Output: `5 and 7` (since 35 = 5 × 7)

The algorithm is most efficient for numbers that are close to perfect squares, making it particularly useful for factoring numbers with factors that are relatively close to each other.

