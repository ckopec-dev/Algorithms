# Baby-step Giant-step Algorithm in Lightning Web Component

Here's an example implementation of the Baby-step Giant-step algorithm in a Lightning Web Component for solving discrete logarithm problems:

```javascript
// babyStepGiantStep.js
import { LightningElement } from 'lwc';

export default class BabyStepGiantStep extends LightningElement {
    base = '';
    modulus = '';
    target = '';
    result = '';
    error = '';

    handleCalculate() {
        try {
            const base = parseInt(this.base);
            const modulus = parseInt(this.modulus);
            const target = parseInt(this.target);

            if (isNaN(base) || isNaN(modulus) || isNaN(target)) {
                throw new Error('Please enter valid numbers');
            }

            if (base <= 0 || modulus <= 1 || target <= 0) {
                throw new Error('Base must be > 0, modulus > 1, target > 0');
            }

            const result = this.babyStepGiantStep(base, modulus, target);
            this.result = `x = ${result}`;
            this.error = '';
        } catch (error) {
            this.error = error.message;
            this.result = '';
        }
    }

    babyStepGiantStep(g, p, h) {
        // Calculate m = ceil(sqrt(p))
        const m = Math.ceil(Math.sqrt(p));
        
        // Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
        const babySteps = new Map();
        let power = 1;
        
        for (let j = 0; j < m; j++) {
            if (!babySteps.has(power)) {
                babySteps.set(power, j);
            }
            power = (power * g) % p;
        }
        
        // Giant steps: compute g^(-m) mod p
        const gInverse = this.modularInverse(g, p);
        const gNegM = this.modularPower(gInverse, m, p);
        
        // Search for solution
        let y = h;
        for (let i = 0; i < m; i++) {
            if (babySteps.has(y)) {
                const x = i * m + babySteps.get(y);
                return x;
            }
            y = (y * gNegM) % p;
        }
        
        throw new Error('No solution found');
    }

    modularInverse(a, m) {
        // Extended Euclidean Algorithm
        const [gcd, x, y] = this.extendedGCD(a, m);
        if (gcd !== 1) {
            throw new Error('Modular inverse does not exist');
        }
        return (x % m + m) % m;
    }

    extendedGCD(a, b) {
        if (b === 0) {
            return [a, 1, 0];
        }
        const [gcd, x1, y1] = this.extendedGCD(b, a % b);
        const x = y1;
        const y = x1 - Math.floor(a / b) * y1;
        return [gcd, x, y];
    }

    modularPower(base, exponent, modulus) {
        let result = 1;
        base = base % modulus;
        
        while (exponent > 0) {
            if (exponent % 2 === 1) {
                result = (result * base) % modulus;
            }
            exponent = Math.floor(exponent / 2);
            base = (base * base) % modulus;
        }
        
        return result;
    }

    handleBaseChange(event) {
        this.base = event.target.value;
    }

    handleModulusChange(event) {
        this.modulus = event.target.value;
    }

    handleTargetChange(event) {
        this.target = event.target.value;
    }
}
```

```html
<!-- babyStepGiantStep.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Baby-step Giant-step Algorithm</h2>
        <p>Solve discrete logarithm problem: g^x ≡ h (mod p)</p>
        
        <div class="slds-form slds-form_stacked">
            <div class="slds-form-element">
                <label class="slds-form-element__label" for="base">Base (g)</label>
                <div class="slds-form-element__control">
                    <input 
                        type="number" 
                        id="base" 
                        class="slds-input"
                        value={base}
                        onchange={handleBaseChange}
                        placeholder="Enter base value"
                    />
                </div>
            </div>

            <div class="slds-form-element">
                <label class="slds-form-element__label" for="modulus">Modulus (p)</label>
                <div class="slds-form-element__control">
                    <input 
                        type="number" 
                        id="modulus" 
                        class="slds-input"
                        value={modulus}
                        onchange={handleModulusChange}
                        placeholder="Enter modulus value"
                    />
                </div>
            </div>

            <div class="slds-form-element">
                <label class="slds-form-element__label" for="target">Target (h)</label>
                <div class="slds-form-element__control">
                    <input 
                        type="number" 
                        id="target" 
                        class="slds-input"
                        value={target}
                        onchange={handleTargetChange}
                        placeholder="Enter target value"
                    />
                </div>
            </div>

            <div class="slds-form-element">
                <button 
                    class="slds-button slds-button_brand"
                    onclick={handleCalculate}
                >
                    Calculate
                </button>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <template if:true={result}>
                <div class="slds-alert slds-alert_success" role="alert">
                    <span class="slds-assistive-text">Success</span>
                    <p>{result}</p>
                </div>
            </template>

            <template if:true={error}>
                <div class="slds-alert slds-alert_error" role="alert">
                    <span class="slds-assistive-text">Error</span>
                    <p>{error}</p>
                </div>
            </template>
        </div>
    </div>
</template>
```

```css
/* babyStepGiantStep.css */
.slds-box {
    padding: 1rem;
}

.slds-form-element {
    margin-bottom: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 300px;
}
```

## How the Algorithm Works

This implementation solves the discrete logarithm problem: **g^x ≡ h (mod p)**

### Key Components:

1. **Baby Steps**: Precompute g^j mod p for j = 0, 1, ..., m-1
2. **Giant Steps**: Compute g^(-m) mod p and search using g^(-m) * h
3. **Hash Map**: Store baby steps for fast lookup
4. **Modular Arithmetic**: Handle large numbers using modular inverse and power functions

### Example Usage:

For the equation 3^x ≡ 13 (mod 17):
- Base (g) = 3
- Modulus (p) = 17  
- Target (h) = 13
- Result: x = 4 (since 3^4 = 81 ≡ 13 (mod 17))

### Time Complexity:
- Time: O(√p)
- Space: O(√p)

This LWC component provides a user-friendly interface to input values and displays the result of the discrete logarithm calculation using the Baby-step Giant-step algorithm.

