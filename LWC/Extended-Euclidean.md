# Extended Euclidean Algorithm in Lightning Web Component

Here's an example implementation of the Extended Euclidean Algorithm in a Lightning Web Component:

```javascript
// extendedEuclidean.js
import { LightningElement } from 'lwc';

export default class ExtendedEuclidean extends LightningElement {
    gcd = 0;
    x = 0;
    y = 0;
    a = 0;
    b = 0;

    handleCalculate() {
        const a = parseInt(this.a);
        const b = parseInt(this.b);
        
        if (isNaN(a) || isNaN(b)) {
            this.showToast('Error', 'Please enter valid numbers', 'error');
            return;
        }

        const result = this.extendedEuclidean(a, b);
        this.gcd = result.gcd;
        this.x = result.x;
        this.y = result.y;
    }

    extendedEuclidean(a, b) {
        // Base case
        if (b === 0) {
            return {
                gcd: a,
                x: 1,
                y: 0
            };
        }

        // Recursive call
        const result = this.extendedEuclidean(b, a % b);
        const gcd = result.gcd;
        const x = result.y;
        const y = result.x - Math.floor(a / b) * result.y;

        return {
            gcd: gcd,
            x: x,
            y: y
        };
    }

    handleInputChange(event) {
        const fieldName = event.target.name;
        this[fieldName] = event.target.value;
    }

    showToast(title, message, variant) {
        const event = new ShowToastEvent({
            title: title,
            message: message,
            variant: variant
        });
        this.dispatchEvent(event);
    }
}
```

```html
<!-- extendedEuclidean.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Extended Euclidean Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Number a</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    name="a"
                    value={a}
                    onchange={handleInputChange}
                    class="slds-input"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Number b</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    name="b"
                    value={b}
                    onchange={handleInputChange}
                    class="slds-input"
                />
            </div>
        </div>

        <button 
            onclick={handleCalculate}
            class="slds-button slds-button_brand"
        >
            Calculate GCD and Coefficients
        </button>

        <template if:true={gcd}>
            <div class="slds-box slds-theme_info slds-m-top_medium">
                <h3>Results</h3>
                <p><strong>GCD:</strong> {gcd}</p>
                <p><strong>x coefficient:</strong> {x}</p>
                <p><strong>y coefficient:</strong> {y}</p>
                <p><strong>Verification:</strong> {a} × {x} + {b} × {y} = {verifyResult}</p>
            </div>
        </template>
    </div>
</template>
```

```css
/* extendedEuclidean.css */
.slds-box {
    margin: 1rem;
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 300px;
}

.slds-button {
    margin-top: 1rem;
}
```

## How it works:

1. **Extended Euclidean Algorithm**: This implementation finds the greatest common divisor (GCD) of two numbers along with the coefficients (x and y) such that ax + by = gcd(a,b)

2. **Recursive Approach**: The algorithm recursively calls itself with parameters (b, a % b) until b becomes 0

3. **Base Case**: When b = 0, the GCD is a, and x = 1, y = 0

4. **Back Substitution**: The coefficients are calculated using the recursive relationship:
   - x = y₁
   - y = x₁ - floor(a/b) × y₁

5. **Verification**: The result can be verified by checking that ax + by = gcd(a,b)

## Example Usage:
- Input: a = 35, b = 15
- Output: GCD = 5, x = 1, y = -2
- Verification: 35 × 1 + 15 × (-2) = 5

This implementation demonstrates the core mathematical algorithm within the Lightning Web Component framework, showing how to handle user input, perform calculations, and display results in a Salesforce Lightning interface.

