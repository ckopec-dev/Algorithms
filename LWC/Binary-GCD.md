# Binary GCD Algorithm in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class BinaryGcd extends LightningElement {
    num1 = '';
    num2 = '';
    result = '';
    error = '';

    handleCalculate() {
        // Clear previous results
        this.result = '';
        this.error = '';

        // Validate inputs
        if (!this.num1 || !this.num2) {
            this.error = 'Please enter both numbers';
            return;
        }

        const a = parseInt(this.num1);
        const b = parseInt(this.num2);

        if (isNaN(a) || isNaN(b)) {
            this.error = 'Please enter valid numbers';
            return;
        }

        if (a < 0 || b < 0) {
            this.error = 'Please enter positive numbers';
            return;
        }

        // Calculate GCD using Binary GCD algorithm
        const gcd = this.binaryGCD(a, b);
        this.result = `GCD of ${a} and ${b} is: ${gcd}`;
    }

    /**
     * Binary GCD algorithm implementation
     * Also known as Stein's algorithm
     */
    binaryGCD(a, b) {
        // Base cases
        if (a === 0) return b;
        if (b === 0) return a;
        
        // If both numbers are even
        if ((a & 1) === 0 && (b & 1) === 0) {
            return 2 * this.binaryGCD(a >> 1, b >> 1);
        }
        
        // If a is even and b is odd
        if ((a & 1) === 0) {
            return this.binaryGCD(a >> 1, b);
        }
        
        // If a is odd and b is even
        if ((b & 1) === 0) {
            return this.binaryGCD(a, b >> 1);
        }
        
        // If both numbers are odd
        // Use the property: gcd(a,b) = gcd(|a-b|, min(a,b))
        if (a > b) {
            return this.binaryGCD(a - b, b);
        } else {
            return this.binaryGCD(a, b - a);
        }
    }

    handleInputChange(event) {
        const inputName = event.target.name;
        const value = event.target.value;
        
        if (inputName === 'num1') {
            this.num1 = value;
        } else if (inputName === 'num2') {
            this.num2 = value;
        }
    }

    // Example usage method
    handleExample() {
        this.num1 = '48';
        this.num2 = '18';
        this.handleCalculate();
    }
}
```

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Binary GCD Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">First Number</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    name="num1" 
                    value={num1}
                    onchange={handleInputChange}
                    class="slds-input"
                    placeholder="Enter first number"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Second Number</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    name="num2" 
                    value={num2}
                    onchange={handleInputChange}
                    class="slds-input"
                    placeholder="Enter second number"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <div class="slds-form-element__control">
                <button 
                    onclick={handleCalculate} 
                    class="slds-button slds-button_brand"
                >
                    Calculate GCD
                </button>
                <button 
                    onclick={handleExample} 
                    class="slds-button slds-button_outline-brand slds-m-left_x-small"
                >
                    Example
                </button>
            </div>
        </div>

        <div if:true={error} class="slds-alert slds-alert_error slds-m-top_small">
            <span class="slds-assistive-text">Error</span>
            <h2 class="slds-alert__title">{error}</h2>
        </div>

        <div if:true={result} class="slds-alert slds-alert_success slds-m-top_small">
            <span class="slds-assistive-text">Success</span>
            <h2 class="slds-alert__title">{result}</h2>
        </div>
    </div>
</template>
```

## How the Binary GCD Algorithm Works:

1. **Base Cases**: If either number is 0, return the other number
2. **Both Even**: If both numbers are even, divide both by 2 and multiply result by 2
3. **One Even**: If one number is even, divide that number by 2
4. **Both Odd**: If both numbers are odd, subtract the smaller from the larger
5. **Recursive**: Continue until one number becomes 0

## Key Features:

- **Efficient**: Uses bit operations instead of division
- **No Division**: Avoids expensive division operations
- **Bit Manipulation**: Uses right shift (>> 1) for division by 2
- **Recursive**: Implements the algorithm recursively
- **Error Handling**: Validates input and handles edge cases

## Example Output:
- Input: 48, 18 → Output: GCD is 6
- Input: 100, 25 → Output: GCD is 25
- Input: 17, 13 → Output: GCD is 1

The Binary GCD algorithm is more efficient than the traditional Euclidean algorithm for large numbers because it uses bit operations which are faster than division operations.

