# Pollard's Rho Algorithm in Lightning Web Component

Here's an example implementation of Pollard's Rho algorithm for integer factorization in Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class PollardRhoFactorization extends LightningElement {
    factors = [];
    numberToFactor = '';
    isCalculating = false;
    errorMessage = '';

    handleInputChange(event) {
        this.numberToFactor = event.target.value;
        this.errorMessage = '';
    }

    async handleFactorize() {
        if (!this.numberToFactor || this.numberToFactor <= 1) {
            this.errorMessage = 'Please enter a number greater than 1';
            return;
        }

        this.isCalculating = true;
        this.factors = [];
        this.errorMessage = '';

        try {
            const number = parseInt(this.numberToFactor);
            const factors = await this.pollardRhoFactorize(number);
            this.factors = factors;
        } catch (error) {
            this.errorMessage = error.message;
        } finally {
            this.isCalculating = false;
        }
    }

    pollardRhoFactorize(n) {
        return new Promise((resolve, reject) => {
            // Handle edge cases
            if (n <= 1) {
                reject(new Error('Number must be greater than 1'));
                return;
            }

            if (this.isPrime(n)) {
                resolve([n]);
                return;
            }

            // Start factorization process
            const factors = [];
            this.pollardRho(n, factors);
            
            // Sort factors
            factors.sort((a, b) => a - b);
            resolve(factors);
        });
    }

    pollardRho(n, factors) {
        if (n <= 1) return;

        if (this.isPrime(n)) {
            factors.push(n);
            return;
        }

        // Use Pollard's Rho algorithm to find a factor
        const factor = this.pollardRhoHelper(n);
        
        if (factor === n) {
            // If factor equals n, it's likely prime
            factors.push(n);
            return;
        }

        // Recursively factor both the found factor and the quotient
        this.pollardRho(factor, factors);
        this.pollardRho(n / factor, factors);
    }

    pollardRhoHelper(n) {
        if (n % 2 === 0) return 2;

        let x = 2;
        let y = 2;
        let d = 1;

        // Function f(x) = (x^2 + 1) mod n
        const f = (x) => (x * x + 1) % n;

        while (d === 1) {
            x = f(x);
            y = f(f(y));
            d = this.gcd(Math.abs(x - y), n);
        }

        return d;
    }

    gcd(a, b) {
        while (b !== 0) {
            let temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    isPrime(n) {
        if (n <= 1) return false;
        if (n <= 3) return true;
        if (n % 2 === 0 || n % 3 === 0) return false;

        for (let i = 5; i * i <= n; i += 6) {
            if (n % i === 0 || n % (i + 2) === 0) {
                return false;
            }
        }
        return true;
    }

    get formattedFactors() {
        if (this.factors.length === 0) return '';
        return this.factors.join(' × ');
    }
}
```

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Pollard's Rho Factorization</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="numberInput">
                Number to Factorize
            </label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    id="numberInput"
                    class="slds-input"
                    value={numberToFactor}
                    onchange={handleInputChange}
                    min="2"
                />
            </div>
        </div>

        <lightning-button 
            label="Factorize" 
            variant="brand" 
            onclick={handleFactorize}
            disabled={isCalculating}
            class="slds-m-top_small">
        </lightning-button>

        <template if:true={isCalculating}>
            <div class="slds-m-top_small">
                <lightning-spinner alternative-text="Calculating factors"></lightning-spinner>
                <p>Factoring in progress...</p>
            </div>
        </template>

        <template if:true={errorMessage}>
            <div class="slds-notify slds-notify_alert slds-theme_error slds-m-top_small">
                <span class="slds-assistive-text">Error</span>
                {errorMessage}
            </div>
        </template>

        <template if:true={factors.length > 0}>
            <div class="slds-m-top_small">
                <h3>Factors:</h3>
                <p class="slds-text-body_regular">
                    <strong>{numberToFactor}</strong> = {formattedFactors}
                </p>
                <ul class="slds-list_dotted slds-m-top_small">
                    <template for:each={factors} for:item="factor">
                        <li key={factor}>{factor}</li>
                    </template>
                </ul>
            </div>
        </template>
    </div>
</template>
```

```css
.slds-box {
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 300px;
}
```

## How it works:

1. **Input Handling**: Users enter a number to factorize
2. **Pollard's Rho Algorithm**: 
   - Uses the function f(x) = (x² + 1) mod n
   - Implements Floyd's cycle detection algorithm
   - Finds a non-trivial factor using the greatest common divisor
3. **Recursive Factorization**: Continues factoring until all prime factors are found
4. **Display**: Shows the complete factorization result

## Key Features:

- **Error Handling**: Validates input and handles edge cases
- **Progress Indicator**: Shows spinner during calculation
- **Responsive Design**: Uses Lightning Design System components
- **Recursive Factorization**: Breaks down composite numbers into prime factors
- **Real-time Display**: Shows both the factorization equation and individual factors

## Example Usage:

Input: `152415787501905210000000000000000000000000000000

