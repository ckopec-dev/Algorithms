# Fast Fourier Transform (FFT) in Lightning Web Component

Here's an example of implementing a Fast Fourier Transform algorithm in a Lightning Web Component:

```javascript
// fft.js
import { LightningElement } from 'lwc';

export default class FFTComponent extends LightningElement {
    inputSignal = [1, 2, 3, 4, 4, 3, 2, 1];
    fftResult = [];
    isCalculated = false;

    get formattedInput() {
        return this.inputSignal.join(', ');
    }

    get formattedOutput() {
        return this.fftResult.map(complex => 
            `${complex.real.toFixed(2)} + ${complex.imaginary.toFixed(2)}i`
        ).join(', ');
    }

    handleCalculate() {
        this.fftResult = this.fft(this.inputSignal);
        this.isCalculated = true;
    }

    // Cooley-Tukey FFT implementation
    fft(signal) {
        const n = signal.length;
        
        // Handle edge cases
        if (n <= 1) {
            return [{ real: signal[0], imaginary: 0 }];
        }

        // Ensure power of 2 length
        const powerOf2 = Math.pow(2, Math.ceil(Math.log2(n)));
        const paddedSignal = [...signal, ...Array(powerOf2 - n).fill(0)];

        // Bit-reversal permutation
        const bitReversed = this.bitReverse(paddedSignal);
        
        // FFT computation
        const result = this.fftRecursive(bitReversed);
        
        // Return only the original length results
        return result.slice(0, n);
    }

    fftRecursive(x) {
        const n = x.length;
        
        if (n === 1) {
            return [x[0]];
        }

        // Divide
        const even = [];
        const odd = [];
        for (let i = 0; i < n; i += 2) {
            even.push(x[i]);
            odd.push(x[i + 1]);
        }

        // Conquer
        const evenFFT = this.fftRecursive(even);
        const oddFFT = this.fftRecursive(odd);

        // Combine
        const result = [];
        for (let k = 0; k < n / 2; k++) {
            const angle = -2 * Math.PI * k / n;
            const w = {
                real: Math.cos(angle),
                imaginary: Math.sin(angle)
            };

            const t = this.complexMultiply(oddFFT[k], w);
            const u = evenFFT[k];

            result[k] = {
                real: u.real + t.real,
                imaginary: u.imaginary + t.imaginary
            };

            result[k + n / 2] = {
                real: u.real - t.real,
                imaginary: u.imaginary - t.imaginary
            };
        }

        return result;
    }

    bitReverse(signal) {
        const n = signal.length;
        const result = new Array(n);
        const bitLength = Math.log2(n);

        for (let i = 0; i < n; i++) {
            let reversed = 0;
            for (let j = 0; j < bitLength; j++) {
                if (i & (1 << j)) {
                    reversed |= (1 << (bitLength - 1 - j));
                }
            }
            result[reversed] = signal[i];
        }

        return result;
    }

    complexMultiply(a, b) {
        return {
            real: a.real * b.real - a.imaginary * b.imaginary,
            imaginary: a.real * b.imaginary + a.imaginary * b.real
        };
    }

    handleInputChange(event) {
        const values = event.target.value.split(',').map(val => parseFloat(val.trim()));
        this.inputSignal = values.filter(val => !isNaN(val));
    }
}
```

```html
<!-- fft.html -->
<template>
    <div class="container">
        <h2>Fast Fourier Transform (FFT)</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input Signal (comma separated)" 
                value={formattedInput}
                onchange={handleInputChange}
                type="text">
            </lightning-input>
        </div>

        <lightning-button 
            label="Calculate FFT" 
            onclick={handleCalculate}
            variant="brand">
        </lightning-button>

        <template if:true={isCalculated}>
            <div class="result-section">
                <h3>FFT Result:</h3>
                <p>{formattedOutput}</p>
            </div>
        </template>
    </div>
</template>
```

```css
/* fft.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.result-section h3 {
    margin-top: 0;
    color: #0070d2;
}

.result-section p {
    font-family: monospace;
    font-size: 14px;
    word-break: break-all;
}
```

## Key Features of this FFT Implementation:

1. **Cooley-Tukey Algorithm**: Uses the standard divide-and-conquer approach for FFT
2. **Bit-Reversal Permutation**: Handles the required reordering for efficient computation
3. **Complex Number Operations**: Properly handles complex arithmetic operations
4. **Input Validation**: Processes comma-separated input values
5. **Responsive UI**: Clean Lightning Web Component interface

## Usage:
1. Enter comma-separated values in the input field
2. Click "Calculate FFT" to compute the transform
3. View the complex results in the output section

The implementation handles power-of-2 length signals and provides proper complex number arithmetic for the FFT computation.

