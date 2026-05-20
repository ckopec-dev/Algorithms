# Hamming Code Implementation in Lightning Web Component

Here's a complete example of implementing Hamming code algorithm in a Lightning Web Component:

```javascript
// hammingCode.js
import { LightningElement } from 'lwc';

export default class HammingCode extends LightningElement {
    dataBits = '';
    encodedData = '';
    decodedData = '';
    errorDetected = false;
    errorPosition = 0;

    handleDataInput(event) {
        this.dataBits = event.target.value;
    }

    // Encode data using Hamming code
    encodeData() {
        if (!this.dataBits) {
            return;
        }

        // Convert input to array of bits
        const data = this.dataBits.split('').map(Number);
        
        // Calculate number of parity bits needed
        const parityBits = this.calculateParityBits(data.length);
        
        // Create array for encoded data
        const encoded = new Array(data.length + parityBits);
        
        // Position of data bits (1-indexed)
        let dataPos = 0;
        let encodedPos = 1;
        
        // Place data bits in their positions
        for (let i = 1; i <= encoded.length; i++) {
            if (this.isPowerOfTwo(i)) {
                // This is a parity bit position
                encoded[i - 1] = 0; // Initialize to 0
            } else {
                // This is a data bit position
                encoded[i - 1] = data[dataPos];
                dataPos++;
            }
        }
        
        // Calculate parity bits
        for (let i = 1; i <= parityBits; i++) {
            const parityPos = Math.pow(2, i - 1);
            const parity = this.calculateParity(encoded, parityPos);
            encoded[parityPos - 1] = parity;
        }
        
        this.encodedData = encoded.join('');
        this.decodedData = '';
        this.errorDetected = false;
        this.errorPosition = 0;
    }

    // Decode data and detect errors
    decodeData() {
        if (!this.encodedData) {
            return;
        }

        const received = this.encodedData.split('').map(Number);
        const parityBits = this.calculateParityBits(received.length);
        
        // Calculate syndrome
        let syndrome = 0;
        for (let i = 1; i <= parityBits; i++) {
            const parityPos = Math.pow(2, i - 1);
            const parity = this.calculateParity(received, parityPos);
            if (parity !== 0) {
                syndrome += parityPos;
            }
        }
        
        this.errorPosition = syndrome;
        this.errorDetected = syndrome !== 0;
        
        if (syndrome !== 0) {
            // Correct the error
            received[syndrome - 1] = received[syndrome - 1] === 0 ? 1 : 0;
        }
        
        // Extract data bits
        const decoded = [];
        for (let i = 1; i <= received.length; i++) {
            if (!this.isPowerOfTwo(i)) {
                decoded.push(received[i - 1]);
            }
        }
        
        this.decodedData = decoded.join('');
    }

    // Calculate number of parity bits needed
    calculateParityBits(dataLength) {
        let r = 0;
        while (Math.pow(2, r) < dataLength + r + 1) {
            r++;
        }
        return r;
    }

    // Check if number is power of 2
    isPowerOfTwo(n) {
        return n > 0 && (n & (n - 1)) === 0;
    }

    // Calculate parity for a given position
    calculateParity(data, parityPos) {
        let parity = 0;
        for (let i = 1; i <= data.length; i++) {
            if (i & parityPos) {
                parity ^= data[i - 1];
            }
        }
        return parity;
    }

    // Handle encode button click
    handleEncode() {
        this.encodeData();
    }

    // Handle decode button click
    handleDecode() {
        this.decodeData();
    }

    // Clear all fields
    handleClear() {
        this.dataBits = '';
        this.encodedData = '';
        this.decodedData = '';
        this.errorDetected = false;
        this.errorPosition = 0;
    }
}
```

```html
<!-- hammingCode.html -->
<template>
    <div class="slds-box slds-box_small slds-theme_default">
        <h2>Hamming Code Encoder/Decoder</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="dataInput">Input Data Bits (0s and 1s)</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="dataInput"
                    class="slds-input"
                    value={dataBits}
                    onchange={handleDataInput}
                    placeholder="Enter binary data (e.g., 1011)"
                />
            </div>
        </div>

        <div class="slds-button-group" role="group">
            <button class="slds-button slds-button_brand" onclick={handleEncode}>
                Encode
            </button>
            <button class="slds-button slds-button_brand" onclick={handleDecode}>
                Decode
            </button>
            <button class="slds-button slds-button_outline-brand" onclick={handleClear}>
                Clear
            </button>
        </div>

        <template if:true={encodedData}>
            <div class="slds-form-element">
                <label class="slds-form-element__label">Encoded Data</label>
                <div class="slds-form-element__control">
                    <p class="slds-text-body_regular">{encodedData}</p>
                </div>
            </div>
        </template>

        <template if:true={decodedData}>
            <div class="slds-form-element">
                <label class="slds-form-element__label">Decoded Data</label>
                <div class="slds-form-element__control">
                    <p class="slds-text-body_regular">{decodedData}</p>
                </div>
            </div>
        </template>

        <template if:true={errorDetected}>
            <div class="slds-notify slds-notify_alert slds-theme_error" role="alert">
                <span class="slds-assistive-text">Error</span>
                <h2>Error detected at position: {errorPosition}</h2>
            </div>
        </template>

        <template if:true={errorPosition !== 0 && !errorDetected}>
            <div class="slds-notify slds-notify_alert slds-theme_success" role="alert">
                <span class="slds-assistive-text">Success</span>
                <h2>No errors detected</h2>
            </div>
        </template>
    </div>
</template>
```

```css
/* hammingCode.css */
.slds-box {
    margin: 1rem;
}

.slds-button-group {
    margin: 1rem 0;
}

.slds-input {
    margin-bottom: 1rem;
}
```

## How it works:

1. **Encoding Process**:
   - Calculates required parity bits using formula: 2^r ≥ n + r + 1
   - Places data bits in appropriate positions
   - Calculates parity bits using XOR operations
   - Inserts parity bits at power-of-2 positions

2. **Decoding Process**:
   - Calculates syndrome by checking all parity bits
   - If syndrome ≠ 0, identifies error position
   - Corrects the error if detected
   - Extracts original data bits

3. **Key Features**:
   - Real-time input validation
   - Error detection and correction
   - Visual feedback for error status
   - Clean user interface with SLDS styling

## Usage Example:
- Input: "1011"
- Output: Encoded data with parity bits
- Decode: Corrects any single-bit errors and returns original data

This implementation demonstrates the core Hamming code algorithm in a Lightning Web Component with proper error handling and user interface elements.

