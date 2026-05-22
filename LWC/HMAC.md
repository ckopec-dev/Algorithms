# HMAC Algorithm in Lightning Web Component

Here's an example of implementing HMAC algorithm in a Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class HmacExample extends LightningElement {
    secretKey = 'mySecretKey123';
    message = 'Hello World';
    hmacResult = '';

    handleCalculateHmac() {
        try {
            // Calculate HMAC using Web Crypto API
            this.hmacResult = this.calculateHmac(this.message, this.secretKey);
        } catch (error) {
            console.error('Error calculating HMAC:', error);
        }
    }

    calculateHmac(message, key) {
        // Convert message to ArrayBuffer
        const encoder = new TextEncoder();
        const data = encoder.encode(message);
        
        // Convert key to ArrayBuffer
        const keyData = encoder.encode(key);
        
        // Import the key
        return crypto.subtle.importKey(
            'raw',
            keyData,
            { name: 'HMAC', hash: 'SHA-256' },
            false,
            ['sign']
        ).then(importedKey => {
            // Sign the message
            return crypto.subtle.sign(
                'HMAC',
                importedKey,
                data
            );
        }).then(signature => {
            // Convert signature to hex string
            return this.arrayBufferToHex(signature);
        });
    }

    arrayBufferToHex(buffer) {
        return Array.from(new Uint8Array(buffer))
            .map(b => b.toString(16).padStart(2, '0'))
            .join('');
    }

    get formattedHmac() {
        return this.hmacResult ? this.hmacResult : 'Click "Calculate HMAC" to generate';
    }
}
```

```html
<template>
    <div class="slds-box slds-box_small">
        <h2>HMAC Calculator</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Message</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={message}
                    onchange={handleMessageChange}
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Secret Key</label>
            <div class="slds-form-element__control">
                <input 
                    type="password" 
                    class="slds-input"
                    value={secretKey}
                    onchange={handleKeyChange}
                />
            </div>
        </div>

        <lightning-button 
            label="Calculate HMAC" 
            onclick={handleCalculateHmac}
            class="slds-m-top_small"
        ></lightning-button>

        <div class="slds-form-element slds-m-top_small">
            <label class="slds-form-element__label">HMAC Result</label>
            <div class="slds-form-element__control">
                <lightning-input 
                    value={formattedHmac}
                    readonly
                    type="text"
                ></lightning-input>
            </div>
        </div>
    </div>
</template>
```

```css
.slds-box {
    padding: 1rem;
}

.slds-form-element {
    margin-bottom: 1rem;
}
```

## Key Features:

1. **Web Crypto API Usage**: Uses `crypto.subtle` for HMAC computation
2. **Asynchronous Processing**: Handles Promises for cryptographic operations
3. **Input Validation**: Includes proper text encoding and conversion
4. **Error Handling**: Catches and logs any cryptographic errors
5. **User Interface**: Clean input fields and result display

## Important Notes:

- This implementation requires the browser to support Web Crypto API
- The `crypto.subtle` API is only available in secure contexts (HTTPS)
- For production use, consider additional security measures
- The HMAC is calculated using SHA-256 hashing algorithm
- Secret keys should be handled securely and never exposed in client-side code

## Usage:

1. Enter a message to hash
2. Enter a secret key
3. Click "Calculate HMAC"
4. The result will be displayed as a hexadecimal string

This example demonstrates a secure way to implement HMAC in LWC using modern browser APIs while maintaining good security practices.

