# HMAC Algorithm in Lightning Web Component

Here's an example of implementing HMAC algorithm in a Lightning Web Component using JavaScript's built-in crypto APIs:

```javascript
import { LightningElement } from 'lwc';

export default class HmacExample extends LightningElement {
    secretKey = 'mySecretKey123';
    message = 'Hello, Lightning Web Components!';
    hmacResult = '';

    handleGenerateHmac() {
        this.generateHmac(this.message, this.secretKey)
            .then(result => {
                this.hmacResult = result;
            })
            .catch(error => {
                console.error('Error generating HMAC:', error);
            });
    }

    async generateHmac(message, key) {
        // Convert message and key to ArrayBuffer
        const encoder = new TextEncoder();
        const data = encoder.encode(message);
        const secret = encoder.encode(key);

        // Import the key for HMAC
        const importedKey = await crypto.subtle.importKey(
            'raw',
            secret,
            { name: 'HMAC', hash: 'SHA-256' },
            false,
            ['sign']
        );

        // Generate HMAC
        const signature = await crypto.subtle.sign(
            'HMAC',
            importedKey,
            data
        );

        // Convert to hexadecimal string
        return this.arrayBufferToHex(signature);
    }

    arrayBufferToHex(buffer) {
        return Array.from(new Uint8Array(buffer))
            .map(b => b.toString(16).padStart(2, '0'))
            .join('');
    }

    handleClear() {
        this.hmacResult = '';
        this.message = '';
    }
}
```

```html
<!-- hmac-example.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>HMAC Algorithm Example</h2>
        
        <lightning-input 
            label="Secret Key" 
            value={secretKey}
            onchange={handleSecretKeyChange}
            type="text">
        </lightning-input>

        <lightning-textarea 
            label="Message to HMAC"
            value={message}
            onchange={handleMessageChange}
            placeholder="Enter your message here...">
        </lightning-textarea>

        <lightning-button 
            label="Generate HMAC" 
            onclick={handleGenerateHmac}
            variant="brand">
        </lightning-button>

        <lightning-button 
            label="Clear" 
            onclick={handleClear}
            variant="neutral">
        </lightning-button>

        <div if:true={hmacResult}>
            <hr/>
            <h3>Generated HMAC:</h3>
            <p class="slds-text-body_small">{hmacResult}</p>
        </div>
    </div>
</template>
```

```javascript
// hmac-example.js
import { LightningElement } from 'lwc';

export default class HmacExample extends LightningElement {
    secretKey = 'mySecretKey123';
    message = 'Hello, Lightning Web Components!';
    hmacResult = '';

    handleGenerateHmac() {
        this.generateHmac(this.message, this.secretKey)
            .then(result => {
                this.hmacResult = result;
            })
            .catch(error => {
                console.error('Error generating HMAC:', error);
            });
    }

    async generateHmac(message, key) {
        try {
            const encoder = new TextEncoder();
            const data = encoder.encode(message);
            const secret = encoder.encode(key);

            // Import the key for HMAC
            const importedKey = await crypto.subtle.importKey(
                'raw',
                secret,
                { name: 'HMAC', hash: 'SHA-256' },
                false,
                ['sign']
            );

            // Generate HMAC
            const signature = await crypto.subtle.sign(
                'HMAC',
                importedKey,
                data
            );

            // Convert to hexadecimal string
            return this.arrayBufferToHex(signature);
        } catch (error) {
            throw new Error(`HMAC generation failed: ${error.message}`);
        }
    }

    arrayBufferToHex(buffer) {
        return Array.from(new Uint8Array(buffer))
            .map(b => b.toString(16).padStart(2, '0'))
            .join('');
    }

    handleSecretKeyChange(event) {
        this.secretKey = event.target.value;
    }

    handleMessageChange(event) {
        this.message = event.target.value;
    }

    handleClear() {
        this.hmacResult = '';
        this.message = '';
    }
}
```

## Key Features of This Implementation:

1. **Asynchronous HMAC Generation**: Uses `crypto.subtle.sign()` for secure HMAC computation
2. **UTF-8 Encoding**: Properly encodes text to ArrayBuffer using TextEncoder
3. **Error Handling**: Includes try-catch blocks for robust error management
4. **Hexadecimal Output**: Converts binary signature to readable hex string
5. **User Interface**: Interactive input fields and clear display of results

## Security Considerations:

- Uses browser's built-in Web Crypto API for secure cryptographic operations
- Key material is properly encoded before import
- Error handling prevents information leakage
- Follows best practices for HMAC implementation

## Usage Notes:

- This requires the component to be served over HTTPS in production
- The `crypto` API is available in modern browsers (Chrome 37+, Firefox 34+, Edge 12+)
- For sensitive applications, consider additional security measures
- The example uses SHA-256 hash algorithm for HMAC

This implementation demonstrates a complete HMAC workflow within the Lightning Web Component framework while maintaining security best practices.

