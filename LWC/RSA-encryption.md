# RSA Encryption in Lightning Web Component

Here's an example of implementing RSA encryption in a Lightning Web Component using the Web Crypto API:

```javascript
import { LightningElement } from 'lwc';

export default class RSAEncryption extends LightningElement {
    message = '';
    encryptedMessage = '';
    decryptedMessage = '';
    publicKey = '';
    privateKey = '';

    // Generate RSA key pair
    async generateKeyPair() {
        try {
            const keyPair = await crypto.subtle.generateKey(
                {
                    name: "RSA-OAEP",
                    modulusLength: 2048,
                    publicExponent: new Uint8Array([1, 0, 1]),
                    hash: "SHA-256",
                },
                true,
                ["encrypt", "decrypt"]
            );

            // Export public key
            const exportedPublicKey = await crypto.subtle.exportKey(
                "jwk",
                keyPair.publicKey
            );

            // Export private key
            const exportedPrivateKey = await crypto.subtle.exportKey(
                "jwk",
                keyPair.privateKey
            );

            this.publicKey = JSON.stringify(exportedPublicKey);
            this.privateKey = JSON.stringify(exportedPrivateKey);
            
            console.log('Key pair generated successfully');
        } catch (error) {
            console.error('Error generating key pair:', error);
        }
    }

    // Encrypt message using RSA
    async encryptMessage() {
        if (!this.message || !this.publicKey) {
            alert('Please enter a message and generate keys first');
            return;
        }

        try {
            // Parse the public key
            const publicKeyObj = JSON.parse(this.publicKey);
            
            // Import public key
            const importedPublicKey = await crypto.subtle.importKey(
                "jwk",
                publicKeyObj,
                {
                    name: "RSA-OAEP",
                    hash: "SHA-256",
                },
                false,
                ["encrypt"]
            );

            // Convert message to ArrayBuffer
            const encoder = new TextEncoder();
            const data = encoder.encode(this.message);

            // Encrypt the data
            const encrypted = await crypto.subtle.encrypt(
                {
                    name: "RSA-OAEP",
                    hash: "SHA-256",
                },
                importedPublicKey,
                data
            );

            // Convert to base64 for display
            const encryptedArray = new Uint8Array(encrypted);
            this.encryptedMessage = btoa(String.fromCharCode(...encryptedArray));
            
            console.log('Message encrypted successfully');
        } catch (error) {
            console.error('Error encrypting message:', error);
        }
    }

    // Decrypt message using RSA
    async decryptMessage() {
        if (!this.encryptedMessage || !this.privateKey) {
            alert('Please enter encrypted message and generate keys first');
            return;
        }

        try {
            // Parse the private key
            const privateKeyObj = JSON.parse(this.privateKey);
            
            // Import private key
            const importedPrivateKey = await crypto.subtle.importKey(
                "jwk",
                privateKeyObj,
                {
                    name: "RSA-OAEP",
                    hash: "SHA-256",
                },
                false,
                ["decrypt"]
            );

            // Convert base64 to ArrayBuffer
            const binaryString = atob(this.encryptedMessage);
            const bytes = new Uint8Array(binaryString.length);
            for (let i = 0; i < binaryString.length; i++) {
                bytes[i] = binaryString.charCodeAt(i);
            }

            // Decrypt the data
            const decrypted = await crypto.subtle.decrypt(
                {
                    name: "RSA-OAEP",
                    hash: "SHA-256",
                },
                importedPrivateKey,
                bytes
            );

            // Convert back to string
            const decoder = new TextDecoder();
            this.decryptedMessage = decoder.decode(decrypted);
            
            console.log('Message decrypted successfully');
        } catch (error) {
            console.error('Error decrypting message:', error);
        }
    }

    handleInputChange(event) {
        const { name, value } = event.target;
        this[name] = value;
    }

    handleGenerateKeys() {
        this.generateKeyPair();
    }

    handleEncrypt() {
        this.encryptMessage();
    }

    handleDecrypt() {
        this.decryptMessage();
    }
}
```

```html
<!-- rsa-encryption.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>RSA Encryption Demo</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Message to Encrypt</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    name="message"
                    value={message}
                    onchange={handleInputChange}
                    placeholder="Enter message to encrypt"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Public Key</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea"
                    name="publicKey"
                    value={publicKey}
                    readonly
                    placeholder="Public key will appear here"
                ></textarea>
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Private Key</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea"
                    name="privateKey"
                    value={privateKey}
                    readonly
                    placeholder="Private key will appear here"
                ></textarea>
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Encrypted Message</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea"
                    name="encryptedMessage"
                    value={encryptedMessage}
                    readonly
                    placeholder="Encrypted message will appear here"
                ></textarea>
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Decrypted Message</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea"
                    name="decryptedMessage"
                    value={decryptedMessage}
                    readonly
                    placeholder="Decrypted message will appear here"
                ></textarea>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Generate Key Pair" 
                variant="brand" 
                onclick={handleGenerateKeys}
                class="slds-m-right_small"
            ></lightning-button>
            
            <lightning-button 
                label="Encrypt Message" 
                variant="success" 
                onclick={handleEncrypt}
                class="slds-m-right_small"
            ></lightning-button>
            
            <lightning-button 
                label="Decrypt Message" 
                variant="warning" 
                onclick={handleDecrypt}
            ></lightning-button>
        </div>
    </div>
</template>
```

```css
/* rsa-encryption.css */
.slds-box {
    padding: 1rem;
    margin: 1rem 0;
}

.slds-textarea {
    min-height: 100px;
}
```

## Key Features of this Implementation:

1. **Key Generation**: Creates RSA key pairs using Web Crypto API
2. **Encryption**: Encrypts messages using RSA-OAEP padding
3. **Decryption**: Decrypts messages using the private key
4. **Base64 Encoding**: Converts binary encrypted data to readable format
5. **Error Handling**: Includes proper error handling for all operations

## Important Security Notes:

- This implementation is for demonstration purposes only
- In production, keys should be stored securely and never exposed to clients
- Consider using proper key management systems
- The Web Crypto API is only available in secure contexts (HTTPS)
- For production applications, consider using dedicated encryption libraries

## Browser Compatibility:

This implementation uses the Web Crypto API which is supported in:
- Chrome 37+
- Firefox 34+
- Safari 10+
- Edge 12+

The LWC component provides a user-friendly interface to demonstrate RSA encryption concepts while leveraging the browser's native cryptographic capabilities.

