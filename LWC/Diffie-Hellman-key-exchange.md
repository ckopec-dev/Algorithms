# Diffie-Hellman Key Exchange in Lightning Web Component

```javascript
// dh-key-exchange.js
import { LightningElement } from 'lwc';

export default class DhKeyExchange extends LightningElement {
    // Public properties
    publicKeyA = '';
    publicKeyB = '';
    sharedSecret = '';
    prime = 23;
    base = 5;
    
    // Private properties
    privateKeyA = 0;
    privateKeyB = 0;
    computedPublicKeyA = 0;
    computedPublicKeyB = 0;

    connectedCallback() {
        this.generateKeys();
    }

    generateKeys() {
        // Generate random private keys (in production, use proper random generation)
        this.privateKeyA = Math.floor(Math.random() * 10) + 1;
        this.privateKeyB = Math.floor(Math.random() * 10) + 1;
        
        // Calculate public keys
        this.computedPublicKeyA = this.modularExponentiation(this.base, this.privateKeyA, this.prime);
        this.computedPublicKeyB = this.modularExponentiation(this.base, this.privateKeyB, this.prime);
        
        // Calculate shared secrets
        const sharedSecretA = this.modularExponentiation(this.computedPublicKeyB, this.privateKeyA, this.prime);
        const sharedSecretB = this.modularExponentiation(this.computedPublicKeyA, this.privateKeyB, this.prime);
        
        // Both should be equal
        this.sharedSecret = sharedSecretA;
        
        // Update UI
        this.publicKeyA = this.computedPublicKeyA;
        this.publicKeyB = this.computedPublicKeyB;
    }

    // Modular exponentiation function (a^b mod m)
    modularExponentiation(base, exponent, modulus) {
        if (modulus === 1) return 0;
        
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

    handleGenerateKeys() {
        this.generateKeys();
    }

    get keyExchangeInfo() {
        return {
            prime: this.prime,
            base: this.base,
            privateKeyA: this.privateKeyA,
            privateKeyB: this.privateKeyB,
            publicKeyA: this.publicKeyA,
            publicKeyB: this.publicKeyB,
            sharedSecret: this.sharedSecret
        };
    }
}
```

```html
<!-- dh-key-exchange.html -->
<template>
    <div class="container">
        <h2>Diffie-Hellman Key Exchange</h2>
        
        <lightning-card title="Key Exchange Parameters">
            <div class="slds-grid slds-gutters slds-wrap">
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                    <p><strong>Prime (p):</strong> {prime}</p>
                    <p><strong>Base (g):</strong> {base}</p>
                </div>
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                    <lightning-button 
                        label="Generate New Keys" 
                        onclick={handleGenerateKeys}
                        variant="brand">
                    </lightning-button>
                </div>
            </div>
        </lightning-card>

        <lightning-card title="Participant A">
            <div class="slds-grid slds-gutters slds-wrap">
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                    <p><strong>Private Key (a):</strong> {privateKeyA}</p>
                    <p><strong>Public Key (A):</strong> {publicKeyA}</p>
                </div>
            </div>
        </lightning-card>

        <lightning-card title="Participant B">
            <div class="slds-grid slds-gutters slds-wrap">
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                    <p><strong>Private Key (b):</strong> {privateKeyB}</p>
                    <p><strong>Public Key (B):</strong> {publicKeyB}</p>
                </div>
            </div>
        </lightning-card>

        <lightning-card title="Shared Secret">
            <div class="slds-grid slds-gutters slds-wrap">
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                    <p><strong>Shared Secret (S):</strong> {sharedSecret}</p>
                </div>
            </div>
        </lightning-card>

        <lightning-card title="Algorithm Steps">
            <ul class="slds-list_dotted">
                <li>Both parties agree on prime number (p) and base (g)</li>
                <li>Participant A generates private key (a) and computes public key A = g^a mod p</li>
                <li>Participant B generates private key (b) and computes public key B = g^b mod p</li>
                <li>Each party exchanges public keys</li>
                <li>A computes: S = B^a mod p</li>
                <li>B computes: S = A^b mod p</li>
                <li>Both parties now have the same shared secret S</li>
            </ul>
        </lightning-card>
    </div>
</template>
```

```css
/* dh-key-exchange.css */
.container {
    padding: 1rem;
    max-width: 1200px;
    margin: 0 auto;
}

.slds-card {
    margin-bottom: 1rem;
}

.slds-grid {
    margin-bottom: 1rem;
}

.slds-list_dotted {
    padding-left: 1.5rem;
}

.slds-list_dotted li {
    margin-bottom: 0.5rem;
}
```

## How It Works

This Lightning Web Component demonstrates the Diffie-Hellman key exchange algorithm:

1. **Parameters**: Both parties agree on a prime number (p = 23) and a base (g = 5)
2. **Private Keys**: Each participant generates a random private key
3. **Public Keys**: Each participant calculates their public key using modular exponentiation
4. **Key Exchange**: Participants exchange public keys
5. **Shared Secret**: Both participants compute the same shared secret using their private keys and the received public keys

## Key Features

- **Modular Exponentiation**: Implements efficient modular exponentiation algorithm
- **Real-time Calculation**: Shows step-by-step key generation
- **Visual Display**: Clear presentation of all key values
- **Educational**: Includes algorithm explanation in the UI

## Security Note

This example uses simple random number generation for demonstration. In production, always use cryptographically secure random number generators and proper key management practices.

