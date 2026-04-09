# CIPHER__family Algorithm Example in Lightning Web Component

```javascript
// cipherFamily.js - Lightning Web Component JavaScript controller
import { LightningElement } from 'lwc';

export default class CipherFamily extends LightningElement {
    inputText = '';
    outputText = '';
    selectedCipher = 'caesar';
    shiftValue = 3;
    isEncrypted = true;

    // Cipher family algorithms
    cipherAlgorithms = {
        caesar: this.caesarCipher,
        rot13: this.rot13Cipher,
        vigenere: this.vigenereCipher,
        atbash: this.atbashCipher
    };

    // Caesar Cipher - shifts letters by a fixed number
    caesarCipher(text, shift, encrypt = true) {
        if (!encrypt) shift = -shift;
        return text.split('').map(char => {
            if (char.match(/[a-z]/i)) {
                const code = char.charCodeAt(0);
                const isUpperCase = code >= 65 && code <= 90;
                const base = isUpperCase ? 65 : 97;
                return String.fromCharCode((code - base + shift + 26) % 26 + base);
            }
            return char;
        }).join('');
    }

    // ROT13 Cipher - fixed shift of 13
    rot13Cipher(text) {
        return text.split('').map(char => {
            if (char.match(/[a-z]/i)) {
                const code = char.charCodeAt(0);
                const isUpperCase = code >= 65 && code <= 90;
                const base = isUpperCase ? 65 : 97;
                return String.fromCharCode((code - base + 13) % 26 + base);
            }
            return char;
        }).join('');
    }

    // Vigenère Cipher - uses a keyword for shifting
    vigenereCipher(text, keyword, encrypt = true) {
        const cleanText = text.replace(/[^a-zA-Z]/g, '').toLowerCase();
        const cleanKeyword = keyword.replace(/[^a-zA-Z]/g, '').toLowerCase();
        let result = '';
        let keyIndex = 0;
        
        for (let i = 0; i < cleanText.length; i++) {
            const charCode = cleanText.charCodeAt(i) - 97;
            const keyChar = cleanKeyword.charCodeAt(keyIndex % cleanKeyword.length) - 97;
            const shift = encrypt ? (charCode + keyChar) % 26 : (charCode - keyChar + 26) % 26;
            result += String.fromCharCode(shift + 97);
            keyIndex++;
        }
        return result;
    }

    // Atbash Cipher - maps letters to their reverse (a↔z)
    atbashCipher(text) {
        return text.split('').map(char => {
            if (char.match(/[a-z]/i)) {
                const code = char.charCodeAt(0);
                const isUpperCase = code >= 65 && code <= 90;
                const base = isUpperCase ? 65 : 97;
                return String.fromCharCode(25 - (code - base) + base);
            }
            return char;
        }).join('');
    }

    // Handle cipher selection
    handleCipherChange(event) {
        this.selectedCipher = event.target.value;
        this.processText();
    }

    // Handle shift value change
    handleShiftChange(event) {
        this.shiftValue = parseInt(event.target.value) || 0;
        this.processText();
    }

    // Handle input text change
    handleInputChange(event) {
        this.inputText = event.target.value;
        this.processText();
    }

    // Process text with selected cipher
    processText() {
        if (!this.inputText) {
            this.outputText = '';
            return;
        }

        let result = '';
        const cipherFunction = this.cipherAlgorithms[this.selectedCipher];

        switch (this.selectedCipher) {
            case 'caesar':
                result = this.caesarCipher(this.inputText, this.shiftValue, this.isEncrypted);
                break;
            case 'rot13':
                result = this.rot13Cipher(this.inputText);
                break;
            case 'vigenere':
                result = this.vigenereCipher(this.inputText, 'KEY', this.isEncrypted);
                break;
            case 'atbash':
                result = this.atbashCipher(this.inputText);
                break;
            default:
                result = this.inputText;
        }

        this.outputText = result;
    }

    // Toggle encryption/decryption
    toggleMode() {
        this.isEncrypted = !this.isEncrypted;
        this.processText();
    }

    // Get cipher name for display
    get cipherName() {
        const names = {
            caesar: 'Caesar Cipher',
            rot13: 'ROT13 Cipher',
            vigenere: 'Vigenère Cipher',
            atbash: 'Atbash Cipher'
        };
        return names[this.selectedCipher] || 'Cipher';
    }
}
```

```html
<!-- cipherFamily.html - Lightning Web Component HTML template -->
<template>
    <div class="cipher-container">
        <lightning-card title="Cipher Family Algorithms" icon-name="standard:lock">
            <div class="slds-grid slds-gutters slds-wrap">
                <!-- Cipher Selection -->
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12">
                    <lightning-combobox
                        label="Select Cipher Algorithm"
                        value={selectedCipher}
                        options={cipherOptions}
                        onchange={handleCipherChange}
                        variant="label-hidden">
                    </lightning-combobox>
                </div>

                <!-- Shift Value (for Caesar cipher) -->
                <div class="slds-col slds-size_12-of-12 slds-medium-size_6-of-12" if:true={showShiftInput}>
                    <lightning-input
                        type="number"
                        label="Shift Value"
                        value={shiftValue}
                        min="0"
                        max="25"
                        onchange={handleShiftChange}>
                    </lightning-input>
                </div>

                <!-- Mode Toggle -->
                <div class="slds-col slds-size_12-of-12">
                    <lightning-button
                        label={modeLabel}
                        onclick={toggleMode}
                        variant="brand">
                    </lightning-button>
                </div>

                <!-- Input Text -->
                <div class="slds-col slds-size_12-of-12">
                    <lightning-textarea
                        label="Input Text"
                        value={inputText}
                        onchange={handleInputChange}
                        placeholder="Enter text to encrypt/decrypt...">
                    </lightning-textarea>
                </div>

                <!-- Output Text -->
                <div class="slds-col slds-size_12-of-12">
                    <lightning-textarea
                        label="Output Text"
                        value={outputText}
                        readonly="true"
                        placeholder="Result will appear here...">
                    </lightning-textarea>
                </div>

                <!-- Cipher Info -->
                <div class="slds-col slds-size_12-of-12">
                    <lightning-card title={cipherName} icon-name="standard:security">
                        <p class="slds-p-around_small">
                            <strong>Description:</strong> {cipherDescription}
                        </p>
                    </lightning-card>
                </div>
            </div>
        </lightning-card>
    </div>
</template>
```

```css
/* cipherFamily.css - Lightning Web Component styling */
.cipher-container {
    padding: 1rem;
}

.cipher-description {
    font-size: 0.9rem;
    color: #333;
}
```

## Cipher Family Algorithms Implemented

This LWC demonstrates several classical cipher algorithms:

1. **Caesar Cipher** - Shifts letters by a fixed number
2. **ROT13 Cipher** - Fixed shift of 13 positions
3. **Vigenère Cipher** - Uses a keyword for variable shifting
4. **Atbash Cipher** - Maps letters to their reverse (a↔z)

## Key Features

- **Dynamic Algorithm Selection** - Users can switch between different cipher types
- **Real-time Processing** - Text is processed as it's entered
- **Interactive Controls** - Shift values, encryption/decryption modes
- **Responsive Design** - Works on all device sizes
- **Educational Purpose** - Demonstrates classical cryptography concepts

The component showcases how different cipher algorithms work while maintaining a clean, user-friendly interface suitable for educational or demonstration purposes.

