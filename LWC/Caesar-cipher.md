# Caesar Cipher in Lightning Web Component

Here's a complete example of a Caesar cipher implementation in Lightning Web Component:

## HTML Template (caesarCipher.html)
```html
<template>
    <div class="slds-box slds-box_small slds-m-around_medium">
        <h2 class="slds-text-heading_small">Caesar Cipher</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Text to Encrypt/Decrypt</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea" 
                    value={inputText} 
                    onchange={handleTextChange}
                    placeholder="Enter text here...">
                </textarea>
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Shift Value</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    class="slds-input" 
                    value={shiftValue} 
                    onchange={handleShiftChange}
                    min="0" 
                    max="25">
            </div>
        </div>

        <div class="slds-grid slds-gutters slds-m-top_medium">
            <div class="slds-col">
                <lightning-button 
                    label="Encrypt" 
                    variant="brand" 
                    onclick={handleEncrypt}
                    class="slds-m-right_small">
                </lightning-button>
                <lightning-button 
                    label="Decrypt" 
                    variant="neutral" 
                    onclick={handleDecrypt}>
                </lightning-button>
            </div>
        </div>

        <div class="slds-form-element slds-m-top_medium">
            <label class="slds-form-element__label">Result</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea" 
                    value={outputText} 
                    readonly>
                </textarea>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (caesarCipher.js)
```javascript
import { LightningElement } from 'lwc';

export default class CaesarCipher extends LightningElement {
    inputText = '';
    outputText = '';
    shiftValue = 3;

    handleTextChange(event) {
        this.inputText = event.target.value;
    }

    handleShiftChange(event) {
        this.shiftValue = parseInt(event.target.value) || 0;
    }

    handleEncrypt() {
        this.outputText = this.caesarCipher(this.inputText, this.shiftValue);
    }

    handleDecrypt() {
        this.outputText = this.caesarCipher(this.inputText, -this.shiftValue);
    }

    caesarCipher(text, shift) {
        let result = '';
        
        for (let i = 0; i < text.length; i++) {
            let char = text[i];
            
            // Check if character is uppercase
            if (char >= 'A' && char <= 'Z') {
                // Shift uppercase letters
                let charCode = char.charCodeAt(0);
                let shifted = ((charCode - 65 + shift) % 26 + 26) % 26;
                result += String.fromCharCode(shifted + 65);
            }
            // Check if character is lowercase
            else if (char >= 'a' && char <= 'z') {
                // Shift lowercase letters
                let charCode = char.charCodeAt(0);
                let shifted = ((charCode - 97 + shift) % 26 + 26) % 26;
                result += String.fromCharCode(shifted + 97);
            }
            // Keep non-alphabetic characters unchanged
            else {
                result += char;
            }
        }
        
        return result;
    }
}
```

## CSS Styles (caesarCipher.css)
```css
.slds-box {
    background-color: #ffffff;
    border: 1px solid #e5e5e5;
    border-radius: 0.25rem;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
}

.slds-textarea {
    min-height: 120px;
}

.slds-input {
    width: 100px;
}
```

## Key Features of this Implementation:

1. **Input/Output Fields**: Text areas for input and output
2. **Shift Value Control**: Number input for specifying shift amount
3. **Encrypt/Decrypt Buttons**: Toggle between encryption and decryption
4. **Case Preservation**: Maintains uppercase and lowercase letters
5. **Non-alphabetic Characters**: Preserves spaces, punctuation, and numbers
6. **Modular Logic**: Uses modulo arithmetic for proper wrapping
7. **Error Handling**: Handles negative shifts and edge cases

## How it Works:

- **Encryption**: Shifts each letter forward by the specified amount
- **Decryption**: Shifts each letter backward by the specified amount
- **Wrapping**: Uses modulo arithmetic to wrap around the alphabet
- **Preservation**: Non-alphabetic characters remain unchanged

This implementation provides a complete, user-friendly Caesar cipher tool within the Lightning Web Component framework.

