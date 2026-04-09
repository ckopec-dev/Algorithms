# MD5 Hashing in Lightning Web Component

Here's an example of implementing MD5 hashing in a Lightning Web Component:

## Component Structure

### myMD5Component.js
```javascript
import { LightningElement } from 'lwc';
import { loadScript } from 'lightning/platformResourceLoader';
import md5 from '@salesforce/resourceUrl/md5'; // Assuming md5 library is loaded

export default class MyMD5Component extends LightningElement {
    inputText = '';
    hashedText = '';
    isLoaded = false;

    handleInputChange(event) {
        this.inputText = event.target.value;
    }

    async handleHash() {
        if (this.inputText) {
            try {
                // Load the MD5 library
                if (!this.isLoaded) {
                    await loadScript(this, md5);
                    this.isLoaded = true;
                }
                
                // Generate MD5 hash
                this.hashedText = md5(this.inputText);
            } catch (error) {
                console.error('Error generating MD5 hash:', error);
            }
        }
    }

    handleClear() {
        this.inputText = '';
        this.hashedText = '';
    }
}
```

### myMD5Component.html
```html
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <h2 class="slds-text-heading_small">MD5 Hash Generator</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Input Text</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={inputText}
                    onchange={handleInputChange}
                    placeholder="Enter text to hash"
                />
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Generate MD5" 
                variant="brand" 
                onclick={handleHash}
                class="slds-m-right_small"
            ></lightning-button>
            <lightning-button 
                label="Clear" 
                variant="neutral" 
                onclick={handleClear}
            ></lightning-button>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="MD5 Hash Result">
                <div class="slds-text-body_small">
                    <p><strong>Input:</strong> {inputText}</p>
                    <p><strong>MD5 Hash:</strong> {hashedText}</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

### myMD5Component.css
```css
.slds-box {
    background-color: #f4f6f9;
    border: 1px solid #e1e8f0;
}

.slds-input {
    width: 100%;
}

.slds-text-heading_small {
    color: #0070d2;
}
```

## Alternative Implementation using CryptoJS

### myMD5Component.js (Alternative)
```javascript
import { LightningElement } from 'lwc';
import { loadScript } from 'lightning/platformResourceLoader';
import cryptoJs from '@salesforce/resourceUrl/cryptoJs';

export default class MyMD5Component extends LightningElement {
    inputText = '';
    hashedText = '';

    handleInputChange(event) {
        this.inputText = event.target.value;
    }

    async handleHash() {
        if (this.inputText) {
            try {
                await loadScript(this, cryptoJs);
                
                // Generate MD5 hash using CryptoJS
                const hash = CryptoJS.MD5(this.inputText).toString();
                this.hashedText = hash;
            } catch (error) {
                console.error('Error generating MD5 hash:', error);
            }
        }
    }

    handleClear() {
        this.inputText = '';
        this.hashedText = '';
    }
}
```

## Key Points:

1. **Library Loading**: MD5 functionality requires external libraries like `crypto-js` or custom MD5 implementations
2. **Async Loading**: Use `loadScript` to load external libraries asynchronously
3. **Error Handling**: Always include try-catch blocks for robust error handling
4. **User Experience**: Provide clear input/output fields with clear actions
5. **Security Note**: MD5 is not recommended for security-sensitive applications due to collision vulnerabilities

## Usage:
1. Create a Lightning Web Component with the above code
2. Include the required MD5 library in your project resources
3. Deploy to Salesforce org
4. Add component to a Lightning page

This implementation provides a clean UI for generating MD5 hashes from user input while handling the asynchronous loading of cryptographic libraries.

