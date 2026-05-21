# Run-Length Encoding (RLE) in Lightning Web Component

Here's a complete example of implementing Run-Length Encoding algorithm in a Lightning Web Component:

## LWC Component Structure

### rleEncoder.js
```javascript
import { LightningElement, track } from 'lwc';

export default class RleEncoder extends LightningElement {
    @track inputText = '';
    @track encodedResult = '';
    @track decodedResult = '';
    @track isEncoded = false;

    handleInputChange(event) {
        this.inputText = event.target.value;
        this.clearResults();
    }

    encode() {
        if (!this.inputText) {
            this.encodedResult = 'Please enter some text to encode';
            return;
        }

        this.encodedResult = this.runLengthEncode(this.inputText);
        this.isEncoded = true;
    }

    decode() {
        if (!this.encodedResult || this.encodedResult === 'Please enter some text to encode') {
            this.decodedResult = 'No encoded text to decode';
            return;
        }

        this.decodedResult = this.runLengthDecode(this.encodedResult);
        this.isEncoded = false;
    }

    runLengthEncode(input) {
        if (!input) return '';

        let encoded = '';
        let count = 1;
        let currentChar = input[0];

        for (let i = 1; i <= input.length; i++) {
            if (i < input.length && input[i] === currentChar) {
                count++;
            } else {
                // Add count and character to result
                if (count > 1) {
                    encoded += count + currentChar;
                } else {
                    encoded += currentChar;
                }
                
                // Reset for next character
                currentChar = input[i];
                count = 1;
            }
        }

        return encoded;
    }

    runLengthDecode(encoded) {
        if (!encoded) return '';

        let decoded = '';
        let i = 0;

        while (i < encoded.length) {
            // Extract the count (could be multiple digits)
            let count = '';
            while (i < encoded.length && /\d/.test(encoded[i])) {
                count += encoded[i];
                i++;
            }

            // Get the character
            if (i < encoded.length) {
                let char = encoded[i];
                i++;
                
                // Repeat character 'count' times
                for (let j = 0; j < (count || 1); j++) {
                    decoded += char;
                }
            }
        }

        return decoded;
    }

    clearResults() {
        this.encodedResult = '';
        this.decodedResult = '';
        this.isEncoded = false;
    }

    handleEncodeClick() {
        this.encode();
    }

    handleDecodeClick() {
        this.decode();
    }
}
```

### rleEncoder.html
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_small">Run-Length Encoding (RLE)</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="inputText">Input Text</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="inputText"
                    class="slds-input"
                    value={inputText}
                    onchange={handleInputChange}
                    placeholder="Enter text to encode..."
                />
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Encode" 
                variant="brand" 
                onclick={handleEncodeClick}
                class="slds-m-right_small"
            ></lightning-button>
            <lightning-button 
                label="Decode" 
                variant="outline-brand" 
                onclick={handleDecodeClick}
                class="slds-m-right_small"
            ></lightning-button>
            <lightning-button 
                label="Clear" 
                variant="destructive" 
                onclick={clearResults}
            ></lightning-button>
        </div>

        <div class="slds-m-top_medium">
            <div class="slds-form-element">
                <label class="slds-form-element__label">Encoded Result</label>
                <div class="slds-form-element__control">
                    <output class="slds-text-body_regular slds-p-around_small slds-theme_info" 
                            style="word-break: break-all;">
                        {encodedResult}
                    </output>
                </div>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <div class="slds-form-element">
                <label class="slds-form-element__label">Decoded Result</label>
                <div class="slds-form-element__control">
                    <output class="slds-text-body_regular slds-p-around_small slds-theme_info" 
                            style="word-break: break-all;">
                        {decodedResult}
                    </output>
                </div>
            </div>
        </div>

        <div class="slds-m-top_medium slds-text-body_small">
            <p><strong>How RLE works:</strong></p>
            <ul>
                <li>Counts consecutive identical characters</li>
                <li>Replaces runs with count + character</li>
                <li>Example: "aaabbbcc" becomes "3a3b2c"</li>
                <li>Single characters remain as-is</li>
            </ul>
        </div>
    </div>
</template>
```

### rleEncoder.css
```css
.slds-box {
    border: 1px solid #e1e1e1;
    border-radius: 0.25rem;
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 500px;
}

.slds-text-body_regular {
    font-size: 0.875rem;
    line-height: 1.4;
}

.slds-theme_info {
    background-color: #f3f2f2;
    border: 1px solid #e1e1e1;
    border-radius: 0.25rem;
}
```

## Example Usage

**Input:** `"aaabbbcccaaa"`
**Output:** `"3a3b3c3a"`

**Input:** `"hello world"`
**Output:** `"hello world"` (no consecutive characters)

**Input:** `"aaaa1111bbbb"`
**Output:** `"4a414b"`

## How It Works

1. **Encoding Process:**
   - Traverse through the input string
   - Count consecutive identical characters
   - If count > 1, output count + character
   - If count = 1, output just the character

2. **Decoding Process:**
   - Parse numbers in the encoded string
   - Repeat the following character that many times
   - Handle multi-digit counts properly

This implementation handles edge cases like empty strings, single characters, and multi-digit counts in the encoded output.

