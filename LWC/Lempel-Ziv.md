# Lempel-Ziv Compression in Lightning Web Component

Here's an example implementation of the Lempel-Ziv compression algorithm in Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class LzwCompression extends LightningElement {
    inputText = '';
    compressedData = '';
    decompressedData = '';
    isCompressed = false;

    handleInputChange(event) {
        this.inputText = event.target.value;
    }

    compress() {
        if (!this.inputText) {
            return;
        }

        const compressed = this.lzwCompress(this.inputText);
        this.compressedData = compressed.join(' ');
        this.isCompressed = true;
    }

    decompress() {
        if (!this.compressedData) {
            return;
        }

        const compressedArray = this.compressedData.split(' ').map(Number);
        const decompressed = this.lzwDecompress(compressedArray);
        this.decompressedData = decompressed;
        this.isCompressed = false;
    }

    // LZW Compression Algorithm
    lzwCompress(text) {
        const dictionary = new Map();
        const result = [];
        
        // Initialize dictionary with single characters
        let code = 256;
        for (let i = 0; i < 256; i++) {
            dictionary.set(String.fromCharCode(i), i);
        }

        let currentString = '';
        
        for (let i = 0; i < text.length; i++) {
            const char = text[i];
            const tempString = currentString + char;
            
            if (dictionary.has(tempString)) {
                currentString = tempString;
            } else {
                result.push(dictionary.get(currentString));
                dictionary.set(tempString, code++);
                currentString = char;
            }
        }
        
        if (currentString) {
            result.push(dictionary.get(currentString));
        }
        
        return result;
    }

    // LZW Decompression Algorithm
    lzwDecompress(compressed) {
        const dictionary = new Map();
        const result = [];
        
        // Initialize dictionary with single characters
        let code = 256;
        for (let i = 0; i < 256; i++) {
            dictionary.set(i, String.fromCharCode(i));
        }

        if (compressed.length === 0) {
            return '';
        }

        let oldString = dictionary.get(compressed[0]);
        result.push(oldString);
        
        for (let i = 1; i < compressed.length; i++) {
            const codeValue = compressed[i];
            let newString;
            
            if (dictionary.has(codeValue)) {
                newString = dictionary.get(codeValue);
            } else {
                newString = oldString + oldString[0];
            }
            
            result.push(newString);
            dictionary.set(code++, oldString + newString[0]);
            oldString = newString;
        }
        
        return result.join('');
    }

    handleCompress() {
        this.compress();
    }

    handleDecompress() {
        this.decompress();
    }

    get compressedSize() {
        return this.compressedData ? this.compressedData.length : 0;
    }

    get originalSize() {
        return this.inputText ? this.inputText.length : 0;
    }

    get compressionRatio() {
        if (!this.inputText || !this.compressedData) {
            return 0;
        }
        return ((this.inputText.length - this.compressedData.length) / this.inputText.length * 100).toFixed(2);
    }
}
```

```html
<template>
    <div class="container">
        <h2>LZW Compression Demo</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input Text" 
                value={inputText}
                onchange={handleInputChange}
                multiline
                rows="5"
                variant="standard">
            </lightning-input>
        </div>

        <div class="button-section">
            <lightning-button 
                label="Compress" 
                onclick={handleCompress}
                variant="brand"
                disabled={inputText === ''}>
            </lightning-button>
            
            <lightning-button 
                label="Decompress" 
                onclick={handleDecompress}
                variant="success"
                disabled={compressedData === ''}>
            </lightning-button>
        </div>

        <div class="output-section">
            <div class="compressed-section">
                <lightning-input 
                    label="Compressed Data" 
                    value={compressedData}
                    readonly
                    multiline
                    rows="3"
                    variant="standard">
                </lightning-input>
            </div>

            <div class="decompressed-section">
                <lightning-input 
                    label="Decompressed Data" 
                    value={decompressedData}
                    readonly
                    multiline
                    rows="3"
                    variant="standard">
                </lightning-input>
            </div>
        </div>

        <div class="stats-section">
            <lightning-card title="Compression Statistics">
                <div class="stats">
                    <p>Original Size: {originalSize} characters</p>
                    <p>Compressed Size: {compressedSize} characters</p>
                    <p>Compression Ratio: {compressionRatio}%</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.button-section {
    display: flex;
    gap: 10px;
    margin-bottom: 20px;
    justify-content: center;
}

.output-section {
    margin-bottom: 20px;
}

.compressed-section, .decompressed-section {
    margin-bottom: 15px;
}

.stats-section {
    margin-top: 20px;
}

.stats {
    display: flex;
    flex-direction: column;
    gap: 10px;
}

.stats p {
    margin: 0;
    padding: 5px;
    background-color: #f5f5f5;
    border-radius: 4px;
}
```

## How it works:

1. **LZW Compression Algorithm**:
   - Builds a dictionary of strings and their codes
   - Starts with single characters (codes 0-255)
   - For each character, checks if the current string + new character exists in dictionary
   - If yes, continues building the string
   - If no, outputs the code for current string and adds new string to dictionary

2. **LZW Decompression Algorithm**:
   - Rebuilds the same dictionary during decompression
   - Uses the codes to reconstruct the original string
   - Handles the special case where a code references a string not yet in dictionary

3. **Features**:
   - Real-time compression/decompression
   - Compression statistics display
   - Input validation
   - Responsive UI with Lightning Web Component styling

This implementation demonstrates the core LZW algorithm principles in a practical LWC component.

