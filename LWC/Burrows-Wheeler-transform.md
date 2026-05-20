# Burrows-Wheeler Transform in Lightning Web Component

Here's an implementation of the Burrows-Wheeler Transform algorithm in Lightning Web Component:

```javascript
// bwt.js
import { LightningElement } from 'lwc';

export default class Bwt extends LightningElement {
    inputText = '';
    transformedText = '';
    originalText = '';
    isTransformed = false;

    handleInputChange(event) {
        this.inputText = event.target.value;
    }

    // Burrows-Wheeler Transform algorithm
    burrowsWheelerTransform(text) {
        // Add sentinel character (typically '$') to the end
        const sentinel = '$';
        const extendedText = text + sentinel;
        
        // Create all rotations of the text
        const rotations = [];
        for (let i = 0; i < extendedText.length; i++) {
            const rotation = extendedText.slice(i) + extendedText.slice(0, i);
            rotations.push(rotation);
        }
        
        // Sort rotations lexicographically
        rotations.sort();
        
        // Take the last character of each sorted rotation
        const bwtResult = rotations.map(rotation => rotation[rotation.length - 1]).join('');
        
        return bwtResult;
    }

    // Burrows-Wheeler Inverse Transform algorithm
    burrowsWheelerInverseTransform(bwtText) {
        const n = bwtText.length;
        const firstColumn = bwtText.split('').sort().join('');
        
        // Create a mapping from first column to last column
        const firstToLast = {};
        const firstCount = {};
        const lastCount = {};
        
        // Count occurrences in first column
        for (let i = 0; i < n; i++) {
            firstCount[firstColumn[i]] = (firstCount[firstColumn[i]] || 0) + 1;
            lastCount[bwtText[i]] = (lastCount[bwtText[i]] || 0) + 1;
        }
        
        // Create mapping from last column to first column
        const firstIndices = {};
        const lastIndices = {};
        
        let firstIndex = 0;
        let lastIndex = 0;
        
        for (let char of Object.keys(firstCount).sort()) {
            firstIndices[char] = firstIndex;
            firstIndex += firstCount[char];
        }
        
        for (let char of Object.keys(lastCount).sort()) {
            lastIndices[char] = lastIndex;
            lastIndex += lastCount[char];
        }
        
        // Build the inverse mapping
        const inverseMap = [];
        for (let i = 0; i < n; i++) {
            const char = bwtText[i];
            const pos = lastIndices[char];
            inverseMap.push(pos);
            lastIndices[char]++;
        }
        
        // Reconstruct original text
        let result = '';
        let current = 0;
        
        for (let i = 0; i < n; i++) {
            result = firstColumn[current] + result;
            current = inverseMap[current];
        }
        
        // Remove sentinel character
        return result.slice(0, -1);
    }

    handleTransform() {
        if (this.inputText.trim() === '') {
            this.transformedText = '';
            this.isTransformed = false;
            return;
        }
        
        this.transformedText = this.burrowsWheelerTransform(this.inputText);
        this.isTransformed = true;
    }

    handleInverseTransform() {
        if (this.transformedText.trim() === '') {
            this.originalText = '';
            return;
        }
        
        this.originalText = this.burrowsWheelerInverseTransform(this.transformedText);
    }

    handleClear() {
        this.inputText = '';
        this.transformedText = '';
        this.originalText = '';
        this.isTransformed = false;
    }
}
```

```html
<!-- bwt.html -->
<template>
    <div class="container">
        <h2>Burrows-Wheeler Transform</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input Text" 
                value={inputText}
                onchange={handleInputChange}
                placeholder="Enter text to transform..."
                type="text">
            </lightning-input>
        </div>

        <div class="button-section">
            <lightning-button 
                label="Transform" 
                variant="brand" 
                onclick={handleTransform}
                disabled={inputText === ''}>
            </lightning-button>
            
            <lightning-button 
                label="Inverse Transform" 
                variant="neutral" 
                onclick={handleInverseTransform}
                disabled={transformedText === ''}>
            </lightning-button>
            
            <lightning-button 
                label="Clear" 
                variant="destructive" 
                onclick={handleClear}>
            </lightning-button>
        </div>

        <template if:true={isTransformed}>
            <div class="result-section">
                <lightning-card title="Transformed Text">
                    <div class="bwt-result">
                        {transformedText}
                    </div>
                </lightning-card>
            </div>
        </template>

        <template if:true={originalText}>
            <div class="result-section">
                <lightning-card title="Original Text (Inverse)">
                    <div class="bwt-result">
                        {originalText}
                    </div>
                </lightning-card>
            </div>
        </template>

        <div class="explanation">
            <h3>How it works:</h3>
            <ul>
                <li>BWT creates all rotations of the input text</li>
                <li>Sorts the rotations lexicographically</li>
                <li>Takes the last character of each sorted rotation</li>
                <li>The inverse transform reconstructs the original text</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* bwt.css */
.container {
    max-width: 800px;
    margin: 0 auto;
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    margin-bottom: 20px;
}

.button-section {
    display: flex;
    gap: 10px;
    margin-bottom: 20px;
    flex-wrap: wrap;
}

.bwt-result {
    font-family: monospace;
    font-size: 16px;
    padding: 10px;
    background-color: #f5f5f5;
    border-radius: 4px;
    word-break: break-all;
}

.result-section {
    margin-bottom: 20px;
}

.explanation {
    background-color: #e8f4f8;
    padding: 15px;
    border-radius: 4px;
    margin-top: 20px;
}

.explanation ul {
    margin: 10px 0;
    padding-left: 20px;
}

.explanation li {
    margin-bottom: 5px;
}
```

## Example Usage

**Input:** "banana"
**Transformed Output:** "annb$aa"

**Input:** "abracadabra"
**Transformed Output:** "rbbraaaaaca"

## How the Algorithm Works

1. **Forward Transform:**
   - Creates all possible rotations of the input text
   - Sorts these rotations lexicographically
   - Takes the last character of each sorted rotation to form the BWT output

2. **Inverse Transform:**
   - Reconstructs the original text by building a mapping between first and last columns
   - Uses the BWT result to trace back to the original sequence

This implementation provides a complete BWT solution within the Lightning Web Component framework with both forward and inverse transformations.

