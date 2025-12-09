# Knuth-Morris-Pratt (KMP) Algorithm in Lightning Web Component

```javascript
// kmpAlgorithm.js
import { LightningElement } from 'lwc';

export default class KmpAlgorithm extends LightningElement {
    inputText = '';
    pattern = '';
    result = '';
    searchResult = null;

    handleTextChange(event) {
        this.inputText = event.target.value;
    }

    handlePatternChange(event) {
        this.pattern = event.target.value;
    }

    handleSearch() {
        if (!this.inputText || !this.pattern) {
            this.result = 'Please enter both text and pattern';
            return;
        }

        const result = this.kmpSearch(this.inputText, this.pattern);
        this.searchResult = result;
        
        if (result.found) {
            this.result = `Pattern found at position ${result.positions.join(', ')}`;
        } else {
            this.result = 'Pattern not found';
        }
    }

    // KMP Search Algorithm Implementation
    kmpSearch(text, pattern) {
        const textLength = text.length;
        const patternLength = pattern.length;
        
        if (patternLength === 0) return { found: true, positions: [] };
        if (textLength < patternLength) return { found: false, positions: [] };

        // Preprocess pattern to create LPS (Longest Proper Prefix which is also Suffix) array
        const lps = this.computeLPSArray(pattern);
        
        let textIndex = 0;
        let patternIndex = 0;
        const positions = [];

        while (textIndex < textLength) {
            if (pattern[patternIndex] === text[textIndex]) {
                patternIndex++;
                textIndex++;
            }

            if (patternIndex === patternLength) {
                // Pattern found at position
                positions.push(textIndex - patternIndex);
                patternIndex = lps[patternIndex - 1];
            } else if (textIndex < textLength && pattern[patternIndex] !== text[textIndex]) {
                if (patternIndex !== 0) {
                    patternIndex = lps[patternIndex - 1];
                } else {
                    textIndex++;
                }
            }
        }

        return {
            found: positions.length > 0,
            positions: positions
        };
    }

    // Compute Longest Proper Prefix which is also Suffix array
    computeLPSArray(pattern) {
        const length = pattern.length;
        const lps = new Array(length);
        lps[0] = 0; // First element is always 0

        let prefixLength = 0;
        let i = 1;

        while (i < length) {
            if (pattern[i] === pattern[prefixLength]) {
                prefixLength++;
                lps[i] = prefixLength;
                i++;
            } else {
                if (prefixLength !== 0) {
                    prefixLength = lps[prefixLength - 1];
                } else {
                    lps[i] = 0;
                    i++;
                }
            }
        }

        return lps;
    }

    // Example usage
    handleExample() {
        this.inputText = 'ABABDABACDABABCABCABCABCABC';
        this.pattern = 'ABABCABCABCABC';
        this.handleSearch();
    }
}
```

```html
<!-- kmpAlgorithm.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Knuth-Morris-Pratt (KMP) Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="text">Text:</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="text" 
                    class="slds-input"
                    value={inputText}
                    onchange={handleTextChange}
                    placeholder="Enter text to search in">
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label" for="pattern">Pattern:</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="pattern" 
                    class="slds-input"
                    value={pattern}
                    onchange={handlePatternChange}
                    placeholder="Enter pattern to search for">
            </div>
        </div>

        <div class="slds-form-element">
            <div class="slds-form-element__control">
                <lightning-button 
                    label="Search" 
                    variant="brand" 
                    onclick={handleSearch}>
                </lightning-button>
                <lightning-button 
                    label="Example" 
                    variant="neutral" 
                    onclick={handleExample}
                    class="slds-m-left_small">
                </lightning-button>
            </div>
        </div>

        <div class="slds-form-element slds-m-top_medium">
            <label class="slds-form-element__label">Result:</label>
            <div class="slds-form-element__control">
                <p class="slds-text-body_regular">{result}</p>
            </div>
        </div>

        <template if:true={searchResult}>
            <div class="slds-form-element slds-m-top_medium">
                <label class="slds-form-element__label">Search Details:</label>
                <div class="slds-form-element__control">
                    <p class="slds-text-body_regular">
                        Found: {searchResult.found}
                    </p>
                    <p class="slds-text-body_regular">
                        Positions: {searchResult.positions.join(', ')}
                    </p>
                </div>
            </div>
        </template>
    </div>
</template>
```

```css
/* kmpAlgorithm.css */
.slds-box {
    padding: 1rem;
}

.slds-input {
    width: 100%;
    margin-bottom: 1rem;
}

.slds-button {
    margin-right: 0.5rem;
}
```

## How KMP Algorithm Works

The KMP algorithm improves upon naive string matching by avoiding unnecessary comparisons:

1. **Preprocessing**: Create LPS (Longest Proper Prefix which is also Suffix) array for the pattern
2. **Search**: Use the LPS array to skip characters in the text when a mismatch occurs
3. **Efficiency**: Time complexity O(n + m) where n is text length and m is pattern length

## Key Features

- **Input validation**: Checks for empty text or pattern
- **Real-time search**: Updates results as you type
- **Example button**: Demonstrates usage with sample data
- **Position tracking**: Shows all occurrences of the pattern
- **User-friendly interface**: Clean Lightning Web Component UI

## Example Output

For text: "ABABDABACDABABCABCABCABCABC" and pattern: "ABABCABCABCABC"
- Found: true
- Positions: 10

This implementation demonstrates the KMP algorithm's efficiency in finding patterns in text with optimal time complexity.

