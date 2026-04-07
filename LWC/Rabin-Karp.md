# Rabin-Karp Algorithm in Lightning Web Component

Here's a complete example of implementing the Rabin-Karp string matching algorithm in a Lightning Web Component:

```javascript
// rabinKarp.js
import { LightningElement } from 'lwc';

export default class RabinKarp extends LightningElement {
    text = '';
    pattern = '';
    result = '';
    showResult = false;

    handleTextChange(event) {
        this.text = event.target.value;
    }

    handlePatternChange(event) {
        this.pattern = event.target.value;
    }

    handleSearch() {
        if (!this.text || !this.pattern) {
            this.result = 'Please enter both text and pattern';
            this.showResult = true;
            return;
        }

        const positions = this.rabinKarpSearch(this.text, this.pattern);
        if (positions.length > 0) {
            this.result = `Pattern found at positions: ${positions.join(', ')}`;
        } else {
            this.result = 'Pattern not found in text';
        }
        this.showResult = true;
    }

    rabinKarpSearch(text, pattern) {
        const textLength = text.length;
        const patternLength = pattern.length;
        const prime = 101; // A prime number
        const base = 256; // Number of characters in input alphabet
        
        const positions = [];
        
        // Edge cases
        if (patternLength > textLength) {
            return positions;
        }
        
        // Calculate base^(patternLength-1) % prime
        let power = 1;
        for (let i = 0; i < patternLength - 1; i++) {
            power = (power * base) % prime;
        }
        
        // Calculate hash value of pattern and first window of text
        let patternHash = 0;
        let textHash = 0;
        
        for (let i = 0; i < patternLength; i++) {
            patternHash = (patternHash * base + pattern.charCodeAt(i)) % prime;
            textHash = (textHash * base + text.charCodeAt(i)) % prime;
        }
        
        // Slide the pattern over text one by one
        for (let i = 0; i <= textLength - patternLength; i++) {
            // Check if hash values match
            if (patternHash === textHash) {
                // Check characters one by one
                let j;
                for (j = 0; j < patternLength; j++) {
                    if (text[i + j] !== pattern[j]) {
                        break;
                    }
                }
                if (j === patternLength) {
                    positions.push(i);
                }
            }
            
            // Calculate hash value for next window of text
            if (i < textLength - patternLength) {
                textHash = (base * (textHash - text.charCodeAt(i) * power) + text.charCodeAt(i + patternLength)) % prime;
                
                // Handle negative hash values
                if (textHash < 0) {
                    textHash += prime;
                }
            }
        }
        
        return positions;
    }

    clearSearch() {
        this.text = '';
        this.pattern = '';
        this.result = '';
        this.showResult = false;
    }
}
```

```html
<!-- rabinKarp.html -->
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <h2>Rabin-Karp String Search Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="text">Text</label>
            <div class="slds-form-element__control">
                <textarea 
                    id="text" 
                    class="slds-textarea"
                    value={text}
                    onchange={handleTextChange}
                    placeholder="Enter text to search in..."
                    rows="4">
                </textarea>
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label" for="pattern">Pattern</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="pattern" 
                    class="slds-input"
                    value={pattern}
                    onchange={handlePatternChange}
                    placeholder="Enter pattern to search for..."
                />
            </div>
        </div>

        <div class="slds-m-top_small">
            <lightning-button 
                label="Search" 
                variant="brand" 
                onclick={handleSearch}
                class="slds-m-right_small">
            </lightning-button>
            <lightning-button 
                label="Clear" 
                variant="neutral" 
                onclick={clearSearch}>
            </lightning-button>
        </div>

        <template if:true={showResult}>
            <div class="slds-m-top_small slds-p-around_small slds-theme_info">
                <p><strong>Result:</strong> {result}</p>
            </div>
        </template>

        <div class="slds-m-top_medium">
            <h3>About Rabin-Karp Algorithm</h3>
            <ul class="slds-list_dotted">
                <li>Time complexity: O(n+m) average case, O(nm) worst case</li>
                <li>Space complexity: O(1)</li>
                <li>Uses rolling hash to find exact matches</li>
                <li>Good for multiple pattern searching</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* rabinKarp.css */
.slds-box {
    background-color: #f4f6f9;
    border: 1px solid #d8dde6;
    border-radius: 0.25rem;
    padding: 1rem;
}

.slds-input,
.slds-textarea {
    width: 100%;
    margin-bottom: 0.5rem;
}

.slds-button {
    margin-right: 0.5rem;
}
```

## How the Algorithm Works:

1. **Hash Calculation**: Computes hash values for the pattern and each window of text
2. **Rolling Hash**: Efficiently updates hash values as the window slides
3. **Collision Handling**: When hash values match, performs character-by-character comparison
4. **Result Tracking**: Records all positions where pattern is found

## Key Features:

- **Real-time search**: Instantly finds all occurrences of pattern in text
- **User-friendly interface**: Clean input fields with clear results display
- **Edge case handling**: Properly handles empty inputs and pattern longer than text
- **Performance optimized**: Uses modular arithmetic to prevent integer overflow
- **Educational**: Includes algorithm information and complexity analysis

## Usage:

1. Enter text in the text area
2. Enter pattern to search for
3. Click "Search" to find all occurrences
4. Results show positions (0-indexed) where pattern is found
5. Use "Clear" to reset the form

This implementation demonstrates the Rabin-Karp algorithm's efficiency in string searching with its rolling hash technique, making it particularly useful for scenarios requiring multiple pattern searches or when dealing with large texts.

