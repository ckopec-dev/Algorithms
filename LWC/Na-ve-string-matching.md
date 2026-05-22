# Naïve String Matching Algorithm in Lightning Web Component

Here's a complete example of implementing the Naïve String Matching algorithm in a Lightning Web Component:

## HTML Template (naiveStringMatching.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_small">Naïve String Matching Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Text to Search In:</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea" 
                    value={inputText} 
                    onchange={handleTextChange}
                    placeholder="Enter the text to search in..."
                    rows="4">
                </textarea>
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Pattern to Search:</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={searchPattern} 
                    onchange={handlePatternChange}
                    placeholder="Enter the pattern to search for...">
            </div>
        </div>

        <div class="slds-form-element">
            <button 
                class="slds-button slds-button_brand"
                onclick={performSearch}>
                Find Pattern
            </button>
        </div>

        <div if:true={searchResults.length > 0} class="slds-m-top_medium">
            <h3 class="slds-text-heading_small">Search Results:</h3>
            <ul class="slds-list_dotted">
                <template for:each={searchResults} for:item="result">
                    <li key={result.index} class="slds-m-bottom_small">
                        <span class="slds-text-title">Found at index: {result.index}</span>
                        <span class="slds-text-body_small slds-m-left_x-small">({result.context})</span>
                    </li>
                </template>
            </ul>
        </div>

        <div if:true={searchResults.length === 0 && searchPerformed} class="slds-m-top_medium">
            <p class="slds-text-body_small slds-text-color_error">Pattern not found in the text.</p>
        </div>

        <div class="slds-m-top_medium">
            <h3 class="slds-text-heading_small">Algorithm Explanation:</h3>
            <p class="slds-text-body_small">
                The Naïve String Matching algorithm checks every possible position in the text 
                to see if the pattern matches. It has a time complexity of O(n*m) where n is 
                the length of the text and m is the length of the pattern.
            </p>
        </div>
    </div>
</template>
```

## JavaScript Controller (naiveStringMatching.js)
```javascript
import { LightningElement } from 'lwc';

export default class NaiveStringMatching extends LightningElement {
    inputText = '';
    searchPattern = '';
    searchResults = [];
    searchPerformed = false;

    handleTextChange(event) {
        this.inputText = event.target.value;
    }

    handlePatternChange(event) {
        this.searchPattern = event.target.value;
    }

    performSearch() {
        if (!this.inputText || !this.searchPattern) {
            this.searchResults = [];
            this.searchPerformed = false;
            return;
        }

        this.searchResults = this.naiveStringMatching(this.inputText, this.searchPattern);
        this.searchPerformed = true;
    }

    /**
     * Naïve String Matching Algorithm Implementation
     * @param {string} text - The text to search in
     * @param {string} pattern - The pattern to search for
     * @returns {Array} Array of objects containing match information
     */
    naiveStringMatching(text, pattern) {
        const results = [];
        const textLength = text.length;
        const patternLength = pattern.length;

        // If pattern is longer than text, no matches possible
        if (patternLength > textLength) {
            return results;
        }

        // Loop through each possible starting position in text
        for (let i = 0; i <= textLength - patternLength; i++) {
            let j;
            
            // Check if pattern matches at current position
            for (j = 0; j < patternLength; j++) {
                if (text[i + j] !== pattern[j]) {
                    break;
                }
            }
            
            // If we've matched all characters, we found a match
            if (j === patternLength) {
                // Create context for better visualization
                const contextStart = Math.max(0, i - 10);
                const contextEnd = Math.min(textLength, i + patternLength + 10);
                const context = text.substring(contextStart, contextEnd);
                
                results.push({
                    index: i,
                    context: context,
                    matchedText: pattern
                });
            }
        }

        return results;
    }
}
```

## CSS Styles (naiveStringMatching.css)
```css
.slds-box {
    padding: 1rem;
}

.slds-text-heading_small {
    font-size: 1rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
}

.slds-form-element {
    margin-bottom: 1rem;
}

.slds-text-title {
    font-weight: 600;
}

.slds-text-body_small {
    font-size: 0.875rem;
}

.slds-m-top_medium {
    margin-top: 1rem;
}

.slds-m-bottom_small {
    margin-bottom: 0.5rem;
}

.slds-m-left_x-small {
    margin-left: 0.25rem;
}
```

## Usage Example

When you use this component:

1. **Input Text**: "Hello world, this is a hello world example"
2. **Search Pattern**: "hello"
3. **Results**: 
   - Found at index: 0
   - Found at index: 20

## Algorithm Steps

The naive string matching algorithm works as follows:

1. Start at position 0 in the text
2. Compare the pattern with the text at that position
3. If all characters match, record the position
4. If any character doesn't match, move to the next position
5. Repeat until the end of the text

## Time Complexity
- **Best Case**: O(n) - when pattern is not found
- **Average/Worst Case**: O(n×m) - where n is text length and m is pattern length

## Space Complexity
- O(1) - only using a constant amount of extra space

This implementation provides a complete, reusable LWC component that demonstrates the naive string matching algorithm with a clean user interface and proper error handling.

