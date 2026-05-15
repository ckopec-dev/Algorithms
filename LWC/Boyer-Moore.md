# Boyer-Moore Algorithm in Lightning Web Component

Here's a complete example of implementing the Boyer-Moore string search algorithm in a Lightning Web Component:

```javascript
// boyerMoore.js
import { LightningElement } from 'lwc';

export default class BoyerMooreSearch extends LightningElement {
    searchInput = '';
    textInput = '';
    result = '';
    searchResult = null;

    handleSearchInput(event) {
        this.searchInput = event.target.value;
    }

    handleTextInput(event) {
        this.textInput = event.target.value;
    }

    handleSearch() {
        if (!this.searchInput || !this.textInput) {
            this.result = 'Please enter both search text and text to search in';
            return;
        }

        const text = this.textInput;
        const pattern = this.searchInput;
        
        const result = this.boyerMooreSearch(text, pattern);
        this.searchResult = result;
        
        if (result.found) {
            this.result = `Pattern found at position ${result.position}`;
        } else {
            this.result = 'Pattern not found in text';
        }
    }

    // Boyer-Moore search algorithm implementation
    boyerMooreSearch(text, pattern) {
        const textLength = text.length;
        const patternLength = pattern.length;

        if (patternLength === 0) {
            return { found: true, position: 0 };
        }

        if (patternLength > textLength) {
            return { found: false, position: -1 };
        }

        // Preprocessing: Create bad character table
        const badCharTable = this.buildBadCharTable(pattern);
        
        // Preprocessing: Create good suffix table
        const goodSuffixTable = this.buildGoodSuffixTable(pattern);

        let i = 0;
        while (i <= textLength - patternLength) {
            let j = patternLength - 1;

            // Match pattern from right to left
            while (j >= 0 && pattern[j] === text[i + j]) {
                j--;
            }

            // If pattern is found
            if (j < 0) {
                return { found: true, position: i };
            }

            // Calculate shift using bad character rule
            const badCharShift = j - badCharTable[text.charCodeAt(i + j)];
            
            // Calculate shift using good suffix rule
            const goodSuffixShift = this.calculateGoodSuffixShift(
                j, 
                patternLength, 
                goodSuffixTable
            );

            // Shift by maximum of both rules
            i += Math.max(badCharShift, goodSuffixShift);
        }

        return { found: false, position: -1 };
    }

    // Build bad character table
    buildBadCharTable(pattern) {
        const table = new Array(256).fill(-1); // ASCII table
        const patternLength = pattern.length;

        for (let i = 0; i < patternLength; i++) {
            table[pattern.charCodeAt(i)] = i;
        }

        return table;
    }

    // Build good suffix table
    buildGoodSuffixTable(pattern) {
        const length = pattern.length;
        const table = new Array(length + 1).fill(0);
        const suffix = new Array(length + 1).fill(0);

        // Compute suffix array
        suffix[length] = length;
        let i = length;
        let j = length;

        while (i > 0) {
            i--;
            while (j < length && pattern[i] !== pattern[j]) {
                j = suffix[j + 1];
            }
            suffix[i] = j;
        }

        // Compute good suffix table
        for (let i = 0; i <= length; i++) {
            table[i] = length - suffix[i];
        }

        // Adjust table for good suffix rule
        for (let i = 0; i < length; i++) {
            if (table[i] === length) {
                // No good suffix, use bad character shift
                table[i] = Math.max(1, i - suffix[i]);
            }
        }

        return table;
    }

    // Calculate good suffix shift
    calculateGoodSuffixShift(j, patternLength, goodSuffixTable) {
        if (j < patternLength - 1) {
            return goodSuffixTable[j + 1];
        }
        return 1;
    }

    // Find all occurrences of pattern in text
    findAllOccurrences(text, pattern) {
        const results = [];
        let i = 0;
        let found = false;

        while (i <= text.length - pattern.length) {
            const result = this.boyerMooreSearch(text.slice(i), pattern);
            if (result.found) {
                results.push(i + result.position);
                i += result.position + 1;
                found = true;
            } else {
                break;
            }
        }

        return {
            found: found,
            positions: results
        };
    }

    handleFindAll() {
        if (!this.searchInput || !this.textInput) {
            this.result = 'Please enter both search text and text to search in';
            return;
        }

        const text = this.textInput;
        const pattern = this.searchInput;
        
        const allResults = this.findAllOccurrences(text, pattern);
        this.searchResult = allResults;
        
        if (allResults.found) {
            this.result = `Pattern found at positions: ${allResults.positions.join(', ')}`;
        } else {
            this.result = 'Pattern not found in text';
        }
    }

    // Reset form
    handleReset() {
        this.searchInput = '';
        this.textInput = '';
        this.result = '';
        this.searchResult = null;
    }
}
```

```html
<!-- boyerMoore.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_small">Boyer-Moore String Search</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Pattern to Search</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input" 
                    value={searchInput}
                    onchange={handleSearchInput}
                    placeholder="Enter pattern to search for"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Text to Search In</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea" 
                    value={textInput}
                    onchange={handleTextInput}
                    placeholder="Enter text to search in"
                    rows="5"
                ></textarea>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Search" 
                variant="brand" 
                onclick={handleSearch}
                class="slds-m-right_small"
            ></lightning-button>
            
            <lightning-button 
                label="Find All Occurrences" 
                variant="outline-brand" 
                onclick={handleFindAll}
                class="slds-m-right_small"
            ></lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="destructive" 
                onclick={handleReset}
            ></lightning-button>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Result">
                <div class="slds-text-body_regular">
                    {result}
                </div>
            </lightning-card>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Algorithm Explanation">
                <p class="slds-text-body_small">
                    The Boyer-Moore algorithm is an efficient string-searching algorithm that 
                    uses two heuristics to skip sections of the text:
                </p>
                <ul class="slds-list_dotted">
                    <li>Bad Character Rule: Skip based on last character mismatch</li>
                    <li>Good Suffix Rule: Skip based on suffix matching</li>
                </ul>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
/* boyerMoore.css */
.slds-box {
    padding: 1rem;
}

.slds-textarea {
    resize: vertical;
}

.slds-m-right_small {
    margin-right: 0.5rem;
}
```

## Key Features of this Implementation:

1. **Complete Boyer-Moore Algorithm**: Implements both bad character and good suffix heuristics
2. **User Interface**: Clean form with input fields and action buttons
3. **Multiple Search Options**: Single search and find all occurrences
4. **Error Handling**: Handles edge cases like empty inputs and pattern longer than text
5. **Visual Feedback**: Clear result display with position information
6. **Educational Component**: Includes explanation of the algorithm

## How it Works:

1. **Preprocessing**: Creates bad character table and good suffix table
2. **Search Process**: Uses both heuristics to determine optimal shifts
3. **Pattern Matching**: Compares pattern from right to left
4. **Shift Calculation**: Uses maximum of both rules for optimal performance

## Time Complexity:
- **Best Case**: O(n/m) where n is text length, m is pattern length
- **Average Case**: O(n/m)
- **Worst Case**: O(n×m) but typically much better in practice

This implementation demonstrates the power of the Boyer-Moore algorithm in efficiently searching for patterns in text, which is particularly useful for large datasets.

