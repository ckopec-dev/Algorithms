# Bitap Algorithm in Lightning Web Component

Here's an example implementation of the Bitap algorithm (also known as the Shift-Or algorithm) in a Lightning Web Component:

```javascript
// bitap.js
import { LightningElement } from 'lwc';

export default class BitapSearch extends LightningElement {
    searchTerm = '';
    textToSearch = '';
    searchResults = [];
    isSearching = false;

    handleSearchTermChange(event) {
        this.searchTerm = event.target.value;
        this.performSearch();
    }

    handleTextChange(event) {
        this.textToSearch = event.target.value;
        this.performSearch();
    }

    performSearch() {
        if (!this.searchTerm || !this.textToSearch) {
            this.searchResults = [];
            return;
        }

        this.isSearching = true;
        // Simulate async operation
        setTimeout(() => {
            const results = this.bitapSearch(this.textToSearch, this.searchTerm);
            this.searchResults = results;
            this.isSearching = false;
        }, 10);
    }

    /**
     * Bitap algorithm implementation
     * @param {string} text - Text to search in
     * @param {string} pattern - Pattern to search for
     * @returns {Array} Array of match objects with positions
     */
    bitapSearch(text, pattern) {
        if (!pattern || !text) return [];

        const m = pattern.length;
        const n = text.length;
        
        // Create bitap table
        const bitapTable = this.createBitapTable(pattern);
        
        // Initialize bitap state
        let state = (1 << m) - 1;
        const results = [];

        // Search through text
        for (let i = 0; i < n; i++) {
            // Update state based on current character
            const charIndex = this.getCharIndex(pattern, text[i]);
            state = ((state << 1) | 1) & bitapTable[charIndex];
            
            // Check if we found a match
            if ((state & (1 << (m - 1))) === 0) {
                results.push({
                    position: i - m + 1,
                    text: text.substring(i - m + 1, i + 1),
                    match: true
                });
            }
        }

        return results;
    }

    /**
     * Create bitap table for pattern
     * @param {string} pattern - Search pattern
     * @returns {Array} Bitap table
     */
    createBitapTable(pattern) {
        const table = new Array(256).fill(0); // ASCII table
        const m = pattern.length;

        // For each character in pattern, set corresponding bit
        for (let i = 0; i < m; i++) {
            const charCode = pattern.charCodeAt(i);
            table[charCode] |= (1 << i);
        }

        return table;
    }

    /**
     * Get character index for bitap table
     * @param {string} pattern - Search pattern
     * @param {string} char - Character to find
     * @returns {number} Character index
     */
    getCharIndex(pattern, char) {
        const charCode = char.charCodeAt(0);
        return charCode;
    }

    get hasResults() {
        return this.searchResults.length > 0;
    }

    get noResults() {
        return this.searchResults.length === 0 && this.searchTerm && this.textToSearch;
    }
}
```

```html
<!-- bitap.html -->
<template>
    <div class="slds-box slds-m-around_medium">
        <h2>Bitap Algorithm Search</h2>
        
        <div class="slds-form-element slds-m-bottom_medium">
            <label class="slds-form-element__label">Search Term</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={searchTerm}
                    onchange={handleSearchTermChange}
                    placeholder="Enter search term"
                />
            </div>
        </div>

        <div class="slds-form-element slds-m-bottom_medium">
            <label class="slds-form-element__label">Text to Search</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea"
                    value={textToSearch}
                    onchange={handleTextChange}
                    placeholder="Enter text to search in"
                    rows="5"
                ></textarea>
            </div>
        </div>

        <template if:true={isSearching}>
            <div class="slds-spinner_container">
                <div class="slds-spinner slds-spinner_large" role="status">
                    <span class="slds-assistive-text">Loading</span>
                </div>
            </div>
        </template>

        <template if:true={hasResults}>
            <div class="slds-m-top_medium">
                <h3>Search Results ({searchResults.length})</h3>
                <template for:each={searchResults} for:item="result">
                    <div key={result.position} class="slds-card slds-m-bottom_small">
                        <div class="slds-card__header slds-grid">
                            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                                <div class="slds-media__body">
                                    <h3 class="slds-card__header-title">Match at position {result.position}</h3>
                                </div>
                            </header>
                        </div>
                        <div class="slds-card__body">
                            <p>Found: <strong>{result.text}</strong></p>
                        </div>
                    </div>
                </template>
            </div>
        </template>

        <template if:true={noResults}>
            <div class="slds-notify slds-notify_alert slds-theme_alert-texture slds-theme_error">
                <span class="slds-assistive-text">Error</span>
                <p>No matches found</p>
            </div>
        </template>
    </div>
</template>
```

```css
/* bitap.css */
.slds-card__header-title {
    font-weight: 600;
}

.slds-spinner_container {
    text-align: center;
    padding: 2rem;
}
```

## Key Features of This Implementation:

1. **Bitap Algorithm**: Implements the Shift-Or algorithm for efficient pattern matching
2. **Real-time Search**: Updates results as user types
3. **Visual Feedback**: Shows loading state and results
4. **Error Handling**: Handles empty inputs gracefully
5. **Lightning Design System**: Uses SLDS components for consistent styling

## How It Works:

1. **Bitap Table Creation**: Builds a lookup table for each character in the pattern
2. **State Tracking**: Maintains a bit state that shifts with each character processed
3. **Match Detection**: When the most significant bit becomes 0, a match is found
4. **Position Tracking**: Records the position where matches occur

## Usage Example:

```
Pattern: "abc"
Text: "abcdefabcxyz"
Results: Match at position 0, Match at position 6
```

This implementation demonstrates how the Bitap algorithm can efficiently find pattern matches in text with O(n*m) time complexity, where n is the text length and m is the pattern length.

