# Knuth-Morris-Pratt (KMP) Algorithm in Lightning Web Component

Here's a complete example of implementing the KMP algorithm in a Lightning Web Component:

## HTML Template (kmpExample.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_medium">KMP Algorithm Demo</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-input 
                    label="Text to search in" 
                    value={text} 
                    onchange={handleTextChange}
                    variant="standard">
                </lightning-input>
            </div>
            <div class="slds-col slds-size_1-of-2">
                <lightning-input 
                    label="Pattern to search for" 
                    value={pattern} 
                    onchange={handlePatternChange}
                    variant="standard">
                </lightning-input>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Search Pattern" 
                onclick={searchPattern}
                variant="brand">
            </lightning-button>
        </div>

        <div class="slds-m-top_medium">
            <template if:true={searchResult}>
                <div class="slds-alert slds-alert_success" role="alert">
                    <span class="slds-assistive-text">Success</span>
                    <h2 class="slds-text-heading_small">Pattern found at position: {searchResult}</h2>
                </div>
            </template>
            
            <template if:true={searchResult === 0}>
                <div class="slds-alert slds-alert_success" role="alert">
                    <span class="slds-assistive-text">Success</span>
                    <h2 class="slds-text-heading_small">Pattern found at position: {searchResult}</h2>
                </div>
            </template>

            <template if:true={searchResult === -1}>
                <div class="slds-alert slds-alert_error" role="alert">
                    <span class="slds-assistive-text">Error</span>
                    <h2 class="slds-text-heading_small">Pattern not found in text</h2>
                </div>
            </template>

            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">KMP Preprocessing (LPS Array)</h3>
                <lightning-datatable
                    data={lpsData}
                    columns={lpsColumns}
                    hide-checkbox-column="true"
                    max-row-selection="1"
                    key-field="id">
                </lightning-datatable>
            </div>

            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Search Process</h3>
                <lightning-datatable
                    data={searchProcessData}
                    columns={searchProcessColumns}
                    hide-checkbox-column="true"
                    max-row-selection="1"
                    key-field="step">
                </lightning-datatable>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (kmpExample.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class KmpExample extends LightningElement {
    @track text = 'ABABDABACDABABCABCABCABCABC';
    @track pattern = 'ABABCABCABCABC';
    @track searchResult = -1;
    @track lpsData = [];
    @track searchProcessData = [];
    
    // Column definitions for LPS table
    lpsColumns = [
        { label: 'Index', fieldName: 'index', type: 'number' },
        { label: 'Character', fieldName: 'character', type: 'text' },
        { label: 'LPS Value', fieldName: 'lpsValue', type: 'number' }
    ];

    // Column definitions for search process table
    searchProcessColumns = [
        { label: 'Step', fieldName: 'step', type: 'number' },
        { label: 'Text Position', fieldName: 'textPos', type: 'number' },
        { label: 'Pattern Position', fieldName: 'patternPos', type: 'number' },
        { label: 'Action', fieldName: 'action', type: 'text' }
    ];

    handleTextChange(event) {
        this.text = event.target.value;
    }

    handlePatternChange(event) {
        this.pattern = event.target.value;
    }

    // KMP Algorithm implementation
    kmpSearch(text, pattern) {
        const n = text.length;
        const m = pattern.length;
        
        // Reset process data
        this.searchProcessData = [];
        
        // Preprocessing: Build LPS (Longest Proper Prefix which is also Suffix) array
        const lps = this.computeLPSArray(pattern);
        
        // Store LPS data for display
        this.lpsData = lps.map((value, index) => ({
            id: index,
            index: index,
            character: pattern[index],
            lpsValue: value
        }));

        let i = 0; // index for text
        let j = 0; // index for pattern
        
        // Add initial step
        this.searchProcessData.push({
            step: 0,
            textPos: i,
            patternPos: j,
            action: 'Start search'
        });

        while (i < n) {
            if (pattern[j] === text[i]) {
                i++;
                j++;
                
                this.searchProcessData.push({
                    step: this.searchProcessData.length,
                    textPos: i,
                    patternPos: j,
                    action: `Match: text[${i-1}] = pattern[${j-1}]`
                });
                
                // If we've found the complete pattern
                if (j === m) {
                    this.searchProcessData.push({
                        step: this.searchProcessData.length,
                        textPos: i,
                        patternPos: j,
                        action: `Pattern found at position ${i - j}`
                    });
                    return i - j; // Return the starting position
                }
            } else {
                // Mismatch after some matches
                if (j !== 0) {
                    // Use previously computed LPS value to skip characters
                    j = lps[j - 1];
                    
                    this.searchProcessData.push({
                        step: this.searchProcessData.length,
                        textPos: i,
                        patternPos: j,
                        action: `Mismatch: Backtrack pattern to position ${j}`
                    });
                } else {
                    // If j is 0, move to next character in text
                    i++;
                    
                    this.searchProcessData.push({
                        step: this.searchProcessData.length,
                        textPos: i,
                        patternPos: j,
                        action: `Mismatch: Move to next text character`
                    });
                }
            }
        }
        
        // Pattern not found
        this.searchProcessData.push({
            step: this.searchProcessData.length,
            textPos: i,
            patternPos: j,
            action: 'Pattern not found'
        });
        
        return -1;
    }

    // Compute LPS (Longest Proper Prefix which is also Suffix) array
    computeLPSArray(pattern) {
        const m = pattern.length;
        const lps = new Array(m);
        lps[0] = 0; // First element is always 0
        
        let len = 0; // Length of previous longest prefix suffix
        let i = 1;
        
        while (i < m) {
            if (pattern[i] === pattern[len]) {
                len++;
                lps[i] = len;
                i++;
            } else {
                if (len !== 0) {
                    // This is tricky - consider the previous lps value
                    len = lps[len - 1];
                } else {
                    lps[i] = 0;
                    i++;
                }
            }
        }
        
        return lps;
    }

    searchPattern() {
        if (!this.text || !this.pattern) {
            this.searchResult = -1;
            return;
        }
        
        this.searchResult = this.kmpSearch(this.text, this.pattern);
    }
}
```

## CSS Styles (kmpExample.css)
```css
.slds-box {
    padding: 1rem;
}

.slds-alert {
    margin-top: 1rem;
}

.slds-text-heading_small {
    margin-bottom: 0.5rem;
}

.slds-grid {
    margin-bottom: 1rem;
}
```

## Key Features of this Implementation:

1. **Complete KMP Algorithm**: Implements both preprocessing (LPS array) and search phases
2. **Visual Process Tracking**: Shows step-by-step search process in a table
3. **LPS Array Visualization**: Displays the computed LPS array for the pattern
4. **Interactive UI**: Allows users to input different text and pattern strings
5. **Clear Results**: Shows whether pattern is found and at which position
6. **Educational Value**: Visualizes how KMP algorithm efficiently skips characters

## How KMP Works:

1. **Preprocessing Phase**: Build LPS (Longest Proper Prefix which is also Suffix) array
2. **Search Phase**: Use LPS array to avoid unnecessary character comparisons
3. **Efficiency**: Time complexity O(n + m) where n is text length and m is pattern length

This implementation demonstrates the power of KMP algorithm in efficiently searching for patterns in text, especially when dealing with repeated patterns.

