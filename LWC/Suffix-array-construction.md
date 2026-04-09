# Suffix Array Construction in Lightning Web Component

Here's a complete example of a suffix array construction algorithm implemented in Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class SuffixArrayComponent extends LightningElement {
    inputText = 'banana';
    suffixArray = [];
    suffixArrayString = '';
    sortedSuffixes = [];
    isProcessing = false;

    handleInputChange(event) {
        this.inputText = event.target.value;
        this.clearResults();
    }

    handleBuildSuffixArray() {
        this.isProcessing = true;
        this.clearResults();
        
        // Simulate processing delay
        setTimeout(() => {
            this.buildSuffixArray();
            this.isProcessing = false;
        }, 500);
    }

    buildSuffixArray() {
        const text = this.inputText;
        if (!text) return;

        // Step 1: Generate all suffixes
        const suffixes = [];
        for (let i = 0; i < text.length; i++) {
            suffixes.push({
                index: i,
                suffix: text.substring(i),
                originalIndex: i
            });
        }

        // Step 2: Sort suffixes lexicographically
        suffixes.sort((a, b) => {
            if (a.suffix < b.suffix) return -1;
            if (a.suffix > b.suffix) return 1;
            return 0;
        });

        // Step 3: Extract suffix array (starting positions)
        this.suffixArray = suffixes.map(suffix => suffix.originalIndex);
        this.suffixArrayString = this.suffixArray.join(', ');
        
        // Step 4: Store sorted suffixes for display
        this.sortedSuffixes = suffixes.map(suffix => ({
            suffix: suffix.suffix,
            index: suffix.originalIndex
        }));
    }

    clearResults() {
        this.suffixArray = [];
        this.suffixArrayString = '';
        this.sortedSuffixes = [];
    }

    get displaySuffixes() {
        return this.sortedSuffixes.map((suffix, index) => ({
            id: index,
            suffix: suffix.suffix,
            index: suffix.index
        }));
    }

    get hasResults() {
        return this.suffixArray.length > 0;
    }
}
```

```html
<template>
    <div class="suffix-array-container">
        <lightning-card title="Suffix Array Construction" icon-name="custom:custom11">
            <div class="slds-m-around_medium">
                <div class="slds-form-element">
                    <label class="slds-form-element__label" for="inputText">Input Text</label>
                    <div class="slds-form-element__control">
                        <input 
                            type="text" 
                            id="inputText"
                            class="slds-input"
                            value={inputText}
                            onchange={handleInputChange}
                            placeholder="Enter text to build suffix array"
                        />
                    </div>
                </div>

                <lightning-button 
                    label="Build Suffix Array" 
                    variant="brand"
                    onclick={handleBuildSuffixArray}
                    disabled={isProcessing}
                    class="slds-m-top_small"
                >
                </lightning-button>

                <template if:true={isProcessing}>
                    <div class="slds-m-top_small">
                        <lightning-spinner alternative-text="Building suffix array"></lightning-spinner>
                        <p>Processing...</p>
                    </div>
                </template>

                <template if:true={hasResults}>
                    <div class="slds-m-top_small">
                        <h3>Suffix Array</h3>
                        <p>Starting positions: {suffixArrayString}</p>
                        
                        <h3>Sorted Suffixes</h3>
                        <lightning-datatable
                            data={displaySuffixes}
                            columns={columns}
                            key-field="id"
                            class="slds-m-top_small"
                        >
                        </lightning-datatable>
                    </div>
                </template>
            </div>
        </lightning-card>
    </div>
</template>
```

```javascript
// Column definitions for the data table
get columns() {
    return [
        {
            label: 'Suffix',
            fieldName: 'suffix',
            type: 'text',
            wrapText: true
        },
        {
            label: 'Starting Index',
            fieldName: 'index',
            type: 'number'
        }
    ];
}
```

```css
.suffix-array-container {
    max-width: 800px;
    margin: 0 auto;
}

.slds-form-element__label {
    font-weight: bold;
}

.slds-input {
    width: 100%;
}
```

## Algorithm Explanation

This implementation demonstrates the suffix array construction algorithm with these key steps:

1. **Suffix Generation**: Creates all possible suffixes of the input text
2. **Lexicographic Sorting**: Sorts the suffixes in dictionary order
3. **Array Construction**: Extracts the starting positions to form the suffix array

## Example Output

For input text "banana":
- Suffixes: ["banana", "anana", "nana", "ana", "na", "a"]
- Sorted suffixes: ["a", "ana", "anana", "banana", "na", "nana"]
- Suffix Array: [5, 3, 1, 0, 4, 2]

The algorithm has O(n²logn) time complexity due to the sorting step, where n is the length of the input string.

