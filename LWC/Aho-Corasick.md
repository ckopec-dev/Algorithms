# Aho-Corasick Algorithm in Lightning Web Component

Here's a complete example of implementing the Aho-Corasick algorithm in a Lightning Web Component:

```javascript
// ahoCorasick.js
export default class AhoCorasick {
    constructor() {
        this.root = {};
        this.fail = new Map();
        this.output = new Map();
    }

    // Insert a pattern into the trie
    insert(pattern, id) {
        let node = this.root;
        for (let char of pattern) {
            if (!node[char]) {
                node[char] = {};
            }
            node = node[char];
        }
        if (!node.patterns) {
            node.patterns = [];
        }
        node.patterns.push(id);
    }

    // Build failure links
    buildFailureLinks() {
        let queue = [];
        for (let char in this.root) {
            if (this.root.hasOwnProperty(char)) {
                this.fail.set(this.root[char], this.root);
                queue.push(this.root[char]);
            }
        }

        while (queue.length > 0) {
            let node = queue.shift();
            for (let char in node) {
                if (node.hasOwnProperty(char) && char !== 'patterns') {
                    let child = node[char];
                    let failState = this.fail.get(node);
                    while (failState !== this.root && !failState[char]) {
                        failState = this.fail.get(failState);
                    }
                    if (failState[char]) {
                        this.fail.set(child, failState[char]);
                    } else {
                        this.fail.set(child, this.root);
                    }
                    queue.push(child);
                }
            }
        }
    }

    // Search for patterns in text
    search(text) {
        let node = this.root;
        let results = [];
        
        for (let i = 0; i < text.length; i++) {
            let char = text[i];
            
            while (node !== this.root && !node[char]) {
                node = this.fail.get(node);
            }
            
            if (node[char]) {
                node = node[char];
                
                // Check if we found a pattern
                if (node.patterns) {
                    for (let patternId of node.patterns) {
                        results.push({
                            patternId: patternId,
                            index: i - patternId.length + 1,
                            length: patternId.length
                        });
                    }
                }
            }
        }
        
        return results;
    }
}
```

```javascript
// searchComponent.js
import { LightningElement, track } from 'lwc';
import AhoCorasick from './ahoCorasick';

export default class SearchComponent extends LightningElement {
    @track textToSearch = '';
    @track patterns = ['hello', 'world', 'test'];
    @track searchResults = [];
    @track patternInput = '';
    
    ahoCorasick = new AhoCorasick();

    connectedCallback() {
        this.buildTrie();
    }

    buildTrie() {
        // Clear existing trie
        this.ahoCorasick = new AhoCorasick();
        
        // Insert all patterns
        this.patterns.forEach((pattern, index) => {
            this.ahoCorasick.insert(pattern, index);
        });
        
        // Build failure links
        this.ahoCorasick.buildFailureLinks();
    }

    handleTextChange(event) {
        this.textToSearch = event.target.value;
        this.performSearch();
    }

    handlePatternChange(event) {
        this.patternInput = event.target.value;
    }

    addPattern() {
        if (this.patternInput.trim() && !this.patterns.includes(this.patternInput.trim())) {
            this.patterns = [...this.patterns, this.patternInput.trim()];
            this.patternInput = '';
            this.buildTrie();
            this.performSearch();
        }
    }

    removePattern(index) {
        this.patterns = this.patterns.filter((_, i) => i !== index);
        this.buildTrie();
        this.performSearch();
    }

    performSearch() {
        if (this.textToSearch && this.patterns.length > 0) {
            this.searchResults = this.ahoCorasick.search(this.textToSearch);
        } else {
            this.searchResults = [];
        }
    }

    get highlightedText() {
        if (!this.textToSearch || this.searchResults.length === 0) {
            return this.textToSearch;
        }

        let result = '';
        let lastEnd = 0;

        // Sort results by start position
        const sortedResults = [...this.searchResults].sort((a, b) => a.index - b.index);

        for (let i = 0; i < sortedResults.length; i++) {
            const match = sortedResults[i];
            const start = match.index;
            const end = start + match.length;
            
            // Add text before match
            result += this.textToSearch.substring(lastEnd, start);
            
            // Add highlighted match
            result += `<mark class="highlight">${this.textToSearch.substring(start, end)}</mark>`;
            
            lastEnd = end;
        }

        // Add remaining text
        result += this.textToSearch.substring(lastEnd);
        
        return result;
    }

    get patternList() {
        return this.patterns.map((pattern, index) => ({
            id: index,
            value: pattern
        }));
    }
}
```

```html
<!-- searchComponent.html -->
<template>
    <div class="slds-box slds-p-around_medium">
        <h2 class="slds-text-heading_small">Aho-Corasick Pattern Search</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Text to Search</label>
            <div class="slds-form-element__control">
                <textarea 
                    class="slds-textarea" 
                    value={textToSearch} 
                    onchange={handleTextChange}
                    placeholder="Enter text to search for patterns..."
                    rows="4">
                </textarea>
            </div>
        </div>

        <div class="slds-form-element slds-m-top_medium">
            <label class="slds-form-element__label">Patterns</label>
            <div class="slds-form-element__control">
                <div class="slds-input-has-icon slds-input-has-icon_right">
                    <input 
                        type="text" 
                        class="slds-input"
                        value={patternInput}
                        onchange={handlePatternChange}
                        placeholder="Enter pattern to add..."
                    />
                    <button 
                        class="slds-button slds-button_icon slds-input__icon slds-input__icon_right"
                        onclick={addPattern}
                        title="Add Pattern">
                        <svg class="slds-button__icon" aria-hidden="true">
                            <use xlink:href="/assets/icons/utility-sprite/svg/symbols.svg#add"></use>
                        </svg>
                    </button>
                </div>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <template for:each={patternList} for:item="pattern">
                <div key={pattern.id} class="slds-badge slds-m-around_x-small">
                    {pattern.value}
                    <button 
                        class="slds-button slds-button_icon slds-button_icon-small slds-button_icon-border-filled"
                        onclick={removePattern}
                        data-index={pattern.id}
                        title="Remove Pattern">
                        <svg class="slds-button__icon" aria-hidden="true">
                            <use xlink:href="/assets/icons/utility-sprite/svg/symbols.svg#close"></use>
                        </svg>
                    </button>
                </div>
            </template>
        </div>

        <template if:true={searchResults.length}>
            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Search Results ({searchResults.length})</h3>
                <div class="slds-box slds-box_x-small slds-p-around_small">
                    <template for:each={searchResults} for:item="result">
                        <div key={result.index} class="slds-m-around_x-small">
                            <span class="slds-badge">
                                Pattern: {patterns[result.patternId]}
                            </span>
                            <span class="slds-badge slds-m-left_x-small">
                                Position: {result.index}
                            </span>
                            <span class="slds-badge slds-m-left_x-small">
                                Length: {result.length}
                            </span>
                        </div>
                    </template>
                </div>
            </div>
        </template>

        <div class="slds-m-top_medium">
            <h3 class="slds-text-heading_small">Highlighted Text</h3>
            <div class="slds-box slds-box_x-small slds-p-around_small slds-text_body_small">
                <div 
                    class="highlighted-text"
                    innerHTML={highlightedText}
                ></div>
            </div>
        </div>
    </div>
</template>
```

```css
/* searchComponent.css */
.highlighted-text {
    font-family: monospace;
    white-space: pre-wrap;
    word-break: break-word;
}

.highlight {
    background-color: yellow;
    font-weight: bold;
}

.slds-badge {
    margin: 0.25rem;
}
```

## Usage Example

1. **Component Registration**: Create a new Lightning Web Component named `searchComponent`
2. **Add the three files** (`searchComponent.js`, `searchComponent.html`, `searchComponent.css`) to your component
3. **Deploy** the component to your Salesforce org
4. **Use** the component in any Lightning page

## Features

- **Real-time Pattern Matching**: Finds all occurrences of multiple patterns
- **Dynamic Pattern Management**: Add/remove patterns at runtime
- **Visual Highlighting**: Highlights matched patterns in the text
- **Detailed Results**: Shows position and length of each match
- **Efficient Search**: Uses Aho-Corasick algorithm for optimal performance

## How It Works

1. **Trie Construction**: Builds a trie structure from all patterns
2. **Failure Links**: Creates failure links for efficient backtracking
3. **Pattern Matching**: Searches text using the constructed automaton
4. **Result Presentation**: Displays matches with positions and highlighting

This implementation demonstrates the power of the Aho-Corasick algorithm for multi-pattern string searching in a Lightning Web Component environment.

