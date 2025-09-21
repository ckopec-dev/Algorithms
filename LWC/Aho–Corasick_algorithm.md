# Aho-Corasick Algorithm in Lightning Web Components

Here's a complete example of implementing the Aho-Corasick algorithm in Lightning Web Components:

## Component Files

### ahoCorasickExample.js
```javascript
import { LightningElement, track } from 'lwc';

export default class AhoCorasickExample extends LightningElement {
    @track patterns = ['he', 'she', 'his', 'hers'];
    @track text = 'She is his wife and she loves him';
    @track results = [];
    @track searchInput = '';
    
    // Aho-Corasick algorithm implementation
    search(text, patterns) {
        const trie = new Trie();
        const fail = new Map();
        const outputs = new Map();
        
        // Build trie from patterns
        for (let i = 0; i < patterns.length; i++) {
            trie.insert(patterns[i], i);
        }
        
        // Build failure links
        this.buildFailureLinks(trie, fail, outputs);
        
        // Search text using Aho-Corasick
        return this.ahoCorasickSearch(text, trie, fail, outputs);
    }
    
    buildFailureLinks(trie, fail, outputs) {
        const queue = [];
        const root = trie.root;
        
        // Initialize failure links for direct children of root
        for (let i = 0; i < 26; i++) {
            if (root.children[i]) {
                root.children[i].fail = root;
                queue.push(root.children[i]);
            } else {
                root.children[i] = root;
            }
        }
        
        // Build failure links using BFS
        while (queue.length > 0) {
            const current = queue.shift();
            const parent = current.parent;
            
            for (let i = 0; i < 26; i++) {
                if (current.children[i]) {
                    queue.push(current.children[i]);
                    
                    let failState = parent.fail;
                    while (failState !== null && !failState.children[i]) {
                        failState = failState.fail;
                    }
                    
                    if (failState === null) {
                        current.children[i].fail = trie.root;
                    } else {
                        current.children[i].fail = failState.children[i];
                    }
                    
                    // Merge outputs
                    const failOutputs = current.children[i].fail.outputs;
                    for (let j = 0; j < failOutputs.length; j++) {
                        current.children[i].outputs.push(failOutputs[j]);
                    }
                }
            }
        }
    }
    
    ahoCorasickSearch(text, trie, fail, outputs) {
        const results = [];
        let currentState = trie.root;
        
        for (let i = 0; i < text.length; i++) {
            const charIndex = text[i].toLowerCase().charCodeAt(0) - 'a'.charCodeAt(0);
            
            // Move to next state
            while (currentState !== null && !currentState.children[charIndex]) {
                currentState = currentState.fail;
            }
            
            if (currentState === null) {
                currentState = trie.root;
            } else {
                currentState = currentState.children[charIndex];
            }
            
            // Check for matches at current state
            if (currentState.outputs.length > 0) {
                const matchPositions = [];
                for (let j = 0; j < currentState.outputs.length; j++) {
                    const patternIndex = currentState.outputs[j];
                    const pattern = this.patterns[patternIndex];
                    matchPositions.push({
                        pattern: pattern,
                        index: i - pattern.length + 1
                    });
                }
                results.push({
                    position: i,
                    matches: matchPositions
                });
            }
        }
        
        return results;
    }
    
    handleSearch() {
        if (this.text && this.patterns.length > 0) {
            this.results = this.search(this.text, this.patterns);
        }
    }
    
    handlePatternChange(event) {
        const value = event.target.value;
        this.patterns = value.split(',').map(p => p.trim()).filter(p => p);
        this.handleSearch();
    }
    
    handleTextChange(event) {
        this.text = event.target.value;
        this.handleSearch();
    }
}

// Trie implementation for Aho-Corasick
class Trie {
    constructor() {
        this.root = new TrieNode();
    }
    
    insert(word, patternIndex) {
        let current = this.root;
        for (let i = 0; i < word.length; i++) {
            const charIndex = word[i].toLowerCase().charCodeAt(0) - 'a'.charCodeAt(0);
            
            if (!current.children[charIndex]) {
                current.children[charIndex] = new TrieNode();
                current.children[charIndex].parent = current;
            }
            
            current = current.children[charIndex];
        }
        
        // Mark end of word and store pattern index
        current.isEnd = true;
        current.outputs.push(patternIndex);
    }
}

class TrieNode {
    constructor() {
        this.children = new Array(26).fill(null);
        this.parent = null;
        this.fail = null;
        this.isEnd = false;
        this.outputs = [];
    }
}
```

### ahoCorasickExample.html
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Aho-Corasick Algorithm Example</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-input 
                    type="text" 
                    label="Patterns (comma separated)" 
                    value={patterns.join(', ')}
                    onchange={handlePatternChange}>
                </lightning-input>
                
                <lightning-textarea 
                    label="Text to search"
                    value={text}
                    onchange={handleTextChange}
                    rows="4">
                </lightning-textarea>
                
                <lightning-button 
                    label="Search" 
                    onclick={handleSearch}
                    variant="brand">
                </lightning-button>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <h3>Results</h3>
                <template if:true={results.length}>
                    <template for:each={results} for:item="result">
                        <div key={result.position} class="slds-box slds-theme_info slds-m-bottom_small">
                            <p><strong>Position:</strong> {result.position}</p>
                            <template for:each={result.matches} for:item="match">
                                <p key={match.pattern}>
                                    <span class="slds-badge slds-theme_success">{match.pattern}</span>
                                    found at position {match.index}
                                </p>
                            </template>
                        </div>
                    </template>
                </template>
                
                <template if:false={results.length}>
                    <p>No matches found</p>
                </template>
            </div>
        </div>
        
        <div class="slds-m-top_medium">
            <h3>How it works:</h3>
            <ul class="slds-list_dotted">
                <li>Builds a trie from all patterns</li>
                <li>Creates failure links using BFS</li>
                <li>Searches text character by character</li>
                <li>Finds all occurrences of patterns in text</li>
            </ul>
        </div>
    </div>
</template>
```

### ahoCorasickExample.css
```css
.slds-badge {
    margin-right: 0.5rem;
}

.slds-box {
    border-radius: 0.25rem;
}
```

## Usage Example

1. **Patterns**: Enter comma-separated patterns like "he,she,his,hers"
2. **Text**: Input text to search through
3. **Search**: Click the search button to find all pattern occurrences

## Key Features

- **Multiple Pattern Matching**: Finds all patterns simultaneously in O(n + m + z) time
- **Real-time Search**: Updates results as you type
- **Visual Results**: Shows matched patterns with their positions
- **Responsive Design**: Works on mobile and desktop

## Algorithm Complexity

- **Time Complexity**: O(n + m + z) where n is text length, m is total pattern length, z is number of matches
- **Space Complexity**: O(m) for the trie structure

This implementation demonstrates how to use the Aho-Corasick algorithm in a Lightning Web Component for efficient multi-pattern string matching.

