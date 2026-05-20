# Huffman Coding in Lightning Web Component

Here's a complete example of implementing Huffman coding algorithm in a Lightning Web Component:

```javascript
// huffmanCoding.js
import { LightningElement } from 'lwc';

export default class HuffmanCoding extends LightningElement {
    inputText = '';
    encodedText = '';
    decodedText = '';
    huffmanTree = null;
    encodingMap = new Map();
    isEncoded = false;

    handleInputChange(event) {
        this.inputText = event.target.value;
    }

    // Huffman Node class
    class HuffmanNode {
        constructor(char, frequency, left = null, right = null) {
            this.char = char;
            this.frequency = frequency;
            this.left = left;
            this.right = right;
        }
    }

    // Build frequency map
    buildFrequencyMap(text) {
        const frequencyMap = new Map();
        for (let char of text) {
            frequencyMap.set(char, (frequencyMap.get(char) || 0) + 1);
        }
        return frequencyMap;
    }

    // Build Huffman tree
    buildHuffmanTree(frequencyMap) {
        const priorityQueue = [];

        // Create leaf nodes for each character
        for (let [char, frequency] of frequencyMap) {
            priorityQueue.push(new this.HuffmanNode(char, frequency));
        }

        // Build tree by combining nodes
        while (priorityQueue.length > 1) {
            // Sort by frequency (ascending)
            priorityQueue.sort((a, b) => a.frequency - b.frequency);
            
            const left = priorityQueue.shift();
            const right = priorityQueue.shift();
            
            const mergedFrequency = left.frequency + right.frequency;
            const mergedNode = new this.HuffmanNode(null, mergedFrequency, left, right);
            
            priorityQueue.push(mergedNode);
        }

        return priorityQueue[0];
    }

    // Generate encoding map
    generateEncodingMap(root) {
        const encodingMap = new Map();
        
        if (!root) return encodingMap;
        
        const traverse = (node, code) => {
            if (node) {
                if (node.char !== null) {
                    encodingMap.set(node.char, code || '0');
                } else {
                    traverse(node.left, code + '0');
                    traverse(node.right, code + '1');
                }
            }
        };
        
        traverse(root, '');
        return encodingMap;
    }

    // Encode text
    encodeText(text, encodingMap) {
        let encoded = '';
        for (let char of text) {
            encoded += encodingMap.get(char);
        }
        return encoded;
    }

    // Decode text
    decodeText(encodedText, root) {
        if (!root) return '';
        
        let decoded = '';
        let currentNode = root;
        
        for (let bit of encodedText) {
            if (bit === '0') {
                currentNode = currentNode.left;
            } else {
                currentNode = currentNode.right;
            }
            
            if (currentNode.char !== null) {
                decoded += currentNode.char;
                currentNode = root;
            }
        }
        
        return decoded;
    }

    // Main encoding process
    handleEncode() {
        if (!this.inputText.trim()) {
            return;
        }

        const frequencyMap = this.buildFrequencyMap(this.inputText);
        this.huffmanTree = this.buildHuffmanTree(frequencyMap);
        this.encodingMap = this.generateEncodingMap(this.huffmanTree);
        this.encodedText = this.encodeText(this.inputText, this.encodingMap);
        this.isEncoded = true;
        
        // Display encoding map
        console.log('Encoding Map:');
        for (let [char, code] of this.encodingMap) {
            console.log(`'${char}': ${code}`);
        }
    }

    // Main decoding process
    handleDecode() {
        if (!this.encodedText || !this.huffmanTree) {
            return;
        }

        this.decodedText = this.decodeText(this.encodedText, this.huffmanTree);
    }

    // Reset all data
    handleReset() {
        this.inputText = '';
        this.encodedText = '';
        this.decodedText = '';
        this.huffmanTree = null;
        this.encodingMap.clear();
        this.isEncoded = false;
    }

    // Get statistics
    get statistics() {
        if (!this.inputText) return '';
        
        const originalSize = this.inputText.length * 8; // in bits
        const encodedSize = this.encodedText.length;
        const compressionRatio = ((originalSize - encodedSize) / originalSize * 100).toFixed(2);
        
        return `Original: ${originalSize} bits | Encoded: ${encodedSize} bits | Compression: ${compressionRatio}%`;
    }

    // Get encoding map for display
    get encodingMapDisplay() {
        const mapArray = [];
        for (let [char, code] of this.encodingMap) {
            mapArray.push({ character: char === ' ' ? 'SPACE' : char, code });
        }
        return mapArray;
    }
}
```

```html
<!-- huffmanCoding.html -->
<template>
    <div class="slds-box slds-box_small slds-m-around_medium">
        <h2>Huffman Coding Algorithm</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="inputText">Input Text</label>
            <div class="slds-form-element__control">
                <textarea 
                    id="inputText"
                    class="slds-textarea"
                    placeholder="Enter text to encode..."
                    value={inputText}
                    onchange={handleInputChange}
                    rows="3">
                </textarea>
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Encode" 
                variant="brand" 
                onclick={handleEncode}
                disabled={isEncoded}>
            </lightning-button>
            
            <lightning-button 
                label="Decode" 
                variant="success" 
                onclick={handleDecode}
                disabled={!isEncoded}>
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="destructive" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <template if:true={isEncoded}>
            <div class="slds-m-top_medium">
                <h3>Encoded Text</h3>
                <p class="slds-text-body_small">{encodedText}</p>
            </div>

            <div class="slds-m-top_medium">
                <h3>Decoded Text</h3>
                <p class="slds-text-body_small">{decodedText}</p>
            </div>

            <div class="slds-m-top_medium">
                <h3>Encoding Map</h3>
                <lightning-datatable
                    data={encodingMapDisplay}
                    columns={columns}
                    key-field="character"
                    hide-checkbox-column="true">
                </lightning-datatable>
            </div>

            <div class="slds-m-top_medium">
                <h3>Statistics</h3>
                <p class="slds-text-body_small">{statistics}</p>
            </div>
        </template>
    </div>
</template>
```

```javascript
// huffmanCoding.js (additional methods for LWC)
import { LightningElement, track } from 'lwc';

export default class HuffmanCoding extends LightningElement {
    // ... [previous code remains the same] ...

    get columns() {
        return [
            { label: 'Character', fieldName: 'character', type: 'text' },
            { label: 'Code', fieldName: 'code', type: 'text' }
        ];
    }

    // Additional utility methods
    getTreeAsString(node, prefix = '', isLast = true) {
        if (!node) return '';
        
        let result = prefix + (isLast ? '└── ' : '├── ') + 
                    (node.char === null ? 'internal' : `'${node.char}'`) + 
                    ` (${node.frequency})\n`;
        
        if (node.left || node.right) {
            if (node.left) {
                result += this.getTreeAsString(node.left, prefix + (isLast ? '    ' : '│   '), !node.right);
            }
            if (node.right) {
                result += this.getTreeAsString(node.right, prefix + (isLast ? '    ' : '│   '), true);
            }
        }
        
        return result;
    }
}
```

## Key Features of this Implementation:

1. **Huffman Tree Construction**: Builds optimal binary tree based on character frequencies
2. **Encoding/Decoding**: Converts text to compressed binary format and back
3. **Interactive UI**: Real-time encoding and decoding with reset functionality
4. **Statistics**: Shows compression ratio and bit savings
5. **Encoding Map Display**: Visualizes character-to-code mapping
6. **Error Handling**: Handles empty inputs gracefully

## How it Works:

1. **Frequency Analysis**: Counts character occurrences in input text
2. **Tree Building**: Creates Huffman tree by combining lowest frequency nodes
3. **Code Generation**: Assigns binary codes based on tree traversal
4. **Compression**: Converts text to binary using generated codes
5. **Decompression**: Reconstructs original text from encoded binary

This implementation demonstrates the core principles of Huffman coding in a user-friendly Lightning Web Component interface.

