# Binary Search Tree in Lightning Web Component

```javascript
// bst.js - Lightning Web Component JavaScript file
import { LightningElement } from 'lwc';

export default class BinarySearchTree extends LightningElement {
    root = null;
    treeData = [];

    // Node class for BST
    class Node {
        constructor(value) {
            this.value = value;
            this.left = null;
            this.right = null;
        }
    }

    // Insert method for BST
    insert(value) {
        const newNode = new this.Node(value);
        
        if (this.root === null) {
            this.root = newNode;
        } else {
            this.insertNode(this.root, newNode);
        }
    }

    // Helper method to insert node
    insertNode(node, newNode) {
        if (newNode.value < node.value) {
            if (node.left === null) {
                node.left = newNode;
            } else {
                this.insertNode(node.left, newNode);
            }
        } else {
            if (node.right === null) {
                node.right = newNode;
            } else {
                this.insertNode(node.right, newNode);
            }
        }
    }

    // Search method for BST
    search(value) {
        return this.searchNode(this.root, value);
    }

    // Helper method to search node
    searchNode(node, value) {
        if (node === null) {
            return false;
        }

        if (value < node.value) {
            return this.searchNode(node.left, value);
        } else if (value > node.value) {
            return this.searchNode(node.right, value);
        } else {
            return true;
        }
    }

    // Inorder traversal (for display)
    inorder(node = this.root) {
        if (node !== null) {
            this.inorder(node.left);
            this.treeData.push(node.value);
            this.inorder(node.right);
        }
    }

    // Get all values in sorted order
    getSortedValues() {
        this.treeData = [];
        this.inorder();
        return this.treeData;
    }

    // Example usage
    connectedCallback() {
        // Insert sample data
        const sampleData = [50, 30, 70, 20, 40, 60, 80];
        sampleData.forEach(value => {
            this.insert(value);
        });
        
        console.log('BST created with values:', sampleData);
        console.log('Sorted values:', this.getSortedValues());
        console.log('Search 40:', this.search(40)); // true
        console.log('Search 25:', this.search(25)); // false
    }

    // Method to find minimum value
    findMin() {
        if (this.root === null) return null;
        
        let current = this.root;
        while (current.left !== null) {
            current = current.left;
        }
        return current.value;
    }

    // Method to find maximum value
    findMax() {
        if (this.root === null) return null;
        
        let current = this.root;
        while (current.right !== null) {
            current = current.right;
        }
        return current.value;
    }
}
```

```html
<!-- bst.html - Lightning Web Component HTML file -->
<template>
    <div class="bst-container">
        <h2>Binary Search Tree Implementation</h2>
        
        <div class="tree-info">
            <p>Root: {rootValue}</p>
            <p>Minimum Value: {minValue}</p>
            <p>Maximum Value: {maxValue}</p>
        </div>

        <div class="tree-display">
            <h3>Sorted Tree Values:</h3>
            <ul>
                <template for:each={sortedValues} for:item="value">
                    <li key={value}>{value}</li>
                </template>
            </ul>
        </div>

        <div class="search-section">
            <h3>Search Value:</h3>
            <lightning-input 
                type="number" 
                label="Enter value to search"
                value={searchValue}
                onchange={handleSearchValueChange}
                variant="standard">
            </lightning-input>
            <lightning-button 
                label="Search" 
                onclick={handleSearch}
                variant="brand">
            </lightning-button>
            <p if:true={searchResult}>Value found: {searchResult}</p>
        </div>
    </div>
</template>
```

```css
/* bst.css - Lightning Web Component CSS file */
.bst-container {
    padding: 20px;
    border: 1px solid #e1e1e1;
    border-radius: 8px;
    margin: 20px;
}

.tree-info {
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.tree-display {
    margin: 20px 0;
}

.tree-display ul {
    list-style-type: none;
    padding: 0;
}

.tree-display li {
    padding: 5px 10px;
    margin: 5px 0;
    background-color: #e9ecef;
    border-radius: 3px;
}

.search-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #f8f9fa;
    border-radius: 5px;
}

.search-section lightning-button {
    margin-top: 10px;
}
```

## Key Features of this BST Implementation:

1. **Node Class**: Represents individual nodes with value, left, and right pointers
2. **Insert Method**: Maintains BST property (left < parent < right)
3. **Search Method**: Efficient O(log n) search using BST property
4. **Inorder Traversal**: Returns values in sorted order
5. **Min/Max Finding**: Efficient methods to find extreme values
6. **Interactive UI**: Search functionality with user input

## Usage Example:
- Insert values: 50, 30, 70, 20, 40, 60, 80
- Search for existing value (40) - returns true
- Search for non-existing value (25) - returns false
- Display sorted values in ascending order

This implementation demonstrates the core BST operations while following Lightning Web Component best practices.

