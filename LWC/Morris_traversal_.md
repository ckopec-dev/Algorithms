# Morris Traversal in Lightning Web Component

Here's an example of implementing Morris traversal algorithm in a Lightning Web Component:

```javascript
// morrisTraversal.js
import { LightningElement, track } from 'lwc';

export default class MorrisTraversal extends LightningElement {
    @track treeData = [];
    @track traversalResult = [];
    @track isTraversing = false;

    connectedCallback() {
        // Initialize sample binary tree data
        this.treeData = [
            { id: 1, value: 1, left: 2, right: 3 },
            { id: 2, value: 2, left: 4, right: 5 },
            { id: 3, value: 3, left: null, right: null },
            { id: 4, value: 4, left: null, right: null },
            { id: 5, value: 5, left: null, right: null }
        ];
    }

    // Morris Inorder Traversal Implementation
    morrisInorderTraversal() {
        this.isTraversing = true;
        this.traversalResult = [];
        
        let current = this.treeData[0]; // Start with root node (id = 1)
        let result = [];
        
        while (current !== null) {
            if (current.left === null) {
                // Process current node
                result.push(current.value);
                current = this.getNodeById(current.right);
            } else {
                // Find inorder predecessor
                let predecessor = this.getNodeById(current.left);
                while (predecessor.right !== null && predecessor.right !== current.id) {
                    predecessor = this.getNodeById(predecessor.right);
                }
                
                if (predecessor.right === null) {
                    // Make current the right child of predecessor
                    predecessor.right = current.id;
                    current = this.getNodeById(current.left);
                } else {
                    // Revert the changes
                    predecessor.right = null;
                    result.push(current.value);
                    current = this.getNodeById(current.right);
                }
            }
        }
        
        this.traversalResult = result;
        this.isTraversing = false;
    }

    // Morris Preorder Traversal Implementation
    morrisPreorderTraversal() {
        this.isTraversing = true;
        this.traversalResult = [];
        
        let current = this.treeData[0]; // Start with root node (id = 1)
        let result = [];
        
        while (current !== null) {
            if (current.left === null) {
                // Process current node
                result.push(current.value);
                current = this.getNodeById(current.right);
            } else {
                // Find inorder predecessor
                let predecessor = this.getNodeById(current.left);
                while (predecessor.right !== null && predecessor.right !== current.id) {
                    predecessor = this.getNodeById(predecessor.right);
                }
                
                if (predecessor.right === null) {
                    // Process current node before going left
                    result.push(current.value);
                    predecessor.right = current.id;
                    current = this.getNodeById(current.left);
                } else {
                    // Revert the changes
                    predecessor.right = null;
                    current = this.getNodeById(current.right);
                }
            }
        }
        
        this.traversalResult = result;
        this.isTraversing = false;
    }

    // Helper method to get node by ID
    getNodeById(id) {
        if (id === null) return null;
        return this.treeData.find(node => node.id === id);
    }

    // Handle traversal button clicks
    handleInorderTraversal() {
        this.morrisInorderTraversal();
    }

    handlePreorderTraversal() {
        this.morrisPreorderTraversal();
    }

    handleReset() {
        this.traversalResult = [];
    }
}
```

```html
<!-- morrisTraversal.html -->
<template>
    <div class="container">
        <h2>Morris Traversal Algorithm</h2>
        
        <div class="tree-info">
            <h3>Binary Tree Structure</h3>
            <p>Tree represented as nodes with ID, value, left child, and right child</p>
            <ul>
                <li>Node 1 (Root): Value = 1, Left = 2, Right = 3</li>
                <li>Node 2: Value = 2, Left = 4, Right = 5</li>
                <li>Node 3: Value = 3, Left = null, Right = null</li>
                <li>Node 4: Value = 4, Left = null, Right = null</li>
                <li>Node 5: Value = 5, Left = null, Right = null</li>
            </ul>
        </div>

        <div class="controls">
            <lightning-button 
                label="Morris Inorder Traversal" 
                onclick={handleInorderTraversal}
                disabled={isTraversing}
                variant="brand">
            </lightning-button>
            
            <lightning-button 
                label="Morris Preorder Traversal" 
                onclick={handlePreorderTraversal}
                disabled={isTraversing}
                variant="brand">
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                onclick={handleReset}
                variant="neutral">
            </lightning-button>
        </div>

        <div class="result">
            <h3>Traversal Result</h3>
            <template if:true={isTraversing}>
                <lightning-spinner alternative-text="Loading..."></lightning-spinner>
            </template>
            <template if:false={isTraversing}>
                <template if:true={traversalResult.length > 0}>
                    <p>Result: {traversalResult}</p>
                </template>
                <template if:false={traversalResult.length > 0}>
                    <p>No traversal performed yet</p>
                </template>
            </template>
        </div>
    </div>
</template>
```

```css
/* morrisTraversal.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.tree-info {
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 8px;
    margin-bottom: 20px;
    border: 1px solid #e9ecef;
}

.controls {
    margin: 20px 0;
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
}

.result {
    background-color: #e8f5e9;
    padding: 15px;
    border-radius: 8px;
    margin-top: 20px;
}

.result p {
    font-weight: bold;
    margin: 0;
    color: #155724;
}
```

## How Morris Traversal Works

The Morris traversal algorithm is a tree traversal technique that allows you to traverse a binary tree without using recursion or a stack. It works by temporarily modifying the tree structure to create links back to previously visited nodes.

### Key Features:
1. **Space Complexity**: O(1) - No extra space needed for stack or recursion
2. **Time Complexity**: O(n) - Each node visited twice in worst case
3. **No Additional Data Structures**: Uses existing tree links

### Algorithm Steps:
1. Start with the root node
2. If current node has no left child, visit it and move to right child
3. If current node has left child:
   - Find the inorder predecessor (rightmost node in left subtree)
   - If predecessor's right is null, make current as its right child and move to left
   - If predecessor's right is current, revert changes and move to right

### Use Cases:
- Memory-constrained environments
- When stack space is limited
- Tree traversal in embedded systems
- Educational purposes to understand tree traversal concepts

This implementation demonstrates both inorder and preorder Morris traversals in a Lightning Web Component, showing how the algorithm works without using additional memory for a stack or recursion.

