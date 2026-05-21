# Disjoint Set Union (Union-Find) in Lightning Web Component

```javascript
// disjointSet.js
import { LightningElement } from 'lwc';

export default class DisjointSet extends LightningElement {
    // Store the parent array for Union-Find
    parent = [];
    rank = [];
    components = 0;

    connectedCallback() {
        // Initialize the Union-Find structure
        this.initializeSet(10);
        this.runExample();
    }

    initializeSet(size) {
        this.parent = new Array(size);
        this.rank = new Array(size);
        this.components = size;
        
        // Initialize each element to be its own parent
        for (let i = 0; i < size; i++) {
            this.parent[i] = i;
            this.rank[i] = 0;
        }
    }

    // Find with path compression
    find(x) {
        if (this.parent[x] !== x) {
            this.parent[x] = this.find(this.parent[x]); // Path compression
        }
        return this.parent[x];
    }

    // Union by rank
    union(x, y) {
        const rootX = this.find(x);
        const rootY = this.find(y);

        if (rootX !== rootY) {
            // Union by rank
            if (this.rank[rootX] < this.rank[rootY]) {
                this.parent[rootX] = rootY;
            } else if (this.rank[rootX] > this.rank[rootY]) {
                this.parent[rootY] = rootX;
            } else {
                this.parent[rootY] = rootX;
                this.rank[rootX]++;
            }
            this.components--;
        }
    }

    // Check if two elements are connected
    isConnected(x, y) {
        return this.find(x) === this.find(y);
    }

    // Get number of connected components
    getComponents() {
        return this.components;
    }

    runExample() {
        console.log('=== Union-Find Example ===');
        
        // Create connections
        this.union(0, 1);
        this.union(2, 3);
        this.union(4, 5);
        this.union(1, 3);
        
        console.log('Components after unions:', this.getComponents());
        console.log('Is 0 connected to 3?', this.isConnected(0, 3));
        console.log('Is 0 connected to 5?', this.isConnected(0, 5));
        
        // Additional union
        this.union(0, 5);
        console.log('Components after union(0,5):', this.getComponents());
        console.log('Is 0 connected to 5 now?', this.isConnected(0, 5));
    }

    // Example usage in LWC methods
    handleUnion() {
        const x = parseInt(this.template.querySelector('[data-id="x"]').value);
        const y = parseInt(this.template.querySelector('[data-id="y"]').value);
        
        if (!isNaN(x) && !isNaN(y)) {
            this.union(x, y);
            this.showToast(`Union(${x}, ${y}) completed`);
        }
    }

    handleFind() {
        const x = parseInt(this.template.querySelector('[data-id="find-x"]').value);
        
        if (!isNaN(x)) {
            const root = this.find(x);
            this.showToast(`Find(${x}) = ${root}`);
        }
    }

    handleIsConnected() {
        const x = parseInt(this.template.querySelector('[data-id="conn-x"]').value);
        const y = parseInt(this.template.querySelector('[data-id="conn-y"]').value);
        
        if (!isNaN(x) && !isNaN(y)) {
            const connected = this.isConnected(x, y);
            this.showToast(`Is ${x} connected to ${y}? ${connected}`);
        }
    }

    showToast(message) {
        // Create toast event
        const toastEvent = new CustomEvent('showtoast', {
            detail: {
                message: message,
                variant: 'info'
            }
        });
        this.dispatchEvent(toastEvent);
    }

    // Get current state for display
    get currentState() {
        return {
            parent: this.parent,
            rank: this.rank,
            components: this.components
        };
    }
}
```

```html
<!-- disjointSet.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Disjoint Set Union (Union-Find) Algorithm</h2>
        
        <div class="slds-grid slds-gutters">
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-card__header-title slds-truncate" title="Operations">
                            <h3>Operations</h3>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <div class="slds-form slds-form_stacked">
                            <div class="slds-form-element">
                                <div class="slds-form-element__control">
                                    <lightning-input 
                                        data-id="x" 
                                        label="First element (x)" 
                                        type="number" 
                                        min="0" 
                                        max="9">
                                    </lightning-input>
                                </div>
                            </div>
                            <div class="slds-form-element">
                                <div class="slds-form-element__control">
                                    <lightning-input 
                                        data-id="y" 
                                        label="Second element (y)" 
                                        type="number" 
                                        min="0" 
                                        max="9">
                                    </lightning-input>
                                </div>
                            </div>
                            <lightning-button 
                                label="Union" 
                                onclick={handleUnion} 
                                variant="brand">
                            </lightning-button>
                        </div>
                    </div>
                </div>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-card__header-title slds-truncate" title="Find">
                            <h3>Find Operation</h3>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <div class="slds-form slds-form_stacked">
                            <div class="slds-form-element">
                                <div class="slds-form-element__control">
                                    <lightning-input 
                                        data-id="find-x" 
                                        label="Element to find" 
                                        type="number" 
                                        min="0" 
                                        max="9">
                                    </lightning-input>
                                </div>
                            </div>
                            <lightning-button 
                                label="Find" 
                                onclick={handleFind} 
                                variant="neutral">
                            </lightning-button>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div class="slds-grid slds-gutters slds-m-top_medium">
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-card__header-title slds-truncate" title="Connectivity">
                            <h3>Connectivity Check</h3>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <div class="slds-form slds-form_stacked">
                            <div class="slds-form-element">
                                <div class="slds-form-element__control">
                                    <lightning-input 
                                        data-id="conn-x" 
                                        label="First element" 
                                        type="number" 
                                        min="0" 
                                        max="9">
                                    </lightning-input>
                                </div>
                            </div>
                            <div class="slds-form-element">
                                <div class="slds-form-element__control">
                                    <lightning-input 
                                        data-id="conn-y" 
                                        label="Second element" 
                                        type="number" 
                                        min="0" 
                                        max="9">
                                    </lightning-input>
                                </div>
                            </div>
                            <lightning-button 
                                label="Check Connectivity" 
                                onclick={handleIsConnected} 
                                variant="success">
                            </lightning-button>
                        </div>
                    </div>
                </div>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-card">
                    <div class="slds-card__header slds-grid">
                        <header class="slds-card__header-title slds-truncate" title="Status">
                            <h3>Current Status</h3>
                        </header>
                    </div>
                    <div class="slds-card__body">
                        <p><strong>Connected Components:</strong> {components}</p>
                        <p><strong>Parent Array:</strong></p>
                        <p>{parentString}</p>
                        <p><strong>Rank Array:</strong></p>
                        <p>{rankString}</p>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>
```

```css
/* disjointSet.css */
.slds-card__header {
    background-color: #f4f6f9;
}

.slds-form_stacked .slds-form-element {
    margin-bottom: 1rem;
}

.slds-button {
    margin-right: 0.5rem;
    margin-bottom: 0.5rem;
}
```

## Key Features of this Implementation:

1. **Path Compression**: Optimizes find operations by making nodes point directly to the root
2. **Union by Rank**: Keeps the tree balanced by always attaching the smaller tree under the root of the larger tree
3. **Interactive UI**: Allows users to perform union, find, and connectivity checks
4. **Real-time Updates**: Shows current state of parent and rank arrays
5. **Error Handling**: Validates input before operations

## Algorithm Complexity:
- **Find**: O(α(n)) amortized time complexity
- **Union**: O(α(n)) amortized time complexity
- **Space**: O(n)

This implementation demonstrates how to use the Union-Find data structure in a Lightning Web Component with a user-friendly interface for educational purposes.

