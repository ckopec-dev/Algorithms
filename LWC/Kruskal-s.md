# Kruskal's Algorithm in Lightning Web Component

Here's a complete example of implementing Kruskal's algorithm in a Lightning Web Component:

## HTML Template (kruskalExample.html)

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Kruskal's Algorithm Example</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Graph Input">
                    <div class="slds-p-around_small">
                        <lightning-input 
                            label="Number of Vertices" 
                            type="number" 
                            value={vertexCount} 
                            onchange={handleVertexCountChange}
                            min="1">
                        </lightning-input>
                        
                        <lightning-button 
                            label="Generate Random Graph" 
                            onclick={generateRandomGraph}
                            class="slds-m-top_small">
                        </lightning-button>
                        
                        <div class="slds-m-top_small">
                            <lightning-button 
                                label="Run Kruskal's Algorithm" 
                                onclick={runKruskal}
                                disabled={isRunning}>
                            </lightning-button>
                        </div>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Results">
                    <div class="slds-p-around_small">
                        <p><strong>Total Weight:</strong> {totalWeight}</p>
                        <p><strong>Edges in MST:</strong> {mstEdges.length}</p>
                        <lightning-button 
                            label="Clear Results" 
                            onclick={clearResults}
                            class="slds-m-top_small">
                        </lightning-button>
                    </div>
                </lightning-card>
            </div>
        </div>
        
        <div class="slds-m-top_small">
            <lightning-card title="Edge List">
                <div class="slds-p-around_small">
                    <lightning-datatable
                        data={edgeData}
                        columns={edgeColumns}
                        key-field="id"
                        hide-checkbox-column="true">
                    </lightning-datatable>
                </div>
            </lightning-card>
        </div>
        
        <div class="slds-m-top_small">
            <lightning-card title="Minimum Spanning Tree">
                <div class="slds-p-around_small">
                    <lightning-datatable
                        data={mstData}
                        columns={mstColumns}
                        key-field="id"
                        hide-checkbox-column="true">
                    </lightning-datatable>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (kruskalExample.js)

```javascript
import { LightningElement, track } from 'lwc';

export default class KruskalExample extends LightningElement {
    @track vertexCount = 6;
    @track edges = [];
    @track mstEdges = [];
    @track totalWeight = 0;
    @track isRunning = false;
    
    edgeColumns = [
        { label: 'From', fieldName: 'from', type: 'number' },
        { label: 'To', fieldName: 'to', type: 'number' },
        { label: 'Weight', fieldName: 'weight', type: 'number' }
    ];
    
    mstColumns = [
        { label: 'From', fieldName: 'from', type: 'number' },
        { label: 'To', fieldName: 'to', type: 'number' },
        { label: 'Weight', fieldName: 'weight', type: 'number' }
    ];
    
    get edgeData() {
        return this.edges.map((edge, index) => ({
            id: index,
            from: edge.from,
            to: edge.to,
            weight: edge.weight
        }));
    }
    
    get mstData() {
        return this.mstEdges.map((edge, index) => ({
            id: index,
            from: edge.from,
            to: edge.to,
            weight: edge.weight
        }));
    }
    
    handleVertexCountChange(event) {
        this.vertexCount = parseInt(event.target.value);
    }
    
    generateRandomGraph() {
        this.edges = [];
        const maxEdges = (this.vertexCount * (this.vertexCount - 1)) / 2;
        
        // Generate random edges
        for (let i = 0; i < this.vertexCount; i++) {
            for (let j = i + 1; j < this.vertexCount; j++) {
                // Random weight between 1 and 20
                const weight = Math.floor(Math.random() * 20) + 1;
                this.edges.push({
                    from: i,
                    to: j,
                    weight: weight
                });
            }
        }
        
        // Sort edges by weight
        this.edges.sort((a, b) => a.weight - b.weight);
    }
    
    runKruskal() {
        this.isRunning = true;
        this.mstEdges = [];
        this.totalWeight = 0;
        
        // Create Union-Find structure
        const parent = new Array(this.vertexCount);
        const rank = new Array(this.vertexCount);
        
        // Initialize parent and rank arrays
        for (let i = 0; i < this.vertexCount; i++) {
            parent[i] = i;
            rank[i] = 0;
        }
        
        // Find function for Union-Find
        const find = (x) => {
            if (parent[x] !== x) {
                parent[x] = find(parent[x]); // Path compression
            }
            return parent[x];
        };
        
        // Union function for Union-Find
        const union = (x, y) => {
            const rootX = find(x);
            const rootY = find(y);
            
            if (rootX !== rootY) {
                // Union by rank
                if (rank[rootX] < rank[rootY]) {
                    parent[rootX] = rootY;
                } else if (rank[rootX] > rank[rootY]) {
                    parent[rootY] = rootX;
                } else {
                    parent[rootY] = rootX;
                    rank[rootX]++;
                }
                return true;
            }
            return false;
        };
        
        // Kruskal's algorithm
        const mst = [];
        let totalWeight = 0;
        
        for (const edge of this.edges) {
            if (union(edge.from, edge.to)) {
                mst.push(edge);
                totalWeight += edge.weight;
            }
            
            // Stop when we have V-1 edges
            if (mst.length === this.vertexCount - 1) {
                break;
            }
        }
        
        this.mstEdges = mst;
        this.totalWeight = totalWeight;
        this.isRunning = false;
    }
    
    clearResults() {
        this.mstEdges = [];
        this.totalWeight = 0;
    }
}
```

## CSS Styles (kruskalExample.css)

```css
.slds-box {
    background-color: #f4f6f9;
}

.slds-card__header {
    background-color: #e6f2ff;
}

.slds-button {
    margin-right: 0.5rem;
}
```

## Key Features of This Implementation:

1. **Graph Input**: Users can specify the number of vertices and generate a random graph
2. **Union-Find Data Structure**: Implements path compression and union by rank for efficient operations
3. **Kruskal's Algorithm**: 
   - Sorts edges by weight
   - Uses Union-Find to detect cycles
   - Builds minimum spanning tree
4. **Visual Feedback**: Shows both original edges and MST results
5. **Interactive**: Users can generate new graphs and run the algorithm multiple times

## How Kruskal's Algorithm Works in This Component:

1. **Initialization**: Creates Union-Find structure for tracking connected components
2. **Edge Sorting**: Sorts all edges by weight in ascending order
3. **Edge Processing**: For each edge, checks if it connects two different components
4. **Union Operation**: If components are different, unions them and adds edge to MST
5. **Termination**: Stops when MST has V-1 edges

This implementation demonstrates the core concepts of Kruskal's algorithm in a user-friendly Lightning Web Component interface.

