# Minimum Spanning Tree using Kruskal's Algorithm in Lightning Web Component

Here's a complete example of implementing a Minimum Spanning Tree algorithm using Kruskal's algorithm in a Lightning Web Component:

```javascript
// mst.js
import { LightningElement, track } from 'lwc';

export default class Mst extends LightningElement {
    @track edges = [
        { id: '1', from: 'A', to: 'B', weight: 4 },
        { id: '2', from: 'A', to: 'H', weight: 8 },
        { id: '3', from: 'B', to: 'C', weight: 8 },
        { id: '4', from: 'B', to: 'H', weight: 11 },
        { id: '5', from: 'C', to: 'D', weight: 7 },
        { id: '6', from: 'C', to: 'F', weight: 4 },
        { id: '7', from: 'C', to: 'I', weight: 2 },
        { id: '8', from: 'D', to: 'E', weight: 9 },
        { id: '9', from: 'D', to: 'F', weight: 14 },
        { id: '10', from: 'E', to: 'F', weight: 10 },
        { id: '11', from: 'F', to: 'G', weight: 2 },
        { id: '12', from: 'G', to: 'H', weight: 1 },
        { id: '13', from: 'G', to: 'I', weight: 6 },
        { id: '14', from: 'H', to: 'I', weight: 7 }
    ];

    @track vertices = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'];
    @track mstEdges = [];
    @track totalWeight = 0;
    @track isRunning = false;

    // Union-Find data structure for Kruskal's algorithm
    unionFind = {
        parent: {},
        rank: {},
        
        makeSet: function(vertex) {
            this.parent[vertex] = vertex;
            this.rank[vertex] = 0;
        },
        
        find: function(vertex) {
            if (this.parent[vertex] !== vertex) {
                this.parent[vertex] = this.find(this.parent[vertex]); // Path compression
            }
            return this.parent[vertex];
        },
        
        union: function(vertexA, vertexB) {
            let rootA = this.find(vertexA);
            let rootB = this.find(vertexB);
            
            if (rootA !== rootB) {
                // Union by rank
                if (this.rank[rootA] < this.rank[rootB]) {
                    this.parent[rootA] = rootB;
                } else if (this.rank[rootA] > this.rank[rootB]) {
                    this.parent[rootB] = rootA;
                } else {
                    this.parent[rootB] = rootA;
                    this.rank[rootA]++;
                }
                return true;
            }
            return false;
        }
    };

    connectedCallback() {
        // Initialize Union-Find structure
        this.vertices.forEach(vertex => {
            this.unionFind.makeSet(vertex);
        });
    }

    handleRunMST() {
        this.isRunning = true;
        this.mstEdges = [];
        this.totalWeight = 0;

        // Reset Union-Find
        this.vertices.forEach(vertex => {
            this.unionFind.parent[vertex] = vertex;
            this.unionFind.rank[vertex] = 0;
        });

        // Sort edges by weight
        const sortedEdges = [...this.edges].sort((a, b) => a.weight - b.weight);

        // Apply Kruskal's algorithm
        let edgeCount = 0;
        let i = 0;

        while (edgeCount < this.vertices.length - 1 && i < sortedEdges.length) {
            const edge = sortedEdges[i];
            const { from, to } = edge;

            // Check if adding this edge creates a cycle
            if (this.unionFind.union(from, to)) {
                this.mstEdges.push(edge);
                this.totalWeight += edge.weight;
                edgeCount++;
            }
            i++;
        }

        this.isRunning = false;
    }

    handleReset() {
        this.mstEdges = [];
        this.totalWeight = 0;
        this.isRunning = false;
    }

    get mstEdgesFormatted() {
        return this.mstEdges.map(edge => ({
            ...edge,
            label: `${edge.from} -- ${edge.to} (weight: ${edge.weight})`
        }));
    }

    get isMSTReady() {
        return this.mstEdges.length > 0;
    }
}
```

```html
<!-- mst.html -->
<template>
    <div class="slds-box slds-box_x-small slds-m-around_medium">
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-1 slds-medium-size_1-of-2">
                <h2 class="slds-text-heading_small">Minimum Spanning Tree</h2>
                <p>This example demonstrates Kruskal's algorithm for finding the minimum spanning tree.</p>
                
                <lightning-button 
                    label="Run MST Algorithm" 
                    variant="brand" 
                    onclick={handleRunMST}
                    disabled={isRunning}
                    class="slds-m-bottom_small">
                </lightning-button>
                
                <lightning-button 
                    label="Reset" 
                    variant="neutral" 
                    onclick={handleReset}
                    class="slds-m-bottom_small">
                </lightning-button>

                <template if:true={isMSTReady}>
                    <div class="slds-m-top_small">
                        <h3 class="slds-text-heading_small">Result</h3>
                        <p>Total Weight: {totalWeight}</p>
                        <p>Number of edges in MST: {mstEdges.length}</p>
                    </div>
                </template>
            </div>

            <div class="slds-col slds-size_1-of-1 slds-medium-size_1-of-2">
                <h3 class="slds-text-heading_small">Graph Edges</h3>
                <lightning-datatable
                    data={edges}
                    columns={columns}
                    key-field="id"
                    hide-checkbox-column
                    class="slds-m-bottom_small">
                </lightning-datatable>
            </div>
        </div>

        <template if:true={isMSTReady}>
            <div class="slds-m-top_small">
                <h3 class="slds-text-heading_small">Minimum Spanning Tree Edges</h3>
                <lightning-datatable
                    data={mstEdgesFormatted}
                    columns={mstColumns}
                    key-field="id"
                    hide-checkbox-column>
                </lightning-datatable>
            </div>
        </template>
    </div>
</template>
```

```javascript
// mst.css
.slds-box {
    background-color: #f4f6f9;
    border: 1px solid #d8dde6;
}

.slds-text-heading_small {
    font-size: 1rem;
    font-weight: 600;
    line-height: 1.25;
    color: #080707;
}
```

## Key Features of this Implementation:

1. **Kruskal's Algorithm**: Uses Union-Find data structure with path compression and union by rank for optimal performance
2. **Visual Interface**: Shows both the original graph and the resulting minimum spanning tree
3. **Interactive Controls**: Run and Reset buttons for algorithm execution
4. **Real-time Results**: Displays total weight and number of edges in the MST
5. **Responsive Design**: Uses Lightning Design System components for consistent styling

## How the Algorithm Works:

1. **Sort Edges**: All edges are sorted by weight in ascending order
2. **Initialize Union-Find**: Each vertex starts as its own parent
3. **Process Edges**: For each edge in sorted order:
   - Check if vertices are in different sets using Union-Find
   - If they are, add the edge to MST and union the sets
   - Continue until we have V-1 edges (where V is number of vertices)
4. **Cycle Detection**: Union-Find efficiently detects cycles without building the actual graph

This implementation demonstrates a practical application of graph algorithms in the Salesforce Lightning platform, showing how complex algorithms can be implemented in LWC components.

