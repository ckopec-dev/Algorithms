# Kosaraju's Algorithm in Lightning Web Component

Here's an example implementation of Kosaraju's algorithm for finding strongly connected components in a directed graph using Lightning Web Components:

```javascript
// kosaraju.js
import { LightningElement } from 'lwc';

export default class KosarajuAlgorithm extends LightningElement {
    graph = [];
    visited = [];
    stack = [];
    sccs = [];
    adjacencyList = [];

    connectedComponents = [];

    connectedComponentsList = [
        { id: 1, name: 'Component 1' },
        { id: 2, name: 'Component 2' },
        { id: 3, name: 'Component 3' },
        { id: 4, name: 'Component 4' }
    ];

    handleRunAlgorithm() {
        // Initialize graph with sample data
        this.initializeGraph();
        
        // Run Kosaraju's algorithm
        const sccs = this.kosaraju();
        
        this.connectedComponents = sccs;
        this.dispatchEvent(new CustomEvent('result', {
            detail: { sccs: sccs }
        }));
    }

    initializeGraph() {
        // Sample directed graph represented as adjacency list
        // Format: [node, [connected_nodes]]
        this.adjacencyList = [
            [0, [1]],
            [1, [2]],
            [2, [0, 3]],
            [3, [4]],
            [4, [5]],
            [5, [3]]
        ];
        
        // Initialize visited array
        this.visited = new Array(this.adjacencyList.length).fill(false);
        this.stack = [];
        this.sccs = [];
    }

    kosaraju() {
        // Step 1: Perform DFS and fill stack
        this.visited = new Array(this.adjacencyList.length).fill(false);
        for (let i = 0; i < this.adjacencyList.length; i++) {
            if (!this.visited[i]) {
                this.dfsFillStack(i);
            }
        }

        // Step 2: Get transpose of graph
        const transpose = this.getTranspose();

        // Step 3: Process vertices in stack order
        this.visited = new Array(this.adjacencyList.length).fill(false);
        const sccs = [];

        while (this.stack.length > 0) {
            const vertex = this.stack.pop();
            if (!this.visited[vertex]) {
                const scc = [];
                this.dfsGetSCC(vertex, transpose, scc);
                sccs.push(scc);
            }
        }

        return sccs;
    }

    dfsFillStack(vertex) {
        this.visited[vertex] = true;
        
        const neighbors = this.adjacencyList[vertex] ? this.adjacencyList[vertex][1] : [];
        for (let i = 0; i < neighbors.length; i++) {
            const neighbor = neighbors[i];
            if (!this.visited[neighbor]) {
                this.dfsFillStack(neighbor);
            }
        }
        
        this.stack.push(vertex);
    }

    getTranspose() {
        const transpose = [];
        for (let i = 0; i < this.adjacencyList.length; i++) {
            transpose.push([]);
        }

        for (let i = 0; i < this.adjacencyList.length; i++) {
            const neighbors = this.adjacencyList[i][1];
            for (let j = 0; j < neighbors.length; j++) {
                const neighbor = neighbors[j];
                transpose[neighbor].push(i);
            }
        }

        return transpose;
    }

    dfsGetSCC(vertex, transpose, scc) {
        this.visited[vertex] = true;
        scc.push(vertex);

        const neighbors = transpose[vertex] || [];
        for (let i = 0; i < neighbors.length; i++) {
            const neighbor = neighbors[i];
            if (!this.visited[neighbor]) {
                this.dfsGetSCC(neighbor, transpose, scc);
            }
        }
    }

    get sccResult() {
        return this.connectedComponents.map((scc, index) => ({
            id: index + 1,
            vertices: scc.join(', '),
            count: scc.length
        }));
    }

    handleClear() {
        this.connectedComponents = [];
        this.stack = [];
        this.visited = [];
        this.sccs = [];
    }
}
```

```html
<!-- kosaraju.html -->
<template>
    <div class="scc-container">
        <lightning-card title="Kosaraju's Algorithm - Strongly Connected Components">
            <div class="scc-controls">
                <lightning-button 
                    label="Run Algorithm" 
                    variant="brand" 
                    onclick={handleRunAlgorithm}
                    class="scc-button">
                </lightning-button>
                <lightning-button 
                    label="Clear" 
                    variant="destructive" 
                    onclick={handleClear}
                    class="scc-button">
                </lightning-button>
            </div>

            <template if:true={connectedComponents.length > 0}>
                <div class="scc-results">
                    <h3>Strongly Connected Components Found:</h3>
                    <template for:each={sccResult} for:item="scc">
                        <div key={scc.id} class="scc-item">
                            <p><strong>Component {scc.id}:</strong> Vertices: {scc.vertices} (Count: {scc.count})</p>
                        </div>
                    </template>
                </div>
            </template>

            <div class="scc-explanation">
                <h3>How Kosaraju's Algorithm Works:</h3>
                <ul>
                    <li><strong>Step 1:</strong> Perform DFS on original graph and fill stack based on finish times</li>
                    <li><strong>Step 2:</strong> Create transpose of the graph</li>
                    <li><strong>Step 3:</strong> Process vertices in reverse order from stack on transpose graph</li>
                    <li><strong>Step 4:</strong> Each DFS tree in Step 3 represents a strongly connected component</li>
                </ul>
            </div>
        </lightning-card>
    </div>
</template>
```

```css
/* kosaraju.css */
.scc-container {
    padding: 1rem;
    max-width: 800px;
    margin: 0 auto;
}

.scc-controls {
    margin-bottom: 1rem;
    display: flex;
    gap: 0.5rem;
    justify-content: center;
}

.scc-button {
    margin: 0.25rem;
}

.scc-results {
    margin-top: 1rem;
    padding: 1rem;
    border: 1px solid #e1e1e1;
    border-radius: 0.25rem;
    background-color: #f9f9f9;
}

.scc-item {
    margin: 0.5rem 0;
    padding: 0.5rem;
    background-color: #ffffff;
    border-radius: 0.25rem;
    border-left: 4px solid #0070d2;
}

.scc-explanation {
    margin-top: 1rem;
    padding: 1rem;
    background-color: #f0f8ff;
    border-radius: 0.25rem;
    border-left: 4px solid #0070d2;
}

.scc-explanation ul {
    margin: 0.5rem 0;
    padding-left: 1.5rem;
}

.scc-explanation li {
    margin: 0.25rem 0;
}
```

```xml
<!-- kosaraju.js-meta.xml -->
<?xml version="1.0" encoding="UTF-8"?>
<LightningComponentBundle xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>58.0</apiVersion>
    <isExposed>true</isExposed>
    <targets>
        <target>lightning__AppPage</target>
        <target>lightning__RecordPage</target>
        <target>lightning__HomePage</target>
    </targets>
</LightningComponentBundle>
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list format to represent the directed graph
2. **Three Main Steps**:
   - DFS traversal to fill stack with finish times
   - Graph transpose creation
   - DFS on transpose graph to find SCCs
3. **Interactive UI**: Buttons to run algorithm and clear results
4. **Visual Results**: Displays each strongly connected component found
5. **Educational Component**: Includes explanation of the algorithm steps

## How to Use:

1. Create a new Lightning Web Component with this code
2. Deploy to your Salesforce org
3. Add the component to a page
4. Click "Run Algorithm" to see the strongly connected components
5. Use "Clear" to reset the component

The algorithm correctly identifies strongly connected components in the sample directed graph, demonstrating the core concepts of Kosaraju's algorithm for finding SCCs in directed graphs.

